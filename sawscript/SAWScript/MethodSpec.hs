{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
module SAWScript.MethodSpec
  ( VerifyCommand
  , ValidationPlan(..)
  , MethodSpecIR
  , specName
  , specMethodClass
  , specValidationPlan
  , resolveMethodSpecIR
  , validateMethodSpec
  , VerifyParams(..)
  ) where

-- Imports {{{1

import Control.Applicative hiding (empty)
import Control.Exception (finally)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error (Error(..), ErrorT, runErrorT, throwError)
import Control.Monad.State
import Data.Int
import Data.List (foldl', intercalate, intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Storable as SV
import qualified Data.Vector as V
import Text.PrettyPrint.HughesPJ
import System.Directory(doesFileExist)

import qualified Execution.Codebase as JSS
import qualified JavaParser as JSS
import qualified SAWScript.CongruenceClosure as CC
import qualified SAWScript.SmtLibTrans as SmtLib
import qualified SAWScript.SmtLibTrans2 as SmtLib2
import qualified SAWScript.QuickCheck as QuickCheck
import qualified SAWScript.Yices  as Yices
import qualified SAWScript.TypeChecker as TC
import qualified Simulation as JSS
import SAWScript.Utils
import SAWScript.MethodSpecIR
import SAWScript.TypeChecker
import Utils.Common

import Verinf.Symbolic
import Verinf.Symbolic.Lit.Functional
import Verinf.Utils.LogMonad

import qualified SMTLib1 as SmtLib
import qualified SMTLib2 as SmtLib2

-- Utilities {{{1

nyi msg = error $ "Not yet implemented: " ++ msg

-- | Return first value satisfying predicate if any.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM check (x:xs)  = do ok <- check x
                         if ok then return (Just x)
                               else findM check xs

-- Verinf Utilities {{{1

-- | Create a lit result with input bits for the given ground dag type.
mkInputLitResultWithType :: (?be :: BitEngine l, SV.Storable l)
                         => DagType -> IO (LitResult l)
mkInputLitResultWithType (SymInt (widthConstant -> Just (Wx w))) =
  LV <$> SV.replicateM w lMkInput
mkInputLitResultWithType (SymArray (widthConstant -> Just (Wx l)) eltTp) =
  LVN <$> V.replicateM l (mkInputLitResultWithType eltTp)
mkInputLitResultWithType _ =
  error "internal: mkInputLitResultWithType called with unsupported type."

deEq :: DagEngine -> DagTerm -> DagTerm -> IO DagTerm
deEq de x y = deApplyBinary de (eqOp (termType x)) x y

deImplies :: DagEngine -> DagTerm -> DagTerm -> IO DagTerm
deImplies de x y = deApplyBinary de bImpliesOp x y

typeBitCount :: DagType -> Int
typeBitCount SymBool = 1
typeBitCount (SymInt (widthConstant -> Just (Wx w))) = w
typeBitCount (SymArray (widthConstant -> Just (Wx l)) e) = l * typeBitCount e
typeBitCount (SymRec d s) = V.sum $ V.map typeBitCount $ recFieldTypes d s
typeBitCount _ = error "internal: typeBitCount called on polymorphic type."

mkInputEval :: DagType -> SV.Vector Bool -> CValue
mkInputEval (SymInt (widthConstant -> Just (Wx _))) lits = mkCIntFromLsbfV lits
mkInputEval (SymArray (widthConstant -> Just (Wx l)) e) lits =
  let w = typeBitCount e
   in CArray $ V.generate l $ (\i -> mkCIntFromLsbfV $ SV.slice (i*w) w lits)
mkInputEval _ _ = error "internal: Unsupported type given to mkInputEval"

-- JSS Utilities {{{1

type DagPathState = JSS.PathState DagTerm
type DagJavaValue = JSS.Value DagTerm

-- | Set value of bound to instance field in path state.
setInstanceFieldValue :: JSS.Ref -> JSS.FieldId -> JSS.Value n
                      -> JSS.PathState n -> JSS.PathState n
setInstanceFieldValue r f v ps =
  ps { JSS.instanceFields = Map.insert (r,f) v (JSS.instanceFields ps) }

-- | Set value bound to array in path state.
-- Assumes value is an array with a ground length.
setArrayValue :: TypedTerm n => JSS.Ref -> n -> JSS.PathState n
              -> JSS.PathState n
setArrayValue r v ps = do
  let SymArray (widthConstant -> Just (Wx w)) _ = termType v
   in ps { JSS.arrays = Map.insert r (fromIntegral w, v) (JSS.arrays ps) }

-- | Returns value constructor from node.
mkJSSValue :: JSS.Type -> n -> JSS.Value n
mkJSSValue JSS.BooleanType n = JSS.IValue n
mkJSSValue JSS.ByteType    n = JSS.IValue n
mkJSSValue JSS.CharType    n = JSS.IValue n
mkJSSValue JSS.IntType     n = JSS.IValue n
mkJSSValue JSS.LongType    n = JSS.LValue n
mkJSSValue JSS.ShortType   n = JSS.IValue n
mkJSSValue _ _ = error "internal: illegal type"

-- | Add assumption for predicate to path state.
addAssumption :: DagEngine -> DagTerm -> DagPathState -> IO DagPathState
addAssumption de x ps =
  do prop <- deApplyBinary de bAndOp (JSS.psAssumptions ps) x
     return ps { JSS.psAssumptions = prop }

-- | Add assertion for predicate to path state.
addAssertion :: String -> n -> JSS.PathState n -> JSS.PathState n
addAssertion nm x ps =
  ps { JSS.psAssertions = (nm,x) : JSS.psAssertions ps }

-- EvalContext {{{1

-- | Contextual information needed to evaluate expressions.
data EvalContext = EvalContext {
         ecDagEngine :: DagEngine
       , ecLocals :: Map JSS.LocalVariableIndex DagJavaValue
       , ecPathState :: DagPathState
       }

evalContextFromPathState :: DagEngine -> DagPathState -> EvalContext
evalContextFromPathState de ps =
  let f:_ = JSS.frames ps
      localMap = JSS.frmLocals f
   in EvalContext {
          ecDagEngine = de
        , ecLocals = localMap
        , ecPathState = ps
        }

type ExprEvaluator a = ErrorT TC.JavaExpr IO a

instance Error TC.JavaExpr where
  noMsg = error "noMsg called with TC.JavaExpr"

runEval :: MonadIO m => ExprEvaluator b -> m (Either TC.JavaExpr b)
runEval v = liftIO (runErrorT v)

-- or undefined subexpression if not.
-- N.B. This method assumes that the Java path state is well-formed, the
-- the JavaExpression syntactically referes to the correct type of method
-- (static versus instance), and correct well-typed arguments.  It does
-- not assume that all the instanceFields in the JavaEvalContext are initialized.
evalJavaExpr :: TC.JavaExpr -> EvalContext -> ExprEvaluator DagJavaValue
evalJavaExpr expr ec = eval expr
  where eval e@(CC.Term app) =
          case app of
            TC.Local _ idx _ ->
              case Map.lookup idx (ecLocals ec) of
                Just v -> return v
                Nothing -> throwError e
            TC.InstanceField r f -> do
              JSS.RValue ref <- eval r
              case Map.lookup (ref, f) (JSS.instanceFields (ecPathState ec)) of
                Just v -> return v
                Nothing -> throwError e

evalJavaRefExpr :: TC.JavaExpr -> EvalContext -> ExprEvaluator JSS.Ref
evalJavaRefExpr expr ec = do
  val <- evalJavaExpr expr ec
  case val of
    JSS.RValue ref -> return ref
    _ -> error "internal: evalJavaRefExpr encountered illegal value."

evalJavaExprAsLogic :: TC.JavaExpr -> EvalContext -> ExprEvaluator DagTerm
evalJavaExprAsLogic expr ec = do
  val <- evalJavaExpr expr ec
  case val of
    JSS.RValue r ->
      case Map.lookup r (JSS.arrays (ecPathState ec)) of
        Nothing    -> throwError expr
        Just (_,n) -> return n
    JSS.IValue n -> return n
    JSS.LValue n -> return n
    _ -> error "internal: evalJavaExprAsExpr encountered illegal value."

-- | Evaluates a typed expression.
evalLogicExpr :: TC.LogicExpr -> EvalContext -> ExprEvaluator DagTerm
evalLogicExpr initExpr ec = eval initExpr
  where de = ecDagEngine ec
        eval (TC.Apply op exprs) =
          (liftIO . deApplyOp de op) =<< V.mapM eval (V.fromList exprs)
        eval (TC.Cns c tp) = return $ ConstTerm c tp
        eval (TC.IntLit i we@(widthConstant -> Just w)) =
          return $ ConstTerm (mkCInt w i) (SymInt we)
        eval TC.IntLit{} = error "internal: evalLogicExpr called with non-ground literal."
        eval (TC.Var _ _) = error "internal: evalLogicExpr called with var"
        eval (TC.JavaValue expr _ _) = evalJavaExprAsLogic expr ec

-- | Return Java value associated with mixed expression.
evalMixedExpr :: TC.MixedExpr -> EvalContext 
              -> ExprEvaluator DagJavaValue
evalMixedExpr (LE expr) ec = do
  n <- evalLogicExpr expr ec
  case termType n of
    SymInt (widthConstant -> Just 32) -> return (JSS.IValue n)
    SymInt (widthConstant -> Just 64) -> return (JSS.LValue n)
    _ -> error "internal: mixedExprValue called with malformed result type."
evalMixedExpr (JE expr) ec = evalJavaExpr expr ec

-- Method specification overrides {{{1
-- OverrideComputation definition {{{2

-- | State for running the behavior specifications in a method override.
data OCState = OCState {
         ocsPC :: JSS.PC
       , ocsEvalContext :: !EvalContext
       , ocsResultState :: !DagPathState
       , ocsReturnValue :: !(Maybe DagJavaValue)
       , ocsErrors :: [OverrideError]
       }

data OverrideError
   = UndefinedExpr TC.JavaExpr
   | FalseAssertion Pos
   | AliasingInputs !TC.JavaExpr !TC.JavaExpr
   | JavaException JSS.Ref
   | SimException String
   | Abort
   deriving (Show)

ppOverrideError :: OverrideError -> String
ppOverrideError (UndefinedExpr expr) =
  "Could not evaluate " ++ show (ppJavaExpr expr) ++ "."
ppOverrideError (FalseAssertion p)   = "Assertion at " ++ show p ++ " is false."
ppOverrideError (AliasingInputs x y) =
 "The expressions " ++ show (ppJavaExpr x) ++ " and " ++ show (ppJavaExpr y)
    ++ " point to the same reference, but are not allowed to alias each other."
ppOverrideError (JavaException _)    = "A Java exception was thrown."
ppOverrideError (SimException s)     = "Simulation exception: " ++ s ++ "."
ppOverrideError Abort                = "Path was aborted."

data OverrideResult
   = SuccessfulRun DagPathState (Maybe JSS.PC) (Maybe DagJavaValue)
   | FalseAssumption
   | FailedRun DagPathState (Maybe JSS.PC) [OverrideError]
   deriving (Show)

type RunResult = (DagPathState, Maybe JSS.PC, Either [OverrideError] (Maybe DagJavaValue))

orParseResults :: [OverrideResult] -> [RunResult]
orParseResults l = [ (ps, pc, Left  e) | FailedRun     ps pc e <- l ]
                ++ [ (ps, pc, Right v) | SuccessfulRun ps pc v <- l ]

type OverrideComputation = ContT OverrideResult (StateT OCState IO)

ocError :: OverrideError -> OverrideComputation ()
ocError e = modify $ \ocs -> ocs { ocsErrors = e : ocsErrors ocs }

ocAssumeFailed :: OverrideComputation a
ocAssumeFailed = ContT (\_ -> return FalseAssumption)

-- OverrideComputation utilities {{{2

-- | Runs an evaluate within an override computation.
ocEval :: (EvalContext -> ExprEvaluator b)
       -> (b -> OverrideComputation ())
       -> OverrideComputation ()
ocEval fn m = do
  ec <- gets ocsEvalContext
  res <- runEval (fn ec)
  case res of
    Left expr -> ocError $ UndefinedExpr expr
    Right v   -> m v

-- Modify result state
ocModifyResultState :: (DagPathState -> DagPathState) -> OverrideComputation ()
ocModifyResultState fn = do
  bcs <- get
  put $! bcs { ocsResultState = fn (ocsResultState bcs) }

ocModifyResultStateIO :: (DagPathState -> IO DagPathState)
                      -> OverrideComputation ()
ocModifyResultStateIO fn = do
  bcs <- get
  new <- liftIO $ fn $ ocsResultState bcs
  put $! bcs { ocsResultState = new }

-- | Add assumption for predicate.
ocAssert :: Pos -> String -> DagTerm -> OverrideComputation ()
ocAssert p nm x = do
  case getBool x of
    Just True -> return ()
    Just False -> ocError (FalseAssertion p)
    _ -> ocModifyResultState $ addAssertion nm x

-- ocStep {{{2

ocStep :: BehaviorCommand -> OverrideComputation ()
ocStep (AssertPred pos expr) =
  ocEval (evalLogicExpr expr) $ \p ->
    ocAssert pos "Override predicate" p
ocStep (AssumePred expr) = do
  de <- gets (ecDagEngine . ocsEvalContext)
  ocEval (evalLogicExpr expr) $ \v ->
    case getBool v of
      Just True -> return ()
      Just False -> ocAssumeFailed
      _ -> ocModifyResultStateIO $ addAssumption de v
ocStep (EnsureInstanceField _pos refExpr f rhsExpr) = do
  ocEval (evalJavaRefExpr refExpr) $ \lhsRef ->
    ocEval (evalMixedExpr rhsExpr) $ \value ->
      ocModifyResultState $ setInstanceFieldValue lhsRef f value
ocStep (EnsureArray _pos lhsExpr rhsExpr) = do
  ocEval (evalJavaRefExpr lhsExpr) $ \lhsRef ->
    ocEval (evalLogicExpr   rhsExpr) $ \rhsVal ->
      ocModifyResultState $ setArrayValue lhsRef rhsVal
ocStep (ModifyInstanceField refExpr f) = 
  ocEval (evalJavaRefExpr refExpr) $ \lhsRef -> do
    de <- gets (ecDagEngine . ocsEvalContext)
    let tp = JSS.fieldIdType f
    let logicType = SymInt (constantWidth (Wx (JSS.stackWidth tp)))
    n <- liftIO $ deFreshInput de logicType
    ocModifyResultState $ setInstanceFieldValue lhsRef f (mkJSSValue tp n)
ocStep (ModifyArray refExpr tp) = do
  ocEval (evalJavaRefExpr refExpr) $ \ref -> do
    de <- gets (ecDagEngine . ocsEvalContext)
    rhsVal <- liftIO (deFreshInput de tp)
    ocModifyResultState $ setArrayValue ref rhsVal
ocStep (Return expr) = do
  ocEval (evalMixedExpr expr) $ \val ->
    modify $ \ocs -> ocs { ocsReturnValue = Just val }

-- Executing overrides {{{2

execBehavior :: [BehaviorSpec] -> EvalContext -> DagPathState -> IO [RunResult]
execBehavior bsl ec ps = do
  -- Get state of current execution path in simulator.
  fmap orParseResults $ forM bsl $ \bs -> do
    let initOCS =
          OCState { ocsPC = bsPC bs
                  , ocsEvalContext = ec
                  , ocsResultState = ps
                  , ocsReturnValue = Nothing
                  , ocsErrors = []
                  }
    let resCont () = do
          OCState { ocsPC = pc
                  , ocsResultState = resPS
                  , ocsReturnValue = v
                  , ocsErrors = l } <- get
          return $
            if null l then
              SuccessfulRun resPS (Just pc) v
            else
              FailedRun resPS (Just pc) l
    flip evalStateT initOCS $ flip runContT resCont $ do

       -- Check that all expressions that reference each other may do so.
       do -- Build map from references to expressions that map to them.
          let exprs = bsRefExprs bs
          ocEval (\_ -> mapM (flip evalJavaRefExpr ec) exprs) $ \refs -> do
            let refExprMap = Map.fromListWith (++) $ refs `zip` [[e] | e <- exprs]
            --- Get counterexamples.
            let mayAliasSet = bsMayAliasSet bs
            let badPairs = catMaybes
                         $ map (\cl -> CC.checkEquivalence cl mayAliasSet)
                         $ Map.elems refExprMap
            -- Throw error if counterexample is found.
            case badPairs of
              [] -> return ()
              (x,y):_ -> ocError (AliasingInputs x y)
       let de = ecDagEngine ec
       -- Verify the initial logic assignments
       forM_ (bsLogicAssignments bs) $ \(pos, lhs, rhs) -> do
         ocEval (evalJavaExprAsLogic lhs) $ \lhsVal -> 
           ocEval (evalLogicExpr rhs) $ \rhsVal ->
             ocAssert pos "Override value assertion"
                =<< liftIO (deEq de lhsVal rhsVal)
       -- Execute statements.
       mapM_ ocStep (bsCommands bs)

checkClassesInitialized :: Pos -> String -> [String] -> JSS.Simulator SymbolicMonad ()
checkClassesInitialized pos nm requiredClasses = do
  forM_ requiredClasses $ \c -> do
    status <- JSS.getInitializationStatus c
    when (status /= Just JSS.Initialized) $
      let msg = "The method spec \'" ++ nm ++ "\' requires that the class "
                  ++ slashesToDots c ++ " is initialized.  SAWScript does not "
                  ++ "currently support methods that initialize new classes."
       in throwIOExecException pos (ftext msg) ""

execOverride :: Pos
             -> MethodSpecIR
             -> Maybe JSS.Ref
             -> [JSS.Value Node]
             -> JSS.Simulator SymbolicMonad ()
execOverride pos ir mbThis args = do
  -- Execute behaviors.
  de <- JSS.liftSymbolic $ getDagEngine
  initPS <- JSS.getPathState
  let Just bsl = Map.lookup 0 (specBehaviors ir)
  let method = specMethod ir
      argLocals = map (JSS.localIndexOfParameter method) [0..] `zip` args
  let ec = EvalContext { ecDagEngine = de
                       , ecLocals =  Map.fromList $
                           case mbThis of
                             Just th -> (0, JSS.RValue th) : argLocals
                             Nothing -> argLocals
                       , ecPathState = initPS
                       }
  -- Check class initialization.
  checkClassesInitialized pos (specName ir) (specInitializedClasses ir)
  -- Execute behavior.
  res <- liftIO . execBehavior bsl ec =<< JSS.getPathState
  when (null res) $ error "internal: execBehavior returned empty result list."
  -- Create function for generation resume actions.
  let -- Failed run
      resAction (ps, _, Left el) = do
        let msg = "Unsatisified assertions in " ++ specName ir ++ ":\n"
                    ++ intercalate "\n" (map ppOverrideError el)
        JSS.CustomRA msg $ do
          verb <- getVerbosity
          let exc = JSS.SimExtErr { JSS.simExtErrMsg = msg
                                  , JSS.simExtErrVerbosity = verb
                                    -- TODO: Determine what to put instead of Map.empty
                                  , JSS.simExtErrResults = Map.empty
                                  }
          JSS.putPathState ps { JSS.finalResult = JSS.Exc exc }
          return $ JSS.NextInst
      resAction (ps, _, Right mval) =
        JSS.CustomRA ("Override execution") $ do
          --TODO: Investigate if this is right.
          JSS.putPathState $
            case (mval, JSS.frames ps) of
              (Just val, [])  -> ps { JSS.finalResult = JSS.ReturnVal val }
              (Just val, f:r) -> ps { JSS.frames = f { JSS.frmOpds = val : JSS.frmOpds f } : r }
              (Nothing,  [])  -> ps { JSS.finalResult = JSS.Terminated }
              (Nothing,  _:_) -> ps
          return $ JSS.NextInst
  -- Split execution paths.
  let (firstRes:restRes) = res
  mapM_ (JSS.onNewPath . resAction) restRes
  JSS.onCurrPath (resAction firstRes)

-- | Add a method override for the given method to the simulator.
overrideFromSpec :: Pos -> MethodSpecIR -> JSS.Simulator SymbolicMonad ()
overrideFromSpec pos ir
  | JSS.methodIsStatic method =
      JSS.overrideStaticMethod cName key $ \args ->
        execOverride pos ir Nothing args
  | otherwise =
      JSS.overrideInstanceMethod cName key $ \thisVal args ->
        execOverride pos ir (Just thisVal) args
 where cName = JSS.className (specMethodClass ir)
       method = specMethod ir
       key = JSS.methodKey method

-- ExpectedStateDef {{{1

-- | Describes expected result of computation.
data ExpectedStateDef = ESD {
         -- | PC that we started from.
         esdStartPC :: JSS.PC
         -- | Initial path state (used for evaluating expressions in
         -- verification).
       , esdInitialPathState :: JSS.PathState Node
         -- | Stores initial assignments.
       , esdInitialAssignments :: [(TC.JavaExpr, Node)]
         -- | Map from references back to Java expressions denoting them.
       , esdRefExprMap :: Map JSS.Ref [TC.JavaExpr]
         -- | Expected return value or Nothing if method returns void.
       , esdReturnValue :: Maybe (JSS.Value Node)
         -- | Maps instance fields to expected value, or Nothing if value may
         -- be arbitrary.
       , esdInstanceFields :: Map (JSS.Ref, JSS.FieldId) (Maybe (JSS.Value Node))
         -- | Maps reference to expected node, or Nothing if value may be arbitrary.
       , esdArrays :: Map JSS.Ref (Maybe (Int32,Node))
       }

esdRefName :: JSS.Ref -> ExpectedStateDef -> String
esdRefName JSS.NullRef _ = "null"
esdRefName ref esd =
  case Map.lookup ref (esdRefExprMap esd) of
    Just cl -> ppJavaExprEquivClass cl
    Nothing -> "fresh allocation"

-- Initial state generation {{{1

-- | State for running the behavior specifications in a method override.
data ESGState = ESGState {
         esDagEngine :: DagEngine
       , esMethod :: JSS.Method
       , esExprRefMap :: Map TC.JavaExpr JSS.Ref
       , esInitialAssignments :: [(TC.JavaExpr, DagTerm)]
       , esInitialPathState :: DagPathState
       , esReturnValue :: Maybe DagJavaValue
       , esInstanceFields :: Map (JSS.Ref, JSS.FieldId) (Maybe DagJavaValue)
       , esArrays :: Map JSS.Ref (Maybe (Int32, DagTerm))
       }

-- | Monad used to execute statements in a behavior specification for a method
-- override.
type ExpectedStateGenerator = StateT ESGState IO

esEval :: (EvalContext -> ExprEvaluator b) -> ExpectedStateGenerator b
esEval fn = do
  de <- gets esDagEngine
  initPS <- gets esInitialPathState
  let ec = evalContextFromPathState de initPS
  res <- runEval (fn ec)
  case res of
    Left expr -> fail $ "internal: esEval given " ++ show expr ++ "."
    Right v   -> return v

esGetInitialPathState :: ExpectedStateGenerator DagPathState
esGetInitialPathState = gets esInitialPathState

esPutInitialPathState :: DagPathState -> ExpectedStateGenerator ()
esPutInitialPathState ps = modify $ \es -> es { esInitialPathState = ps }

esModifyInitialPathState :: (DagPathState -> DagPathState)
                         -> ExpectedStateGenerator ()
esModifyInitialPathState fn =
  modify $ \es -> es { esInitialPathState = fn (esInitialPathState es) }

esModifyInitialPathStateIO :: (DagPathState -> IO DagPathState)
                         -> ExpectedStateGenerator ()
esModifyInitialPathStateIO fn =
  do s0 <- esGetInitialPathState
     esPutInitialPathState =<< liftIO (fn s0)

esAddEqAssertion :: DagEngine -> String -> DagTerm -> DagTerm
                 -> ExpectedStateGenerator ()
esAddEqAssertion de nm x y =
  do prop <- liftIO (deEq de x y)
     esModifyInitialPathState (addAssertion nm prop)


-- | Assert that two terms are equal.
esAssertEq :: String -> DagJavaValue -> DagJavaValue
           -> ExpectedStateGenerator ()
esAssertEq nm (JSS.RValue x) (JSS.RValue y) = do
  when (x /= y) $
    error $ "internal: Asserted different references for " ++ nm ++ " are equal."
esAssertEq nm (JSS.IValue x) (JSS.IValue y) = do
  de <- gets esDagEngine
  esAddEqAssertion de nm x y
esAssertEq nm (JSS.LValue x) (JSS.LValue y) = do
  de <- gets esDagEngine
  esAddEqAssertion de nm x y
esAssertEq _ _ _ = error "internal: esAssertEq given illegal arguments."

-- | Set value in initial state.
esSetJavaValue :: TC.JavaExpr -> DagJavaValue -> ExpectedStateGenerator ()
esSetJavaValue e@(CC.Term exprF) v = do
  case exprF of
    TC.Local _ idx _ -> do
      ps <- esGetInitialPathState
      let f:r = JSS.frames ps
      case Map.lookup idx (JSS.frmLocals f) of
        Just oldValue -> esAssertEq (TC.ppJavaExpr e) oldValue v
        Nothing ->
          esPutInitialPathState ps {
              JSS.frames = f { JSS.frmLocals = Map.insert idx v (JSS.frmLocals f) }:r
            }
    TC.InstanceField refExpr f -> do
      -- Lookup refrence associated to refExpr
      Just ref <- Map.lookup refExpr <$> gets esExprRefMap
      ps <- esGetInitialPathState
      case Map.lookup (ref,f) (JSS.instanceFields ps) of
        Just oldValue -> esAssertEq (TC.ppJavaExpr e) oldValue v
        Nothing -> return ()
      esPutInitialPathState ps {
         JSS.instanceFields = Map.insert (ref,f) v (JSS.instanceFields ps)
       }

esResolveLogicExprs :: DagType -> [TC.LogicExpr]
                    -> ExpectedStateGenerator DagTerm
esResolveLogicExprs tp [] = do
  de <- gets esDagEngine
  -- Create input variable.
  liftIO $ deFreshInput de tp
esResolveLogicExprs _ (hrhs:rrhs) = do
  de <- gets esDagEngine
  t <- esEval $ evalLogicExpr hrhs
  -- Add assumptions for other equivalent expressions.
  forM_ rrhs $ \rhsExpr -> do
    rhs <- esEval $ evalLogicExpr rhsExpr
    esModifyInitialPathStateIO $ \s0 -> do prop <- deEq de t rhs
                                           addAssumption de prop s0
  -- Return value.
  return t

esSetLogicValues :: [TC.JavaExpr] -> DagType -> [TC.LogicExpr]
                 -> ExpectedStateGenerator ()
esSetLogicValues cl tp lrhs = do
  -- Get value of rhs.
  value <- esResolveLogicExprs tp lrhs
  -- Update Initial assignments.
  modify $ \es -> es { esInitialAssignments =
                         map (\e -> (e,value)) cl ++  esInitialAssignments es }
  -- Update value.
  case termType value of
     SymArray (widthConstant -> Just (Wx l)) _ -> do
       refs <- forM cl $ \expr -> do
                 JSS.RValue ref <- esEval $ evalJavaExpr expr
                 return ref
       let insertValue r m = Map.insert r (fromIntegral l, value) m
       esModifyInitialPathState $ \ps -> ps {
           JSS.arrays = foldr insertValue (JSS.arrays ps) refs
         }
     SymInt (widthConstant -> Just 32) ->
       mapM_ (flip esSetJavaValue (JSS.IValue value)) cl
     SymInt (widthConstant -> Just 64) ->
       mapM_ (flip esSetJavaValue (JSS.LValue value)) cl
     _ -> error "internal: initializing Java values given bad rhs."


esStep :: BehaviorCommand -> ExpectedStateGenerator ()
esStep (AssertPred _ expr) = do
  de <- gets esDagEngine
  v <- esEval $ evalLogicExpr expr
  esModifyInitialPathStateIO $ addAssumption de v
esStep (AssumePred expr) = do
  de <- gets esDagEngine
  v <- esEval $ evalLogicExpr expr
  esModifyInitialPathStateIO $ addAssumption de v
esStep (Return expr) = do
  v <- esEval $ evalMixedExpr expr
  modify $ \es -> es { esReturnValue = Just v }
esStep (EnsureInstanceField _pos refExpr f rhsExpr) = do
  -- Evaluate expressions.
  ref <- esEval $ evalJavaRefExpr refExpr
  value <- esEval $ evalMixedExpr rhsExpr
  -- Get dag engine
  de <- gets esDagEngine
  -- Check that instance field is already defined, if so add an equality check for that.
  ifMap <- gets esInstanceFields
  case (Map.lookup (ref, f) ifMap, value) of
    (Nothing, _) -> return ()
    (Just Nothing, _) -> return ()
    (Just (Just (JSS.RValue prev)), JSS.RValue new)
      | prev == new -> return ()
    (Just (Just (JSS.IValue prev)), JSS.IValue new) ->
       esAddEqAssertion de (show refExpr) prev new
    (Just (Just (JSS.LValue prev)), JSS.LValue new) ->
       esAddEqAssertion de (show refExpr) prev new
    -- TODO: See if we can give better error message here.
    -- Perhaps this just ends up meaning that we need to verify the assumptions in this
    -- behavior are inconsistent.
    _ -> error "internal: Incompatible values assigned to instance field."
  -- Define instance field post condition.
  modify $ \es ->
    es { esInstanceFields = Map.insert (ref,f) (Just value) (esInstanceFields es) }
esStep (ModifyInstanceField refExpr f) = do
  -- Evaluate expressions.
  ref <- esEval $ evalJavaRefExpr refExpr
  es <- get
  -- Add postcondition if value has not been assigned.
  when (Map.notMember (ref, f) (esInstanceFields es)) $ do
    put es { esInstanceFields = Map.insert (ref,f) Nothing (esInstanceFields es) }
esStep (EnsureArray _pos lhsExpr rhsExpr) = do
  -- Evaluate expressions.
  ref    <- esEval $ evalJavaRefExpr lhsExpr
  value  <- esEval $ evalLogicExpr rhsExpr
  let SymArray (widthConstant -> Just (Wx l)) _ = termType value
  -- Get dag engine
  de <- gets esDagEngine
  -- Check if array has already been assigned value.
  aMap <- gets esArrays
  case Map.lookup ref aMap of
    Just (Just (oldLen, prev))
      | oldLen == fromIntegral l ->
        esAddEqAssertion de (show lhsExpr) prev value
        -- TODO: Check to make sure this error is avoidable.
        -- Need to make sure
      | otherwise -> error "internal: Incompatible values assigned to array."
    _ -> return ()
  -- Define instance field post condition.
  modify $ \es -> es { esArrays = Map.insert ref (Just (fromIntegral l, value)) (esArrays es) }
esStep (ModifyArray refExpr _) = do
  ref <- esEval $ evalJavaRefExpr refExpr
  es <- get
  -- Add postcondition if value has not been assigned.
  when (Map.notMember ref (esArrays es)) $ do
    put es { esArrays = Map.insert ref Nothing (esArrays es) }

initializeVerification :: MethodSpecIR
                       -> BehaviorSpec
                       -> RefEquivConfiguration
                       -> JSS.Simulator SymbolicMonad ExpectedStateDef
initializeVerification ir bs refConfig = do
  exprRefs <- mapM (JSS.genRef . jssTypeOfActual . snd) refConfig
  let refAssignments = (map fst refConfig `zip` exprRefs)
  de <- JSS.liftSymbolic $ getDagEngine
  let clName = JSS.className (specThisClass ir)
      key = JSS.methodKey (specMethod ir)
      callFrame = JSS.Call { JSS.frmClass = clName
                           , JSS.frmMethod = specMethod ir
                           , JSS.frmPC = bsPC bs
                           , JSS.frmLocals = Map.empty
                           , JSS.frmOpds = []
                           }
  -- Push frame to create appropriate merge frames.
  JSS.pushFrame callFrame
  -- Get path state selector.
  initPSS <- JSS.pathStSel <$> JSS.getPathState
  -- Create initialize path state.
  let initPS =
        JSS.PathState {
            JSS.frames         = [callFrame]
          , JSS.finalResult    = JSS.Unassigned
          , JSS.initialization = Map.fromList $
                                   [ (cl, JSS.Initialized)
                                   | cl <- specInitializedClasses ir ]
          , JSS.staticFields   = Map.empty
          , JSS.instanceFields = Map.empty
          , JSS.arrays         = Map.empty
          , JSS.refArrays      = Map.empty
          , JSS.psAssumptions  = mkCBool True
          , JSS.psAssertions   = []
          , JSS.pathStSel      = initPSS
          , JSS.classObjects   = Map.empty
          , JSS.startingPC     = bsPC bs
          , JSS.breakpoints    = Set.fromList $
              map (\brpc -> (clName, key, brpc)) (Map.keys (specBehaviors ir))
          , JSS.insnCount      = 0
          }
      initESG = ESGState { esDagEngine = de
                         , esMethod = specMethod ir
                         , esExprRefMap = Map.fromList
                             [ (e, r) | (cl,r) <- refAssignments, e <- cl ]
                         , esInitialAssignments = []
                         , esInitialPathState = initPS
                         , esReturnValue = Nothing
                         , esInstanceFields = Map.empty
                         , esArrays = Map.empty
                         }
  es <- liftIO $ flip execStateT initESG $ do
          -- Set references
          forM_ refAssignments $ \(cl,r) ->
            forM_ cl $ \e -> esSetJavaValue e (JSS.RValue r)
          -- Set initial logic values.
          case bsLogicClasses bs refConfig of
            Nothing ->
              let msg = "Unresolvable cyclic dependencies between assumptions."
               in throwIOExecException (specPos ir) (ftext msg) ""
            Just assignments -> mapM_ (\(l,t,r) -> esSetLogicValues l t r) assignments
          -- Process commands
          mapM esStep (bsCommands bs)
  let ps = esInitialPathState es
  JSS.putPathState ps
  return ESD { esdStartPC = bsPC bs
             , esdInitialPathState = esInitialPathState es
             , esdInitialAssignments = reverse (esInitialAssignments es)
             , esdRefExprMap =
                  Map.fromList [ (r, cl) | (cl,r) <- refAssignments ]
             , esdReturnValue = esReturnValue es
               -- Create esdArrays map while providing entry for unspecified
               -- expressions.
             , esdInstanceFields =
                 Map.union (esInstanceFields es)
                           (Map.map Just (JSS.instanceFields ps))
               -- Create esdArrays map while providing entry for unspecified
               -- expressions.
             , esdArrays =
                 Map.union (esArrays es)
                           (Map.map Just (JSS.arrays ps))
             }

-- MethodSpec verification {{{1

-- VerificationCheck {{{2

data VerificationCheck
  = AssertionCheck String DagTerm -- ^ Name of assertion.
  -- | Check that equalitassertion is true.
  | EqualityCheck String -- ^ Name of value to compare
                  DagTerm -- ^ Value returned by JVM symbolic simulator.
                  DagTerm -- ^ Expected value in Spec.
  deriving (Eq, Ord, Show)

vcName :: VerificationCheck -> String
vcName (AssertionCheck nm _) = nm
vcName (EqualityCheck nm _ _) = nm

-- | Returns goal that one needs to prove.
vcGoal :: DagEngine -> VerificationCheck -> IO DagTerm
vcGoal _ (AssertionCheck _ n) = return n
vcGoal de (EqualityCheck _ x y) = deEq de x y

type CounterexampleFn = (DagTerm -> IO CValue) -> IO Doc

-- | Returns documentation for check that fails.
vcCounterexample :: VerificationCheck -> CounterexampleFn
vcCounterexample (AssertionCheck nm n) _ =
  return $ text ("Assertion " ++ nm ++ " is unsatisfied:") <+> prettyTermD n
vcCounterexample (EqualityCheck nm jvmNode specNode) evalFn =
  do jn <- evalFn jvmNode
     sn <- evalFn specNode
     return (text nm $$
        nest 2 (text "Encountered: " <> ppCValueD Mixfix jn) $$
        nest 2 (text "Expected:    " <> ppCValueD Mixfix sn))

-- PathVC {{{2

-- | Describes the verification result arising from one symbolic execution path.
data PathVC = PathVC {
          pvcStartPC :: JSS.PC
        , pvcEndPC :: Maybe JSS.PC
        , pvcInitialAssignments :: [(TC.JavaExpr, Node)]
          -- | Assumptions on inputs.
        , pvcAssumptions :: Node
          -- | Static errors found in path.
        , pvcStaticErrors :: [Doc]
          -- | What to verify for this result.
        , pvcChecks :: [VerificationCheck]
        }

type PathVCGenerator = State PathVC

-- | Add verification condition to list.
pvcgAssertEq :: String -> Node -> Node -> PathVCGenerator ()
pvcgAssertEq name jv sv  =
  modify $ \pvc -> pvc { pvcChecks = EqualityCheck name jv sv : pvcChecks pvc }

pvcgAssert :: String -> Node -> PathVCGenerator ()
pvcgAssert nm v =
  modify $ \pvc -> pvc { pvcChecks = AssertionCheck nm v : pvcChecks pvc }

pvcgFail :: Doc -> PathVCGenerator ()
pvcgFail msg =
  modify $ \pvc -> pvc { pvcStaticErrors = msg : pvcStaticErrors pvc }

-- generateVC {{{2

-- | Compare result with expected state.
generateVC :: MethodSpecIR
           -> ExpectedStateDef -- ^ What is expected
           -> RunResult -- ^ Results of symbolic execution.
           -> PathVC -- ^ Proof oblications
generateVC ir esd (ps, endPC, res) = do
  let initState  = 
        PathVC { pvcInitialAssignments = esdInitialAssignments esd
               , pvcStartPC = esdStartPC esd
               , pvcEndPC = endPC
               , pvcAssumptions = JSS.psAssumptions ps
               , pvcStaticErrors = []
               , pvcChecks = []
               }
  flip execState initState $ do
    case res of
      Left oe -> pvcgFail (vcat (map (ftext . ppOverrideError) oe))
      Right maybeRetVal -> do
        -- Check return value
        case (maybeRetVal, esdReturnValue esd) of
          (Nothing,Nothing) -> return ()
          (Just (JSS.IValue rv), Just (JSS.IValue srv)) ->
            pvcgAssertEq "return value" rv srv
          (Just (JSS.LValue rv), Just (JSS.LValue srv)) ->
            pvcgAssertEq "return value" rv srv
          (Just (JSS.RValue rv), Just (JSS.RValue srv)) ->
            when (rv /= srv) $
              pvcgFail $ ftext $ "Assigns unexpected return value."
          _ ->  error "internal: The Java method has an unsupported return type."
        -- Check initialization
        do let sinits = Set.fromList (specInitializedClasses ir)
           forM_ (Map.keys (JSS.initialization ps)) $ \cl -> do
             when (cl `Set.notMember` sinits) $ do
               pvcgFail $ ftext $
                 "Initializes extra class " ++ slashesToDots cl ++ "."
        -- Check static fields
        do forM_ (Map.toList $ JSS.staticFields ps) $ \(f,_jvmVal) -> do
             let clName = slashesToDots (JSS.fieldIdClass f)
             let fName = clName ++ "." ++ JSS.fieldIdName f
             pvcgFail $ ftext $ "Modifies the static field " ++ fName ++ "."
        -- Check instance fields
        forM_ (Map.toList (JSS.instanceFields ps)) $ \((ref,f), jval) -> do
          let fieldName = show (JSS.fieldIdName f)
                            ++ " of " ++ esdRefName ref esd
          case Map.lookup (ref,f) (esdInstanceFields esd) of
            Nothing ->
              pvcgFail $ ftext $ "Modifies the undefined field " ++ fieldName ++ "."
            Just sval -> do
              case (jval,sval) of
                (_,Nothing) -> return ()
                (jv, Just sv) | jv == sv -> return ()
                (JSS.RValue jref, Just (JSS.RValue _)) ->
                  pvcgFail $ ftext $
                    "Assigns an unexpected value " ++ esdRefName jref esd
                       ++ " to " ++ fieldName ++ "."
                (JSS.IValue jvmNode, Just (JSS.IValue specNode)) ->
                  pvcgAssertEq fieldName jvmNode specNode
                (JSS.LValue jvmNode, Just (JSS.LValue specNode)) ->
                  pvcgAssertEq fieldName jvmNode specNode
                (_, Just _) ->
                  error "internal: comparePathStates encountered illegal field type."
        -- Check value arrays
        forM_ (Map.toList (JSS.arrays ps)) $ \(ref,(jlen,jval)) -> do
          case Map.lookup ref (esdArrays esd) of
            Nothing -> pvcgFail $ ftext $ "Allocates an array."
            Just Nothing -> return ()
            Just (Just (slen, sval))
              | jlen == slen -> pvcgAssertEq (esdRefName ref esd) jval sval
              | otherwise ->
                  pvcgFail $ ftext $ "Assigns an unexpected size to an array."
        -- Check ref arrays
        when (not (Map.null (JSS.refArrays ps))) $ do
          pvcgFail $ ftext "Modifies references arrays."
        -- Check assertions
        forM_ (JSS.psAssertions ps) $ \(nm,n) -> do
          pvcgAssert nm n
        -- Check class objects
        forM_ (Map.keys (JSS.classObjects ps)) $ \clNm ->
          pvcgFail $ ftext $ "Allocated class object for " ++ slashesToDots clNm ++ "."

-- verifyMethodSpec and friends {{{2

mkSpecVC :: VerifyParams
         -> BehaviorSpec 
         -> RefEquivConfiguration
         -> ExpectedStateDef
         -> JSS.Simulator SymbolicMonad [PathVC]
mkSpecVC params bs refConfig esd = do
  let ir = vpSpec params
  -- Log execution.
  setVerbosity (simverbose (vpOpts params))
  -- Add method spec overrides.
  mapM_ (overrideFromSpec (specPos ir)) (vpOver params)
  -- Execute code.
  jssResults <- JSS.run
  finalPathResults <-
    forM jssResults $ \(pd, fr) -> do
      finalPS <- JSS.getPathStateByName pd
      case fr of
        JSS.ReturnVal val -> return [(finalPS, Nothing, Right (Just val))]
        JSS.Terminated ->    return [(finalPS, Nothing, Right Nothing)]
        JSS.Breakpoint pc -> do
          de <- JSS.liftSymbolic getDagEngine
          -- Execute behavior specs at PC.
          let Just bsl = Map.lookup pc (specBehaviors ir)
          let ec = evalContextFromPathState de finalPS
          liftIO $ execBehavior bsl ec finalPS
        JSS.Exc JSS.SimExtErr { JSS.simExtErrMsg = msg } ->
          return [(finalPS, Nothing, Left [SimException msg]) ]
        JSS.Exc JSS.JavaException{ JSS.excRef = r } ->
          return [(finalPS, Nothing, Left [JavaException r])]
        JSS.Aborted ->
          return [(finalPS, Nothing, Left [Abort])]
        JSS.Unassigned -> error "internal: run terminated before completing."
  return $ map (generateVC ir esd) (concat finalPathResults)

data VerifyParams = VerifyParams
  { vpOpCache :: OpCache
  , vpCode    :: JSS.Codebase
  , vpOpts    :: SSOpts
  , vpSpec    :: MethodSpecIR
  , vpOver    :: [MethodSpecIR]
  , vpRules   :: [Rule]
  , vpEnabledRules :: Set String
  , vpEnabledOps  :: Set OpIndex
  }

-- | Attempt to verify method spec using verification method specified.
validateMethodSpec :: VerifyParams -> IO ()
validateMethodSpec
    params@VerifyParams { vpCode = cb
                        , vpOpCache = oc
                        , vpOpts = opts
                        , vpSpec = ir
                        } = do
  let verb = verbose opts
  when (verb >= 2) $ putStrLn $ "Starting verification of " ++ specName ir
  let configs = [ (bs, cl)
                | bs <- concat $ Map.elems $ specBehaviors ir
                , cl <- bsRefEquivClasses bs
                ]
  forM_ configs $ \(bs,cl) -> do
    when (verb >= 3) $ do
      liftIO $ putStrLn $ "Executing " ++ specName ir
                             ++ " at PC " ++ show (bsPC bs) ++ "."
    runSymbolic oc $ do
      de <- getDagEngine
      setVerbosity verb
      (esd,results) <- 
        JSS.runSimulator cb $ do
           -- Create initial Java state.
           esd <- initializeVerification ir bs cl
           res <- mkSpecVC params bs cl esd
           return (esd,res)
      liftIO $ forM_ results $ \pvc -> do
        let mkVState nm cfn =
              VState { vsVCName = nm
                     , vsMethodSpec = ir
                     , vsVerbosity = verb
                     , vsRules = vpRules params
                     , vsEnabledRules = vpEnabledRules params
                     , vsEnabledOps = vpEnabledOps params
                     , vsFromPC = bsPC bs
                     , vsEvalContext = 
                         evalContextFromPathState de (esdInitialPathState esd)
                     , vsInitialAssignments = pvcInitialAssignments pvc
                     , vsCounterexampleFn = cfn
                     , vsStaticErrors = pvcStaticErrors pvc
                     }
        case specValidationPlan ir of
          Skip -> error "internal: Unexpected call to validateMethodSpec with Skip"
          QuickCheck n lim -> do
            testRandom de verb ir (fromInteger n) (fromInteger <$> lim) pvc
          Verify cmds
            | null (pvcStaticErrors pvc) -> do
               forM_ (pvcChecks pvc) $ \vc -> do
                 let vs = mkVState (vcName vc) (vcCounterexample vc)
                 g <- deImplies de (pvcAssumptions pvc) =<< vcGoal de vc
                 runVerify vs g cmds
            | otherwise -> do
               let vs = mkVState ("an invalid path "
                                     ++ (case pvcStartPC pvc of
                                           0 -> ""
                                           pc -> " from pc " ++ show pc)
                                     ++ maybe "" (\pc -> " to pc " ++ show pc) 
                                              (pvcEndPC pvc))
                                 (\_ -> return $ vcat (pvcStaticErrors pvc))
               g <- deImplies de (pvcAssumptions pvc) (mkCBool False)
               when (verb >= 3) $ do
                 putStrLn $ "Calling runVerify with " ++
                            prettyTerm (pvcAssumptions pvc)
               runVerify vs g cmds

data VerifyState = VState {
         vsVCName :: String
       , vsMethodSpec :: MethodSpecIR
       , vsVerbosity :: Verbosity
       , vsRules :: [Rule]
       , vsEnabledRules :: Set String
       , vsEnabledOps :: Set OpIndex
         -- | Starting PC is used for checking VerifyAt commands.
       , vsFromPC :: JSS.PC
         -- | Evaluation context used for parsing expressions during
         -- verification.
       , vsEvalContext :: EvalContext
       , vsInitialAssignments :: [(TC.JavaExpr, DagTerm)]
       , vsCounterexampleFn :: CounterexampleFn
       , vsStaticErrors :: [Doc]
       }

vsDagEngine :: VerifyState -> DagEngine
vsDagEngine = ecDagEngine . vsEvalContext

type VerifyExecutor = StateT VerifyState IO

runVerify :: VerifyState -> Node -> [VerifyCommand] -> IO ()
runVerify vs g cmds = evalStateT (applyTactics cmds g) vs

-- runABC {{{2

runABC :: Node -> VerifyExecutor ()
runABC goal = do
  de <- gets vsDagEngine
  v <- gets vsVerbosity
  ir <- gets vsMethodSpec
  ia <- gets vsInitialAssignments
  cfn <- gets vsCounterexampleFn

  liftIO $ do
    when (v >= 3) $ do
      putStrLn $ "Running ABC on " ++ specName ir
      putStrLn $ "Goal is:"
      putStrLn $ prettyTerm goal
    be <- createBitEngine
    flip finally (beFree be) $ do
      inputTypes <- deInputTypes de
      inputPairs <- V.forM inputTypes $ \tp -> do
        -- Get number of literals.
        lc <- beInputLitCount be
        -- Create input variable.
        l <- let ?be = be in mkInputLitResultWithType tp
        let evalFn bits = mkInputEval tp $ SV.slice lc (typeBitCount tp) bits
        return (l,evalFn)
      let (iLits,inputs) = V.unzip inputPairs
      let inputFn i _ = return (iLits V.! i)
      lEval <- evalDagTermFn inputFn (mkBitBlastTermSemantics be)
      LV value <- lEval goal
      unless (SV.length value == 1) $
        error "internal: Unexpected number of in verification condition"
      case beCheckSat be of
        Nothing -> error "internal: Bit engine does not support SAT checking."
        Just checkSat -> do
          b <- checkSat (beNeg be (value SV.! 0))
          case b of
            UnSat -> when (v >= 3) $ putStrLn "Verification succeeded."
            Unknown -> do
              let msg = "ABC has returned a status code indicating that it "
                        ++ "could not determine whether the specification is "
                        ++ "correct.  This result is not expected for "
                        ++ "sequential circuits, and could indicate an internal"
                        ++ "error in ABC or SAWScript's connection to ABC."
               in throwIOExecException (specPos ir) (ftext msg) ""
            Sat lits -> do
              evalFn <- concreteEvalFn (V.map (\fn -> fn lits) inputs)
              -- Get doc showing inputs
              let docInput (e,n) =
                    do vn <- evalFn n
                       return $ text (TC.ppJavaExpr e) <+> equals
                                                       <+> ppCValueD Mixfix vn
              inputDocs <- mapM docInput ia

              -- Get differences between two.
              val <- cfn evalFn
              let msg = ftext ("ABC failed to verify " ++ specName ir ++ ".\n\n") $$
                        ftext ("The inputs that generated the failure are:") $$
                        nest 2 (vcat inputDocs) $$
                         ftext ("Counterexample:") $$ nest 2 val
              throwIOExecException (specPos ir) msg ""

-- testRandom {{{2

type Verbosity = Int

testRandom :: DagEngine -> Verbosity
           -> MethodSpecIR -> Int -> Maybe Int -> PathVC -> IO ()
testRandom de v ir test_num lim pvc =
    do when (v >= 3) $
         putStrLn $ "Generating random tests: " ++ specName ir
       (passed,run) <- loop 0 0
       when (passed < test_num) $
         let m = text "Quickcheck: Failed to generate enough good inputs."
                $$ nest 2 (vcat [ text "Attempts:" <+> int run
                                , text "Passed:" <+> int passed
                                , text "Goal:" <+> int test_num
                                ])
         in throwIOExecException (specPos ir) m ""
  where
  loop run passed | passed >= test_num      = return (passed,run)
  loop run passed | Just l <- lim, run >= l = return (passed,run)
  loop run passed = loop (run + 1) =<< testOne passed

  testOne passed = do
    vs   <- V.mapM QuickCheck.pickRandom =<< deInputTypes de
    eval <- concreteEvalFn vs
    badAsmp <- isViolated eval (pvcAssumptions pvc)
    if badAsmp
      then do
        return passed
      else do when (v >= 4) $
                dbugM $ "Begin concrete DAG eval on random test case for all goals ("
                        ++ show (length $ pvcChecks pvc) ++ ")."
              forM_ (pvcChecks pvc) $ \goal ->
                do bad_goal <- isInvalid eval goal
                   when (v >= 4) $ dbugM "End concrete DAG eval for one VC check."
                   when bad_goal $ do
                     (vs1,goal1) <- QuickCheck.minimizeCounterExample
                                            isCounterExample (V.toList vs) goal
                     txt <- msg eval goal1
                     throwIOExecException (specPos ir) txt ""
              return $! passed + 1

  isCounterExample vs =
    do eval    <- concreteEvalFn (V.fromList vs)
       badAsmps <- isViolated eval (pvcAssumptions pvc)
       if badAsmps
         then return Nothing
         else findM (isInvalid eval) (pvcChecks pvc)

  isViolated eval goal = (not . toBool) <$> (eval goal)
  isInvalid eval vcc   = isViolated eval =<< vcGoal de vcc

  msg eval g =
    do what_happened <-
         case g of
           EqualityCheck n x y ->
              do val_y <- eval y
                 val_x <- eval x
                 return (text "Unexpected value for:" <+> text n
                         $$ nest 2 (text "Expected:" <+> ppCValueD Mixfix val_y)
                         $$ text "Found:"    <+> ppCValueD Mixfix val_x)
           AssertionCheck nm _ -> return (text ("Invalid " ++ nm))

       args <- mapM (ppInput eval) (pvcInitialAssignments pvc)

       return (
         text "Random testing found a counter example:"
         $$ nest 2 (vcat
            [ text "Method:" <+> text (specName ir)
            , what_happened
            , text "Arguments:" $$ nest 2 (vcat args)
            ])
         )

  ppInput eval (expr, n) =
    do val <- eval n
       return $ text (ppJavaExpr expr) <+> text "=" <+> ppCValueD Mixfix val
         {-
         tsUnsorted ->
           let tsSorted = sortBy cmp tsUnsorted
               t0       = last tsSorted
               ts       = init tsSorted

               cmp (Arg a _) (Arg b _) = compare a b
               cmp _ _                 = EQ

           in vcat [ text (show t) <+> text "=" <+> text (show t0) | t <- ts ]
              $$ text (show t0) <+> text "=" <+> ppCValueD Mixfix value
              -}

  toBool (CBool b) = b
  toBool value = error $ unlines [ "Internal error in 'testRandom':"
                                 , "  Expected: boolean value"
                                 , "  Result:   " ++ ppCValue Mixfix value ""
                                 ]

-- useSMTLIB {{{2

announce :: String -> VerifyExecutor ()
announce msg = do
  v <- gets vsVerbosity
  when (v >= 3) $ liftIO (putStrLn msg)

useSMTLIB :: Maybe Int -> Maybe String -> Node -> VerifyExecutor ()
useSMTLIB mbVer mbNm g = do
  de <- gets vsDagEngine
  ir <- gets vsMethodSpec
  enabledOps <- gets vsEnabledOps
  inputTypes <- liftIO $ deInputTypes de
  announce ("Translating to SMTLIB (version " ++ show version ++"): " ++ specName ir)
  let name = case mbNm of
               Just x  -> x
               Nothing -> specName ir
  liftIO $ do
    let params = SmtLib.TransParams
                   { SmtLib.transName = name
                   , SmtLib.transInputs = V.toList inputTypes
                   , SmtLib.transAssume = mkCBool True
                   , SmtLib.transCheck = [g]
                   , SmtLib.transEnabled = enabledOps
                   , SmtLib.transExtArr = True
                   }
    doc <-
      case version of
        1 -> do (script,_) <- SmtLib.translate params
                return (SmtLib.pp script)

        2 -> do (script,_) <- SmtLib2.translate params
                return (SmtLib2.pp script)
        _ -> error "Unexpected version"

    -- XXX: THERE IS A RACE CONDITION HERE!
    let pickName n = do let cand = name ++ (if n == 0 then "" else show n)
                                        ++ ".smt" ++ ver_exr
                        b <- doesFileExist cand
                        if b then pickName (n + 1) else return cand

    fileName <- pickName (0 :: Integer)
    writeFile fileName (show doc)

  where
  version :: Int
  (version, ver_exr) = case mbVer of
                         Just n | n /= 1 -> (n, show n)
                         _      -> (1, "")   -- For now, we use 1 by default.

-- useYices {{{2

useYices :: Maybe Int -> Node -> VerifyExecutor ()
useYices mbTime g = do
  de <- gets vsDagEngine
  ir <- gets vsMethodSpec
  enabledOps <- gets vsEnabledOps
  inputTypes <- liftIO $ deInputTypes de
  ia <- gets vsInitialAssignments
  announce ("Using Yices2: " ++ specName ir)
  liftIO $ do
    (script,info) <- SmtLib.translate SmtLib.TransParams
        { SmtLib.transName = "CheckYices"
        , SmtLib.transInputs = V.toList inputTypes
        , SmtLib.transAssume = mkCBool True
        , SmtLib.transCheck = [g]
        , SmtLib.transEnabled = enabledOps
        , SmtLib.transExtArr = True
        }

    res <- Yices.yices mbTime script
    case res of
      Yices.YUnsat   -> return ()
      Yices.YUnknown -> yiFail ir (text "Failed to decide property.")
      Yices.YSat m   ->
        yiFail ir ( text "Found a counter example:"
              -- TODO: Fix counterexample generation.
              -- $$ nest 2 (vcat $ intersperse (text " ") $
              --     zipWith ppIn ia (map (Yices.getIdent m) (SmtLib.trInputs info)))
              $$ text " "
              $$ ppUninterp m (SmtLib.trUninterp info)

              $$ ppArgHist ia info m
              $$ text " "

              $$ text "Assumptions:"
              $$ nest 2 (SmtLib.pp (SmtLib.trAsmp info))
              $$ text " "
              $$ text "Goals:"
              $$ nest 2 (vcat $ intersperse (text " ")
                              $ map SmtLib.pp (SmtLib.trGoals info))
              $$ text " "

              $$ text "Full model:"
              $$ nest 2 (vcat $ map Yices.ppVal (Map.toList m))
               )
  where
  yiFail ir xs = fail $ show $ vcat $
                   [ text "Yices: Verification failed."
                   , text "*** Method:" <+> text (specName ir)
                   , text "*** Location:" <+> text (show (specPos ir))
                   , text "*** Details:"
                   , nest 2 xs
                   ]

  ppIn expr val = Yices.ppVal (TC.ppJavaExpr expr, val)

  ppUninterp _ [] = empty
  ppUninterp m us =
    text "Uninterpreted functions:"
    $$ nest 2 (vcat $
      [ Yices.ppVal (s, Yices.getIdent m i) $$ text " " | (i,s) <- us ]
    )

{-
  varName base 0    = base
  varName base time = base ++ "@" ++ show time
  -}

  varSuccessors m time i = (i,time) :
    case Map.lookup i m of
      Nothing -> []
      Just js -> concatMap (varSuccessors m $! (time + 1)) js

  ppHist model upds nm arg = vcat $ intersperse (text " ")
    [ Yices.ppVal (nm {-varName nm time-}, Yices.getIdent model i)
         -- This is written in this weird way, so that we can easily
         -- switch between the whole update-history for a variable
         -- and just the last value.
         | (i,time) <- [ last $ varSuccessors upds (0::Integer) arg ],
                                                              time /= 0 ]

  ppArgHist ia info model =
    case zipWith (ppHist model (SmtLib.trArrays info))
                 (map (ppJavaExpr . fst) ia)
                 (SmtLib.trInputs info) of
      [] -> empty
      ds -> text "Final values for array arguments:"
         $$ nest 2 (vcat (intersperse (text " ") ds))
-- applyTactics {{{2

applyTactics :: [VerifyCommand] -> Node -> VerifyExecutor ()
applyTactics (Rewrite:r) g = do
  de <- gets vsDagEngine
  rules <- gets vsRules
  enRules <- gets vsEnabledRules
  let pgm = foldl' addRule' emptyProgram rules
      addRule' p r | ruleName r `Set.member` enRules = addRule p r
                   | otherwise = p
  g' <- liftIO $ do
          rew <- mkRewriter pgm de
          reduce rew g
  case getBool g' of
    Just True -> return ()
    _ -> applyTactics r g'
applyTactics (ABC:_) goal = runABC goal
-- SmtLib always succeeds.
applyTactics (SmtLib ver file :_) g = useSMTLIB ver file g
-- Yices always succeeds.
applyTactics (Yices v :_) g = useYices v g
applyTactics (Expand p expandOp argExprs rhs:r) g = do
  ec <- gets vsEvalContext
  mterms <- runEval $ V.mapM (flip evalLogicExpr ec) (V.fromList argExprs)
  case mterms of
    Left expr -> 
      error $ "internal: Unexpected expression " ++ ppJavaExpr expr
    Right terms -> do
      let de = ecDagEngine ec
      let applyFn op lazyArgs = do
            args <- V.sequence lazyArgs
            if op == expandOp && args == terms then
              let argFn i _ = return (terms V.! i)
               in evalDagTerm argFn (deTermSemantics de) rhs
            else 
              deApplyOp de op args
      let ts = mkTermSemantics (\i tp -> return (ConstTerm i tp)) applyFn
      g' <- liftIO $ do
        inputs <- deInputTerms de
        evalDagTerm (\i _ -> return (inputs V.! i)) ts g
      applyTactics r g'
applyTactics (VerifyEnable nm :r) g = do
  modify (\s -> s { vsEnabledRules = Set.insert nm (vsEnabledRules s) })
  applyTactics r g
applyTactics (VerifyDisable nm :r) g = do
  modify (\s -> s { vsEnabledRules = Set.delete nm (vsEnabledRules s) })
  applyTactics r g
applyTactics (VerifyAt pc cmds :r) g = do
  fromPC <- gets vsFromPC
  let newCmds | fromPC == pc = cmds ++ r
              | otherwise = r
  applyTactics newCmds g
applyTactics [] g = do
  nm <- gets vsVCName
  ir <- gets vsMethodSpec
  se <- gets vsStaticErrors
  if null se then
    let msg = ftext ("Failed to discharge the verification of " ++ nm
                       ++ " in " ++ specName ir ++ " .\n\n") $$
              ftext ("The remaining goal is:") $$
              nest 2 (prettyTermD g)
     in throwIOExecException (specPos ir) msg ""
  else
    let msg = ftext ("A potentially satisfiable error path was found when verifying "
                       ++ nm ++ " in " ++ specName ir ++ " .\n\n") $$
              ftext ("Path errors:") $$ nest 2 (vcat se) $$
              ftext ("The remaining goal is:") $$ nest 2 (prettyTermD g)
     in throwIOExecException (specPos ir) msg ""

