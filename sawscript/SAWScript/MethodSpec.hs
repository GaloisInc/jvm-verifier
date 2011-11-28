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
import qualified Control.Exception as CE
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error (Error(..), ErrorT, runErrorT, throwError)
import Control.Monad.Identity
import Control.Monad.State
import Data.Either (lefts, rights)
import Data.Int
import Data.IORef
import Data.List (foldl', intercalate, sort,intersperse,sortBy,find)
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
import qualified Execution.JavaSemantics as Sem
import qualified JavaParser as JSS
import MethodSpec (partitions)
import qualified SAWScript.CongruenceClosure as CC
import qualified SAWScript.SmtLib as SmtLib
import qualified SAWScript.SmtLib2 as SmtLib2
import qualified SAWScript.QuickCheck as QuickCheck
import qualified SAWScript.Yices  as Yices
import qualified SAWScript.MethodAST as AST
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

-- Verinf Utilities {{{1

-- | Create a node with given number of bits.
mkIntInput :: (SV.Storable l) => DagEngine n l -> Int -> IO n
mkIntInput de w = do
  let ?be = deBitEngine de
  lv <- LV <$> SV.replicateM w lMkInput
  deFreshInput de (Just lv) (SymInt (constantWidth (Wx w)))

-- | Create a lit result with input bits for the given ground dag type.
mkInputLitResultWithType :: (?be :: BitEngine l, SV.Storable l)
                         => DagType -> IO (LitResult l)
mkInputLitResultWithType (SymInt (widthConstant -> Just (Wx w))) =
  LV <$> SV.replicateM w lMkInput
mkInputLitResultWithType (SymArray (widthConstant -> Just (Wx l)) eltTp) =
  LVN <$> V.replicateM l (mkInputLitResultWithType eltTp)
mkInputLitResultWithType _ =
  error "internal: mkInputLitResultWithType called with unsupported type."

-- | Create ani node with given number of bits.
mkInputWithType :: SV.Storable l => DagEngine n l -> DagType -> IO n
mkInputWithType de tp = do
 r <- let ?be = deBitEngine de in mkInputLitResultWithType tp
 deFreshInput de (Just r) tp

deEq :: TypedTerm t => DagEngine t l -> t -> t -> t 
deEq de x y = deApplyBinary de (eqOp (termType x)) x y

typeBitCount :: DagType -> Int
typeBitCount SymBool = 1
typeBitCount (SymInt (widthConstant -> Just (Wx w))) = w
typeBitCount (SymArray (widthConstant -> Just (Wx l)) e) = l * typeBitCount e
typeBitCount (SymRec d s) = V.sum $ V.map typeBitCount $ recFieldTypes d s

mkInputEval :: DagType -> SV.Vector Bool -> CValue
mkInputEval (SymInt (widthConstant -> Just (Wx w))) lits = mkCIntFromLsbfV lits
mkInputEval (SymArray (widthConstant -> Just (Wx l)) e) lits =
  let w = typeBitCount e
   in CArray $ V.generate l $ (\i -> mkCIntFromLsbfV $ SV.slice (i*w) w lits)
mkInputEval _ _ = error "internal: Unsupported type given to mkInputEval"

-- JSS Utilities {{{1

-- | Return value of field in path state.
instanceFieldValue :: JSS.Ref -> JSS.FieldId -> JSS.PathState n -> Maybe (JSS.Value n)
instanceFieldValue r f ps = Map.lookup (r,f) (JSS.instanceFields ps)

-- | Set value of bound to instance field in path state.
setInstanceFieldValue :: JSS.Ref -> JSS.FieldId -> JSS.Value n
                      -> JSS.PathState n -> JSS.PathState n
setInstanceFieldValue r f v ps =
  ps { JSS.instanceFields = Map.insert (r,f) v (JSS.instanceFields ps) }

-- | Set value bound to array in path state. 
-- Assumes value is an array with a ground length.
setArrayValue :: TypedTerm n => JSS.Ref -> n -> JSS.PathState n -> JSS.PathState n
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
addAssumption :: DagEngine n l -> n -> JSS.PathState n -> JSS.PathState n
addAssumption de x ps =
  ps { JSS.psAssumptions =
         deApplyBinary de bAndOp (JSS.psAssumptions ps) x
     }

-- | Add assertion for predicate to path state.
addAssertion :: n -> JSS.PathState n -> JSS.PathState n
addAssertion x ps =
  ps { JSS.psAssertions = x : JSS.psAssertions ps }

-- EvalContext {{{1

-- | Contextual information needed to evaluate expressions.
data EvalContext n l = EvalContext {
         ecDagEngine :: DagEngine n l
       , ecThis :: Maybe JSS.Ref
       , ecArgs :: V.Vector (Maybe (JSS.Value n))
       , ecPathState :: JSS.PathState n
       }

evalContextFromPathState :: DagEngine t l -> JSS.PathState t -> EvalContext  t l
evalContextFromPathState de ps = 
  let f:r = JSS.frames ps
      method   = JSS.frmMethod f
      paramCount = length (JSS.methodParameterTypes method)
      localMap = JSS.frmLocals f
   in EvalContext {
          ecDagEngine = de
        , ecThis = if JSS.methodIsStatic method then
                     Nothing
                   else 
                     (\(JSS.RValue r) -> r) <$> Map.lookup 0 localMap
        , ecArgs = V.generate paramCount $ \i ->
            Map.lookup (JSS.localIndexOfParameter method i) localMap
        , ecPathState = ps
        }


type ExprEvaluator a = ErrorT TC.JavaExpr Identity a

instance Error TC.JavaExpr where
  noMsg = error "noMsg called with TC.JavaExpr"

runEval :: ExprEvaluator b -> Either TC.JavaExpr b
runEval v = runIdentity (runErrorT v) 

-- or undefined subexpression if not.
-- N.B. This method assumes that the Java path state is well-formed, the
-- the JavaExpression syntactically referes to the correct type of method
-- (static versus instance), and correct well-typed arguments.  It does
-- not assume that all the instanceFields in the JavaEvalContext are initialized.
evalJavaExpr :: TC.JavaExpr -> EvalContext n l -> ExprEvaluator (JSS.Value n)
evalJavaExpr expr ec = eval expr
  where eval e@(CC.Term f) =
          case f of
            TC.This _ -> 
              case ecThis ec of
                Just r -> return (JSS.RValue r)
                Nothing -> error "internal: evalJavaExpr given TC.This for static method"
            TC.Arg i _ ->
              let args = ecArgs ec
               in assert (i < V.length args) $
                    case args V.! i of
                      Just v -> return v
                      Nothing -> throwError e
            TC.Local _ idx _ ->
              let f:_ = JSS.frames (ecPathState ec)
               in case Map.lookup idx (JSS.frmLocals f) of
                    Just v -> return v
                    Nothing -> throwError e
            TC.InstanceField r f -> do
              JSS.RValue ref <- eval r
              case Map.lookup (ref, f) (JSS.instanceFields (ecPathState ec)) of
                Just v -> return v
                Nothing -> throwError e

evalJavaRefExpr :: TC.JavaExpr -> EvalContext n l -> ExprEvaluator JSS.Ref
evalJavaRefExpr expr ec = do
  val <- evalJavaExpr expr ec
  case val of
    JSS.RValue ref -> return ref
    _ -> error "internal: evalJavaRefExpr encountered illegal value."

evalJavaExprAsLogic :: TC.JavaExpr -> EvalContext n l -> ExprEvaluator n
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
evalLogicExpr :: TC.LogicExpr -> EvalContext n l -> ExprEvaluator n
evalLogicExpr initExpr ec = eval initExpr
  where de = ecDagEngine ec
        eval (TC.Apply op exprs) =
          deApplyOp de op <$> V.mapM eval (V.fromList exprs)
        eval (TC.Cns c tp) = return $ deConstantTerm de c tp
        eval (TC.Var _ _) = error "internal: evalLogicExpr called with var"
        eval (TC.JavaValue expr _ _) = evalJavaExprAsLogic expr ec

{-
-- | Value of a mixed expression.
data MixedValue n
   = MVNode n
   | MVRef JSS.Ref

evalMixedExpr :: EvalContext n l -> TC.MixedExpr -> ExprEvaluator (MixedValue n)
evalMixedExpr ec (LE expr) = MVNode <$> evalLogicExpr ec expr
evalMixedExpr ec (JE expr) = do
 res <- evalJavaExpr ec expr
 case res of
   --TODO: Fix this and other areas to give graceful error message.
   -- This would ideally be done during type checking, but would require analzing
   -- the Java byte code to determine what local and stack values are expected at each location.
   JSS.AValue _ -> error "internal: evalMixedExpr given address value"
   JSS.DValue _ -> error "internal: evalMixedExpr given floating point value"
   JSS.FValue _ -> error "internal: evalMixedExpr given floating point value"
   JSS.IValue n -> return $ MVNode n
   JSS.LValue n -> return $ MVNode n
   JSS.RValue r -> return $ MVRef r
   -}

-- | Return Java value associated with mixed expression.
evalMixedExpr :: TypedTerm n
              => TC.MixedExpr -> EvalContext n l -> ExprEvaluator (JSS.Value n)
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
data OCState n l = OCState {
         ocsEvalContext :: !(EvalContext n l)
       , ocsResultState :: !(JSS.PathState n)
       , ocsReturnValue :: !(Maybe (JSS.Value n))
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
ppOverrideError (UndefinedExpr expr) = "Could not evaluate " ++ show expr ++ "."
ppOverrideError (FalseAssertion p)   = "Assertion at " ++ show p ++ " is false."
ppOverrideError (AliasingInputs x y) = "The expressions " ++ show x ++ " and " ++ show y
  ++ " point to the same reference, but are not allowed to alias each other."
ppOverrideError (JavaException _)    = "A Java exception was thrown."
ppOverrideError (SimException s)     = "Simulation exception: " ++ s ++ "."

data OverrideResult n
   = SuccessfulRun (JSS.PathState n) (Maybe (JSS.Value n))
   | FalseAssumption
   | FailedRun (JSS.PathState n) OverrideError -- ^ Type of error and assumptions leading up to error.
   deriving (Show)

type RunResult n = (JSS.PathState n, Either OverrideError (Maybe (JSS.Value n)))

orParseResults :: [OverrideResult n] -> [RunResult n]
orParseResults l = [ (ps, Left e) | FailedRun ps e <- l ] ++ [ (ps, Right v) | SuccessfulRun ps v <- l ]

type OverrideComputation n l = ContT (OverrideResult n) (StateT (OCState n l) IO)

ocError :: OverrideError -> OverrideComputation n l a
ocError e = ContT $ \_ -> do
  ps <- gets ocsResultState
  return (FailedRun ps e)

ocAssumeFailed :: OverrideComputation n l a
ocAssumeFailed = ContT (\_ -> return FalseAssumption)

-- OverrideComputation utilities {{{2

-- | Runs an evaluate within an override computation.
ocEval :: (EvalContext n l -> ExprEvaluator b) -> OverrideComputation n l b
ocEval fn = do
  ec <- gets ocsEvalContext
  case runEval (fn ec) of
    Left expr -> ocError $ UndefinedExpr expr
    Right v -> return v

-- Modify result state
ocModifyResultState :: (JSS.PathState n -> JSS.PathState n)
                    -> OverrideComputation n l ()
ocModifyResultState fn = do
  bcs <- get
  put $! bcs { ocsResultState = fn (ocsResultState bcs) }

-- | Add assumption for predicate.
ocAssert :: ConstantProjection n => Pos -> n -> OverrideComputation n l ()
ocAssert p x = do
  de <- gets (ecDagEngine . ocsEvalContext)
  case getBool x of
    Just True -> return ()
    Just False -> ocError (FalseAssertion p)
    _ -> ocModifyResultState $ addAssertion x

ocSetArrayValue :: TypedTerm n => TC.JavaExpr -> n -> OverrideComputation n l ()
ocSetArrayValue lhsExpr rhsVal = do
  lhsRef <- ocEval $ evalJavaRefExpr lhsExpr
  ocModifyResultState $ setArrayValue lhsRef rhsVal

-- ocStep {{{2

ocStep :: (ConstantProjection n, TypedTerm n, SV.Storable l)
       => BehaviorCommand
       -> OverrideComputation n l ()
ocStep (InputValue _ _) = return ()
ocStep (AssertValue pos lhs rhs) = do
  lhsVal <- ocEval $ evalJavaExprAsLogic lhs
  rhsVal <- ocEval $ evalLogicExpr rhs
  de <- gets (ecDagEngine . ocsEvalContext)
  ocAssert pos (deEq de lhsVal rhsVal)
ocStep (AssertPred pos expr) =
  ocAssert pos =<< ocEval (evalLogicExpr expr)
ocStep (AssumePred expr) = do
  de <- gets (ecDagEngine . ocsEvalContext)
  v <- ocEval (evalLogicExpr expr)
  case getBool v of
    Just True -> return ()
    Just False -> ocAssumeFailed
    _ -> ocModifyResultState $ addAssumption de v
ocStep (EnsureInstanceField _pos refExpr f rhsExpr) = do
  lhsRef <- ocEval $ evalJavaRefExpr refExpr
  value <- ocEval $ evalMixedExpr rhsExpr
  ocModifyResultState $ setInstanceFieldValue lhsRef f value
ocStep (EnsureArray _pos lhsExpr rhsExpr) = do
  lhsRef <- ocEval $ evalJavaRefExpr lhsExpr
  rhsVal <- ocEval $ evalLogicExpr   rhsExpr
  ocModifyResultState $ setArrayValue lhsRef rhsVal
ocStep (ModifyInstanceField refExpr f) = do
  lhsRef <- ocEval $ evalJavaRefExpr refExpr
  de <- gets (ecDagEngine . ocsEvalContext)
  let tp = JSS.fieldIdType f
  n <- liftIO $ mkIntInput de (JSS.stackWidth tp)
  ocModifyResultState $ setInstanceFieldValue lhsRef f (mkJSSValue tp n)
ocStep (ModifyArray refExpr tp) = do
  ref <- ocEval $ evalJavaRefExpr refExpr
  de <- gets (ecDagEngine . ocsEvalContext)
  rhsVal <- liftIO (mkInputWithType de tp)
  ocModifyResultState $ setArrayValue ref rhsVal
ocStep (Return expr) = do
  val <- ocEval $ evalMixedExpr expr
  modify $ \ocs -> ocs { ocsReturnValue = Just val }

-- Executing overrides {{{2

execBehavior :: (ConstantProjection n, TypedTerm n, SV.Storable l)
             => Pos
             -> MethodSpecIR
             -> [BehaviorSpec]
             -> EvalContext n l
             -> JSS.PathState n
             -> IO [RunResult n]
execBehavior pos ir bsl ec ps = do
  -- Get state of current execution path in simulator.
  -- Get spec at PC 0.
  let initBCS = OCState { ocsEvalContext = ec
                        , ocsResultState = ps
                        , ocsReturnValue = Nothing
                        }
  let cont () = do
        OCState { ocsResultState = ps, ocsReturnValue = v } <- get
        return (SuccessfulRun ps v)
  fmap orParseResults $ forM bsl $ \bs ->
    flip evalStateT initBCS $ flip runContT cont $ do
       -- Check that all expressions that reference each other may do so.
       do -- Build map from references to expressions that map to them.
          let exprs = bsRefExprs bs
          refs <- ocEval $ \ec -> mapM (flip evalJavaRefExpr ec) exprs
          let refExprMap = Map.fromListWith (++) $ refs `zip` [[e] | e <- exprs]
          --- Get counterexamples.
          let mayAliasSet = bsMayAliasSet bs
          let badPairs = catMaybes
                       $ map (\exprs -> CC.checkEquivalence exprs mayAliasSet)
                       $ Map.elems refExprMap
          -- Throw error if counterexample is found.
          case badPairs of
            [] -> return ()
            (x,y):_ -> ocError (AliasingInputs x y)
       -- Execute statements.
       mapM_ ocStep (bsCommands bs)

checkClassesInitialized :: Pos -> String -> [String] -> JSS.Simulator SymbolicMonad ()
checkClassesInitialized pos specName requiredClasses = do
  forM_ requiredClasses $ \c -> do
    status <- JSS.getInitializationStatus c
    when (status /= Just JSS.Initialized) $
      let msg = "The method spec " ++ specName ++ " requires that the class "
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
  let ec = EvalContext { ecDagEngine = de
                       , ecThis = mbThis
                       , ecArgs = V.map Just $ V.fromList args
                       , ecPathState = initPS
                       }
  -- Check class initialization.
  checkClassesInitialized pos (specName ir) (specInitializedClasses ir)
  -- Execute behavior.
  ps <- JSS.getPathState
  res <- liftIO $ execBehavior pos ir bsl ec ps
  when (null res) $ error "internal: execBehavior returned empty result list."
  -- Create function for generation resume actions.
  let -- Failed run
      resAction (ps, Left e) =
        JSS.CustomRA ("Override behavior failed: " ++ ppOverrideError e) $ do
          JSS.putPathState ps { JSS.finalResult = JSS.Aborted }
          return $ JSS.NextInst
      resAction (ps, Right mval) =
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
         -- | Inputs to expected value in order they appeared.
         esdInputs :: [InputDescriptor Node]
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

{-
-- | Create a expected state definition from method spec and eval state.
expectedStateDef :: DagEngine Node Lit
                 -> MethodSpecIR
                 -> JavaVerificationState
                 -> JavaEvalContext Node
                 -> JSS.FinalResult Node
                 -> ExpectedStateDef
expectedStateDef de ir jvs jec fr = do
  ESD { esdAssumptions = methodAssumptions de ir sec
      , esdReturnValue = mixedExprValue de sec <$> returnValue ir
      , esdInstanceFields = Map.fromList
          [ ( (ref,fid)
            , case cond of
                PostUnchanged -> instanceFieldValue ref fid (jecPathState jec)
                PostArbitrary _ -> Nothing -- arbitrary case
                PostResult expr -> Just $ mixedExprValue de sec expr
            )
          | (refExpr,fid,cond) <- instanceFieldPostconditions spec
          , let Just (JSS.RValue ref) = evalJavaExpr jec refExpr
          ]
      , esdArrays = Map.fromList
          [ ( r
            , case mapLookupAny refEquivClass (arrayPostconditions spec) of
                Nothing -> Just initValue
                Just PostUnchanged -> Just initValue
                Just (PostArbitrary _) -> Nothing
                Just (PostResult (LE expr)) -> Just $ evalLogicExpr de sec expr
                Just (PostResult (JE _)) -> error "internal: illegal post result for array"
            )
          | (r,refEquivClass,initValue) <- jvsArrayNodeList jvs ]
      }
 where sec = createSpecEvalContext de ir jec
       spec = case fr of
                JSS.Breakpoint bpc ->
                  case Map.lookup bpc (localSpecs ir) of
                    Nothing -> error $
                               "internal: no intermediate specifications for pc " ++
                               show bpc
                    Just s -> s
                _ -> returnSpec ir
                -}

-- Initial state generation {{{1

-- | State for running the behavior specifications in a method override.
data VerifyState t l = VerifyState {
         vsDagEngine :: DagEngine t l
       , vsMethod :: JSS.Method
       , vsExprRefMap :: Map TC.JavaExpr JSS.Ref
       , vsInputs :: [InputDescriptor t] -- ^ Inputs created initially (in reverse order).
       , vsInitialPathState :: JSS.PathState t
       , vsReturnValue :: Maybe (JSS.Value t)
       , vsInstanceFields :: Map (JSS.Ref, JSS.FieldId) (Maybe (JSS.Value t))
       , vsArrays :: Map JSS.Ref (Maybe (Int32, t))
       }

-- | Monad used to execute statements in a behavior specification for a method
-- override. 
type VerifyStateGenerator t l = StateT (VerifyState t l) IO

vsEval :: (EvalContext t l -> ExprEvaluator b) -> VerifyStateGenerator t l b
vsEval fn = do
  de <- gets vsDagEngine
  initPS <- gets vsInitialPathState
  let ec = evalContextFromPathState de initPS
  case runEval (fn ec) of
    Left expr -> error $ "internal: Could not evaluate " ++ show expr ++ "."
    Right v -> return v

getInitialPathState :: VerifyStateGenerator t l (JSS.PathState t)
getInitialPathState = gets vsInitialPathState

putInitialPathState :: JSS.PathState t -> VerifyStateGenerator t l ()
putInitialPathState ps = modify $ \vs -> vs { vsInitialPathState = ps }

modifyInitialPathState :: (JSS.PathState t -> JSS.PathState t) -> VerifyStateGenerator t l ()
modifyInitialPathState fn =
  modify $ \vs -> vs { vsInitialPathState = fn (vsInitialPathState vs) }

-- | Assert that two terms are equal.
vsAssertEq :: TypedTerm t
           => JSS.Value t -> JSS.Value t -> VerifyStateGenerator t l ()
vsAssertEq (JSS.RValue x) (JSS.RValue y) = do
  when (x /= y) $
    error "internal: Asserted different references are equal."
vsAssertEq (JSS.IValue x) (JSS.IValue y) = do
  de <- gets vsDagEngine
  modifyInitialPathState $ addAssertion (deEq de x y)
vsAssertEq (JSS.LValue x) (JSS.LValue y) = do
  de <- gets vsDagEngine
  modifyInitialPathState $ addAssertion (deEq de x y)
vsAssertEq _ _ = error "internal: vsAssertEq given illegal arguments."

vsSetLocal :: TypedTerm t
           => JSS.LocalVariableIndex -> JSS.Value t -> VerifyStateGenerator t l ()
vsSetLocal idx v = do
  ps <- getInitialPathState
  let f:r = JSS.frames ps
  case Map.lookup idx (JSS.frmLocals f) of
    Just oldValue -> vsAssertEq oldValue v
    Nothing ->
      putInitialPathState ps {
          JSS.frames = f { JSS.frmLocals = Map.insert idx v (JSS.frmLocals f) }:r
        }

vsLookupRef :: TC.JavaExpr -> VerifyStateGenerator t l (Maybe JSS.Ref)
vsLookupRef expr = Map.lookup expr <$> gets vsExprRefMap

-- | Set value in initial state.
vsSetJavaValue :: TypedTerm t
               => TC.JavaExpr -> JSS.Value t -> VerifyStateGenerator t l ()
vsSetJavaValue (CC.Term expr) v = do
  case expr of
    TC.This _ -> vsSetLocal 0 v
    TC.Arg i _ -> do
      method <- gets vsMethod
      vsSetLocal (JSS.localIndexOfParameter method i) v
    TC.Local _ idx _ -> do
      vsSetLocal idx v
    TC.InstanceField refExpr f -> do
      -- Lookup refrence associated to refExpr
      Just ref <- vsLookupRef refExpr
      ps <- getInitialPathState
      case Map.lookup (ref,f) (JSS.instanceFields ps) of
        Just oldValue -> vsAssertEq oldValue v
        Nothing -> return ()
      putInitialPathState ps {
         JSS.instanceFields = Map.insert (ref,f) v (JSS.instanceFields ps)
       }

-- | Set logic value in initial state.
vsSetLogicValue :: TypedTerm t => TC.JavaExpr -> t -> VerifyStateGenerator t l ()
vsSetLogicValue expr value
  | TC.isRefJavaExpr expr = do
      JSS.RValue ref <- vsEval $ evalJavaExpr expr
      let SymArray (widthConstant -> Just (Wx l)) _ = termType value
      amap <- gets (JSS.arrays . vsInitialPathState)
      case Map.lookup ref amap of
        Just (oldLen,oldValue) ->
          assert (oldLen == fromIntegral l) $ do
            de <- gets vsDagEngine
            modifyInitialPathState $ addAssertion (deEq de oldValue value)
        Nothing -> return ()
      modifyInitialPathState $ \ps -> ps {
          JSS.arrays = Map.insert ref (fromIntegral l, value) (JSS.arrays ps)
        }
  | otherwise = do
      case termType value of
        SymInt (widthConstant -> Just 32) -> vsSetJavaValue expr (JSS.IValue value)
        SymInt (widthConstant -> Just 64) -> vsSetJavaValue expr (JSS.LValue value)
        _ -> error "internal: mixedExprValue called with malformed result type."

vsStep :: (TypedTerm t, SV.Storable l)
       => BehaviorCommand -> VerifyStateGenerator t l ()
vsStep (InputValue expr tp) = do
  de <- gets vsDagEngine
  -- Get number of literals.
  lc <- liftIO $ beInputLitCount (deBitEngine de)
  t <- liftIO $ mkInputWithType de tp
  -- Record input in vsInputs
  let desc = InputDescriptor {
               viExpr = expr
             , viNode = t
             , viEval = \bits -> mkInputEval tp $ SV.slice lc (typeBitCount tp) bits
             }
  modify $ \vs -> vs {vsInputs = desc:vsInputs vs }
  vsSetLogicValue expr t
vsStep (AssertValue _ lhs rhs) = do
  de <- gets vsDagEngine
  t <- vsEval $ evalLogicExpr rhs
  vsSetLogicValue lhs t
vsStep (AssertPred _ expr) = do
  de <- gets vsDagEngine
  v <- vsEval $ evalLogicExpr expr
  modifyInitialPathState $ addAssumption de v
vsStep (AssumePred expr) = do
  de <- gets vsDagEngine
  v <- vsEval $ evalLogicExpr expr
  modifyInitialPathState $ addAssumption de v
vsStep (Return expr) = do
  v <- vsEval $ evalMixedExpr expr
  modify $ \vs -> vs { vsReturnValue = Just v }
vsStep (EnsureInstanceField _pos refExpr f rhsExpr) = do
  -- Evaluate expressions.
  ref <- vsEval $ evalJavaRefExpr refExpr
  value <- vsEval $ evalMixedExpr rhsExpr
  -- Get dag engine
  de <- gets vsDagEngine 
  -- Check that instance field is already defined, if so add an equality check for that.
  ifMap <- gets vsInstanceFields
  case (Map.lookup (ref, f) ifMap, value) of
    (Nothing, _) -> return ()
    (Just Nothing, _) -> return ()
    (Just (Just (JSS.RValue prev)), JSS.RValue new)
      | prev == new -> return ()
    (Just (Just (JSS.IValue prev)), JSS.IValue new) -> 
       modifyInitialPathState $ addAssertion (deEq de prev new)
    (Just (Just (JSS.LValue prev)), JSS.LValue new) -> 
       modifyInitialPathState $ addAssertion (deEq de prev new)
    -- TODO: See if we can give better error message here.
    -- Perhaps this just ends up meaning that we need to verify the assumptions in this
    -- behavior are inconsistent.
    _ -> error "internal: Incompatible values assigned to instance field."
  -- Define instance field post condition.
  modify $ \vs ->
    vs { vsInstanceFields = Map.insert (ref,f) (Just value) (vsInstanceFields vs) }
vsStep (ModifyInstanceField refExpr f) = do
  -- Evaluate expressions.
  ref <- vsEval $ evalJavaRefExpr refExpr
  vs <- get
  -- Add postcondition if value has not been assigned.
  when (Map.notMember (ref, f) (vsInstanceFields vs)) $ do
    put vs { vsInstanceFields = Map.insert (ref,f) Nothing (vsInstanceFields vs) }
vsStep (EnsureArray _pos lhsExpr rhsExpr) = do
  -- Evaluate expressions.
  ref    <- vsEval $ evalJavaRefExpr lhsExpr
  value  <- vsEval $ evalLogicExpr rhsExpr
  let SymArray (widthConstant -> Just (Wx l)) _ = termType value
  -- Get dag engine
  de <- gets vsDagEngine 
  -- Check if array has already been assigned value.
  aMap <- gets vsArrays
  case Map.lookup ref aMap of
    Just (Just (oldLen, prev))
      | oldLen == fromIntegral l ->
        modifyInitialPathState $ addAssertion (deEq de prev value)
        -- TODO: Check to make sure this error is avoidable.
        -- Need to make sure 
      | otherwise -> error "internal: Incompatible values assigned to array."
    _ -> return ()
  -- Define instance field post condition.
  modify $ \vs -> vs { vsArrays = Map.insert ref (Just (fromIntegral l, value)) (vsArrays vs) }
vsStep (ModifyArray refExpr _) = do
  ref <- vsEval $ evalJavaRefExpr refExpr
  vs <- get
  -- Add postcondition if value has not been assigned.
  when (Map.notMember ref (vsArrays vs)) $ do
    put vs { vsArrays = Map.insert ref Nothing (vsArrays vs) }
  
type RefAssignment = (JavaExprEquivClass, JSS.Ref)

initializeVerification :: MethodSpecIR
                       -> BehaviorSpec
                       -> [RefAssignment]
                       -> JSS.Simulator SymbolicMonad ExpectedStateDef
initializeVerification ir bs exprRefs = do
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
      initVS = VerifyState { vsDagEngine = de
                           , vsMethod = specMethod ir
                           , vsExprRefMap = Map.fromList
                               [ (e, r) | (cl,r) <- exprRefs, e <- cl ]
                           , vsInputs = []
                           , vsInitialPathState = initPS
                           , vsReturnValue = Nothing
                           , vsInstanceFields = Map.empty
                           , vsArrays = Map.empty
                           }
  vs <- liftIO $ flip execStateT initVS $ do
          -- Set references
          forM_ exprRefs $ \(cl,r) ->
            forM_ cl $ \e -> vsSetJavaValue e (JSS.RValue r)
          -- Process commands
          mapM vsStep (bsCommands bs)
  let ps = vsInitialPathState vs
  JSS.putPathState ps
  return ESD { esdInputs = reverse (vsInputs vs)
             , esdRefExprMap =
                  Map.fromList [ (r, cl) | (cl,r) <- exprRefs ]
             , esdReturnValue = vsReturnValue vs
               -- Create esdArrays map while providing entry for unspecified expressions.
             , esdInstanceFields =
                 Map.union (vsInstanceFields vs)
                           (Map.map Just (JSS.instanceFields ps))
               -- Create esdArrays map while providing entry for unspecified expressions.
             , esdArrays =
                 Map.union (vsArrays vs)
                           (Map.map Just (JSS.arrays ps))
             }

-- MethodSpec verification {{{1

-- JavaVerificationState {{{2
type InputEvaluator = SV.Vector Bool -> CValue

data InputDescriptor n = InputDescriptor
  { viExpr  :: TC.JavaExpr      -- ^ Expression associated with input.
  , viNode  :: n                  -- ^ The node for the aliased group.
  , viEval  :: InputEvaluator     -- ^ Turn bit-patterns back into values.
  }

{-
-- | JavaVerificationState contains information necessary to map between specification
-- expressions and the initial JVM state during verification.
data JavaVerificationState = JVS {
        -- | Name of method in printable form.
        jvsMethodName :: String
        -- | Maps Spec Java expression to value for that expression.
      , jvsExprValueMap :: Map TC.JavaExpr (JSS.Value Node)
        -- | Contains inputs to Java verifiction in reverse order that they appear.
      , jvsInputs :: [InputDescriptor Node]
        -- | Maps JSS refs to name for that ref.
      , jvsRefExprMap :: Map JSS.Ref [TC.JavaExpr]
      }

-- | Returns name of reference in a state.
getRefName :: Monad m => Pos -> JSS.Ref -> JavaVerificationState -> m String
getRefName pos r jvs = do
  case Map.lookup r (jvsRefNameMap jvs) of
    Nothing ->
      let msg = "The JVM method \'" ++ jvsMethodName jvs ++ "\' has allocated "
                ++ "a new reference.  JavaVerifier does not currently support "
                ++ "methods that allocate new references."
       in throwExecException pos (ftext msg) ""
    Just e -> return e

type JavaEvaluator = StateT JavaVerificationState (JSS.Simulator SymbolicMonad)
-}

-- initializeJavaVerificationState {{{3

{-
-- EquivClassMap {{{2
type EquivClassMap = (Int, Map Int [TC.JavaExpr], Map Int TC.JavaActualType)

-- | Return list of class indices to initial values.
equivClassMapEntries :: EquivClassMap
                     -> V.Vector (Int, [TC.JavaExpr], TC.JavaActualType)
equivClassMapEntries (_,em,vm)
  = V.map (\(i,v) -> (i,em Map.! i,v))
  $ V.fromList $ Map.toList vm

-- | Initialize JavaVerificationState components involving references,
-- and create them in simulator.
createJavaEvalReferences :: EquivClassMap -> JavaEvaluator ()
createJavaEvalReferences cm = do
  de <- lift $ JSS.liftSymbolic $ getDagEngine
  V.forM_ (equivClassMapEntries cm) $ \(_idx, exprClass, initValue) -> do
    litCount <- liftIO $ beInputLitCount (deBitEngine de)
    let refName = ppJavaExprEquivClass exprClass
    let firstExpr:_ = exprClass
    case initValue of
      TC.ClassInstance cl -> do
        ref <- lift $ JSS.genRef (ClassType (className cl))
        modify $ \s -> s
          { jvsExprValueMap = mapInsertKeys exprClass (JSS.RValue ref) (jvsExprValueMap s)
          , jvsRefNameMap = Map.insert ref refName (jvsRefNameMap s)
          }
      TC.ArrayInstance l javaEltType (Just v) -> do
        let n = deConstantTerm de v (jssArrayDagType l javaEltType)
        ref <- lift $ JSS.newSymbolicArray (JSS.ArrayType javaEltType) (fromIntegral l) n
        modify $ \s -> s
          { jvsExprValueMap = mapInsertKeys exprClass (JSS.RValue ref) (jvsExprValueMap s)
          , jvsRefNameMap = Map.insert ref refName (jvsRefNameMap s)
          , jvsArrayNodeList = (ref,exprClass,n):(jvsArrayNodeList s)
          }
      TC.ArrayInstance l javaEltType Nothing -> do
        -- create array input node with length and int width.
        let w = jssArrayEltWidth javaEltType
        -- Create input array node.
        n <- liftIO $ do
          let ?be = deBitEngine de
          lv <- V.replicateM l $ LV <$> SV.replicateM w lMkInput
          deFreshInput de (Just (LVN lv)) (jssArrayDagType l javaEltType)
        -- Create Java reference for creating array node.
        ref <- lift $ JSS.newSymbolicArray (JSS.ArrayType javaEltType) (fromIntegral l) n
        -- Create input evaluator.
        let inputEval lits =
              CArray $ V.map (\j -> mkCIntFromLsbfV $ SV.slice j w lits)
                     $ V.enumFromStepN litCount w (fromIntegral l)
        modify $ \s ->
          s { jvsExprValueMap = mapInsertKeys exprClass (JSS.RValue ref) (jvsExprValueMap s)
            , jvsInputs =
                InputDescriptor
                   { viExpr = firstExpr
                   , viNode = n
                   , viEval = inputEval
                   } : jvsInputs s
            , jvsRefNameMap = Map.insert ref refName (jvsRefNameMap s)
            , jvsArrayNodeList = (ref,exprClass,n):(jvsArrayNodeList s) }
            -}

{-
createJavaEvalScalar :: TC.JavaExpr -> JavaEvaluator ()
createJavaEvalScalar expr = do
  de <- lift $ JSS.liftSymbolic $ getDagEngine
  litCount <- liftIO $ beInputLitCount (deBitEngine de)
  let exprType = TC.jssTypeOfJavaExpr expr
  let w = JSS.stackWidth exprType
  n <- liftIO $ mkIntInput de w
  let v = mkJSSValue exprType n
  modify $ \s ->
    s { jvsExprValueMap = Map.insert expr v (jvsExprValueMap s)
      , jvsInputs =
          InputDescriptor
             { viNode = n
             , viEval = \lits -> mkCIntFromLsbfV (SV.slice litCount w lits)
             , viExpr = expr
             } : jvsInputs s
      }
      -}
 
{-
-- | Initialize the Java verification state.
initializeJavaVerificationState :: MethodSpecIR
                                -> EquivClassMap
                                -> JSS.Simulator SymbolicMonad JavaVerificationState
initializeJavaVerificationState ir cm = do
  let initialState = JVS
        { jvsMethodName = specName ir
        , jvsExprValueMap = Map.empty
        , jvsInputs = []
        , jvsRefNameMap = Map.empty
        , jvsArrayNodeList = []
        }
  flip execStateT initialState $ do
    -- Add initialized classes
    lift $ forM_ (initializedClasses ir) $ \c -> do
      JSS.setInitializationStatus c JSS.Initialized
    -- Create references.
    createJavaEvalReferences cm
    -- Create scalars.
    forM_ (specJavaExprs ir) $ \expr -> do
      case TC.jssTypeOfJavaExpr expr of
        JSS.BooleanType -> createJavaEvalScalar expr
        JSS.ByteType    -> createJavaEvalScalar expr
        JSS.CharType    -> createJavaEvalScalar expr
        JSS.IntType     -> createJavaEvalScalar expr
        JSS.LongType    -> createJavaEvalScalar expr
        JSS.ShortType   -> createJavaEvalScalar expr
        _ -> return ()
    -- Set field values.
    m <- gets jvsExprValueMap
    lift $ sequence_ [ let Just (JSS.RValue r) = Map.lookup re m
                        in JSS.setInstanceFieldValue r f v
                     | (TC.InstanceField re f, v) <- Map.toList m ]
                     -}

-- Java execution {{{2

{-
-- Run method and get final path state, along with the evaluation
-- context describing the initial state after all initial ensures
-- clauses applied.
runMethod :: MethodSpecIR
          -> SpecEvalContext Node
          -> JSS.Simulator SymbolicMonad
             ( [(JSS.PathDescriptor, JSS.FinalResult Node)]
             , JavaEvalContext Node
             )
runMethod ir sec = do
  let jec = secJavaEvalContext sec
  let clName = className (specMethodClass ir)
  let method = specMethod ir
  let args = V.toList (jecArgs jec)
  let pc = jecInitPC jec
  if methodIsStatic method then
    JSS.invokeStaticMethod clName (methodKey method) args
    JSS.pushFrame clName method (RValue objectref : operands)

  else
    let Just thisRef = jecThis jec
     in JSS.invokeInstanceMethod clName (methodKey method) thisRef args
  when (pc /= 0) $ do
    let pc' = nextPc method pc
    Sem.setPc pc'
  -- Update the starting state with any 'ensures' located at the
  -- starting PC.
  --
  -- NB: eventually we'll do this, but only once we've settled on a
  -- state semantics for MethodSpecs.
  forM_ (Map.toList $ arrayPostconditions specs) $ \(javaExpr,post) ->
    evalArrayPost de ssi javaExpr post
  forM_ (instanceFieldPostconditions specs) $ \(refExpr,f,post) ->
    evalInstanceFieldPost ssi refExpr f post
  -- Retrieve the path state after these updates and build a new
  -- JEC based on it.
  newPS <- JSS.getPathState
  let jec' = jec { jecPathState = newPS }
  res <- JSS.run
  return (res, jec')
  -}

-- VerificationCheck {{{2

data VerificationCheck n
  = AssertionCheck n
  -- | Check that equalitassertion is true.
  | EqualityCheck String -- ^ Name of value to compare
                  n -- ^ Value returned by JVM symbolic simulator.
                  n -- ^ Expected value in Spec.
  deriving (Eq, Ord, Show)

{-
-- | Returns goal that one needs to prove.
checkGoal :: DagEngine Node Lit -> VerificationCheck Node -> Node
checkGoal _ (PathCheck n) = n
checkGoal de (EqualityCheck _ c x y) =
  deApplyBinary de bImpliesOp c $ deApplyBinary de (eqOp (termType x)) x y

checkName :: VerificationCheck Node -> String
checkName (PathCheck _) = "the path condition"
checkName (EqualityCheck nm _ _ _) = nm

-- | Returns documentation for check that fails.
checkCounterexample :: VerificationCheck Node -> (Node -> CValue) -> Doc
checkCounterexample (PathCheck n) evalFn =
  text "The path conditions were unsatisfied:" <+> prettyTermD n
checkCounterexample (EqualityCheck nm _cond jvmNode specNode) evalFn =
  text nm $$
    nest 2 (text "Encountered: " <> ppCValueD Mixfix (evalFn jvmNode)) $$
    nest 2 (text "Expected:    " <> ppCValueD Mixfix (evalFn specNode))
    -}

-- | Describes the verification result arising from one symbolic execution path.
data PathVC = PathVC {
          -- | Inputs to verification problem (universally quantified).
          pvcInputs :: [InputDescriptor Node]
          -- | Assumptions on inputs (in reverse order).
        , pvcAssumptions :: Node
          -- | Static errors found in path.         
        , pvcStaticErrors :: [Doc]
          -- | What to verify for this result.
        , pvcChecks :: [VerificationCheck Node]
        }

type PathVCGenerator = State PathVC

runPathVCG :: [InputDescriptor Node] -- ^ Inputs 
           -> Node -- ^ Assumptions
           -> PathVCGenerator () -- ^ Generator
           -> PathVC
runPathVCG i a m =
  execState m
            PathVC { pvcInputs = i
                   , pvcAssumptions = a
                   , pvcStaticErrors = []
                   , pvcChecks = [] 
                   }

-- | Add verification condition to list.
pvcgAssertEq :: String -> Node -> Node -> PathVCGenerator ()
pvcgAssertEq name jv sv  =
  modify $ \pvc -> pvc { pvcChecks = EqualityCheck name jv sv : pvcChecks pvc }

pvcgAssert :: String -> Node -> PathVCGenerator ()
pvcgAssert name v =
  modify $ \pvc -> pvc { pvcChecks = AssertionCheck v : pvcChecks pvc }

pvcgFail :: Doc -> PathVCGenerator ()
pvcgFail msg =
  modify $ \pvc -> pvc { pvcStaticErrors = msg : pvcStaticErrors pvc }

-- | Compare result with expected state.
generateVC :: MethodSpecIR
           -> ExpectedStateDef -- ^ What is expected
           -> RunResult Node -- ^ Results of symbolic execution.
           -> PathVC -- ^ Proof oblications
generateVC ir esd (ps, res) =
  runPathVCG (esdInputs esd) (JSS.psAssumptions ps) $ do
    let pos = specPos ir
        nm = show (specName ir)
    case res of
      Left oe -> pvcgFail (ftext (ppOverrideError oe))
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
          let fieldName = show (JSS.fieldIdName f) ++ " of " ++ esdRefName ref esd
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
        forM_ (JSS.psAssertions ps) $ \n -> do
          pvcgAssert "path assertion" n
        -- Check class objects
        forM_ (Map.keys (JSS.classObjects ps)) $ \clNm ->
          pvcgFail $ ftext $ "Allocated class object for " ++ slashesToDots clNm ++ "."

-- verifyMethodSpec and friends {{{2

-- | Analyze the specification, and return a list of symbolic executions to run
-- that will generate the different paths we need to verify.
specVCs :: VerifyParams -> [SymbolicMonad [PathVC]]
specVCs
  params@(VerifyParams
    { vpPos = pos
    , vpCode = cb
    , vpOpts = opts
    , vpOver = overrides
    , vpSpec = ir
    }
  ) = do
  let vrb = verbose opts
  -- Get list of behavior specs to start from and the equivclass for them.
  let executionParams = [ (bs,cl) | bs <- concat $ Map.elems $ specBehaviors ir
                                  , cl <- bsRefEquivClasses bs]
  -- Return executions
  flip map executionParams $ \(bs, refClasses) -> do
    de <- getDagEngine
    setVerbosity vrb
    JSS.runSimulator cb $ do
      -- Log execution.
      setVerbosity vrb
      -- Create map from exprsssions to references.
      exprRefs <- mapM (JSS.genRef . jssTypeOfActual . snd) refClasses
      -- Add method spec overrides.
      mapM_ (overrideFromSpec pos) overrides
      -- Create initial Java state.
      esd <- initializeVerification ir bs (map fst refClasses `zip` exprRefs)
      -- Execute code.
      when (vrb >= 3) $ do
        liftIO $ putStrLn $ "Executing " ++ specName ir ++ " at PC " ++ show (bsPC bs) ++ "."
      jssResults <- JSS.run
      finalPathResults <-
        forM jssResults $ \(pd, fr) -> do
          finalPS <- JSS.getPathStateByName pd
          case fr of
            JSS.ReturnVal val -> return [(finalPS, Right (Just val))]
            JSS.Terminated ->    return [(finalPS, Right Nothing)]
            JSS.Breakpoint pc -> do
              -- Execute behavior specs at PC.
              let Just bsl = Map.lookup pc (specBehaviors ir)
              let ec = evalContextFromPathState de finalPS
              liftIO $ execBehavior pos ir bsl ec finalPS
            JSS.Exc JSS.SimExtErr { JSS.simExtErrMsg = msg } ->
              return [(finalPS, Left (SimException msg)) ]
            JSS.Exc JSS.JavaException{ JSS.excRef = r } ->
              return [(finalPS, Left (JavaException r))]
            JSS.Aborted ->
              return [(finalPS, Left Abort)]
            JSS.Unassigned -> error "internal: run terminated before completing."
      return $ map (generateVC ir esd) (concat finalPathResults)

{-
  let refEquivClasses = 
  let assertPCs = Map.keys (localSpecs ir)
  let cls = JSS.className $ specThisClass ir
  let meth = JSS.methodKey $ specMethod ir
  flip concatMap refEquivClasses $ \cm -> flip map (0 : assertPCs) $ \pc -> do
    de <- getDagEngine
      -- Create map from specification entries to JSS simulator values.
      jvs <- initializeJavaVerificationState ir cm
      -- JavaEvalContext for inital verification state.
      initialPS <- JSS.getPathState
      let jec' =
            let evm = jvsExprValueMap jvs
                mbThis = case Map.lookup (TC.This cls) evm of
                           Nothing -> Nothing
                           Just (JSS.RValue r) -> Just r
                           Just _ -> error "internal: Unexpected value for This"
                method = specMethod ir
             in EvalContext {
                    jecThis = mbThis
                  , jecArgs = V.map (evm Map.!)
                            $ V.map (uncurry TC.Arg)
                            $ V.fromList
                            $ [0..] `zip` methodParameterTypes method
                  , jecPathState = initialPS
                  , jecInitPC = pc
                  }
      let sec' = createSpecEvalContext de ir jec'
          -- isReturn returns True if result is a normal return value.
      let isReturn JSS.ReturnVal{} = True
          isReturn JSS.Terminated = True
          isReturn _ = False
      let isExpected JSS.Breakpoint{} = True
          isExpected fr = isReturn fr
      let finalResults = filter (isExpected . snd) jssResult
      let returnResults = filter (isReturn . snd) jssResult
      when (null finalResults) $
        let msg = "The Java method " ++ specName ir
              ++ " throws exceptions on all paths, and cannot be verified"
            res = "Please check that all fields needed for correctness are defined."
         in throwIOExecException pos (ftext msg) res
      when (length returnResults > 1) $
        error "internal: verifyMethodSpec returned multiple valid results"
      forM finalResults $ \(ps, fr) -> do
        let returnVal = case fr of
                          JSS.ReturnVal val -> Just val
                          JSS.Terminated -> Nothing
                          JSS.Breakpoint{} -> Nothing
                          _ -> error "internal: Unexpected final result from JSS"
        let name = case fr of
                     (JSS.Breakpoint bpc) ->
                       specName ir ++
                       "[" ++ show pc ++ "->" ++ show bpc ++ "]"
                     _ -> case pc of
                            0 -> specName ir
                            _ -> specName ir ++
                                 "[" ++ show pc ++ "->end]"
        -- Build final equation and functions for generating counterexamples.
        newPathState <- JSS.getPathStateByName ps
        JSS.liftSymbolic $ do
          let esd = expectedStateDef de ir jvs jec fr
          liftIO $ when (vrb >= 6) $
            putStrLn $ "Creating verification conditions for " ++ name
          -- Create verification conditions from path states.
          let vcs = comparePathStates ir jvs esd newPathState returnVal
          return PathVC {
                     pvcAssumptions = esdAssumptions esd
                   , pvcInputs = reverse (jvsInputs jvs)
                   , pvcChecks = vcs
                   , erEnabled = vpEnabledOps params
                   }
                   -}

{-
runABC :: DagEngine Node Lit
       -> Int -- ^ Verbosity
       -> MethodSpecIR
       -> [InputDescriptor]
       -> VerificationCheck
       -> Node
       -> IO ()
runABC de v ir inputs vc goal = do
  when (v >= 3) $ do
    putStrLn $ "Running ABC on " ++ specName ir
    putStrLn $ "Goal is:"
    putStrLn $ prettyTerm goal
  let LV value = deBitBlast de goal
  unless (SV.length value == 1) $
    error "internal: Unexpected number of in verification condition"
  let be = deBitEngine de
  case beCheckSat be of
    Nothing -> error "internal: Bit engine does not support SAT checking."
    Just checkSat -> do
      b <- checkSat (beNeg be (value SV.! 0))
      case b of
        UnSat -> when (v >= 3) $ putStrLn "Verification succeeded."
        Unknown -> do
          let msg = "ABC has returned a status code indicating that it could not "
                     ++ "determine whether the specification is correct.  This "
                     ++ "result is not expected for sequential circuits, and could"
                     ++ "indicate an internal error in ABC or JavaVerifer's "
                     ++ "connection to ABC."
           in throwIOExecException (specPos ir) (ftext msg) ""
        Sat lits -> do
          -- Get doc showing inputs
          let docInput vi = map (\e -> text (show e) <+> equals <+> ppCValueD Mixfix c)
                                (viExpr vi)
                where c = viEval vi lits
          let inputDocs = concatMap docInput inputs
          -- Get differences between two.
          let inputValues = V.map (\vi -> viEval vi lits) (V.fromList inputs)
          diffDoc <- checkCounterexample vc <$> deConcreteEval inputValues
          let msg = ftext ("A counterexample was found by ABC when verifying "
                             ++ specName ir ++ ".\n\n") $$
                    ftext ("The inputs that generated the counterexample are:") $$
                    nest 2 (vcat inputDocs) $$
                    ftext ("Counterexample:") $$
                    nest 2 diffDoc
          throwIOExecException (specPos ir) msg ""
          -}

data VerifyParams = VerifyParams
  { vpOpCache :: OpCache
  , vpPos     :: Pos
  , vpCode    :: JSS.Codebase
  , vpOpts    :: SSOpts
  , vpSpec    :: MethodSpecIR
  , vpOver    :: [MethodSpecIR]
  , vpRules   :: [Rule]
  , vpEnabledOps  :: Set OpIndex
  }

-- | Attempt to verify method spec using verification method specified.
validateMethodSpec :: VerifyParams -> IO ()
validateMethodSpec
    params@VerifyParams { vpOpCache = oc
                        , vpPos = _pos
                        , vpOpts = opts
                        , vpSpec = ir
                        , vpRules = rules
                        } = do
  let verb = verbose opts
  when (verb >= 2) $ putStrLn $ "Starting verification of " ++ specName ir
  let vcList = specVCs params
  forM_ vcList $ \vcGenerator -> do
    runSymbolic oc $ do
      results <- vcGenerator
      forM_ results $ \pvc -> do
        --TODO: Generate goals for verification.
        goals <- 
          if null (pvcStaticErrors pvc) then
            nyi "Not yet implemented: validateMethodSpec"
          else
            forM (pvcChecks pvc) $ \c ->
              nyi "Not yet implemented: validateMethodSpec"
        nyi "Not yet implemented: validateMethodSpec" goals


{- TODO: Fix below
  let pgm = foldl' addRule emptyProgram rules

  forM_ vcList $ \mVC -> do
    when (v >= 6) $
      liftIO $ putStrLn $ "Considering new alias configuration of " ++ specName ir
    runSymbolic oc $ do
      setVerbosity v
      de <- getDagEngine
      vcs <- mVC

      when (v >= 2) $ do
        termCnt <- length <$> liftIO (deGetTerms de)
        let txt = if length vcList > 1
                    then "(one alias configuration of) "
                    else ""
        dbugM ""
        dbugM $ "Completed simulation of " ++ txt ++ methodSpecName ir ++ "."
        when (v >= 3) $ do
          dbugM $ "Term DAG has " ++ show termCnt ++ " application nodes."

      let goal vc check = deApplyBinary de bImpliesOp (vcAssumptions vc) (checkGoal de check)
          gs vc = (vc, map (\check -> (check, goal vc check)) (vcChecks vc))
          allGoals = map gs vcs
      foldM_ (applyTactic v ir) (pgm, allGoals) (methodSpecVerificationTactics ir)

type TacticContext = [(VerificationContext, [(VerificationCheck, Node)])]

applyTactic :: Int -- ^ Verbosity
            -> MethodSpecIR
            -> (RewriteProgram Node, TacticContext)
            -> AST.VerificationTactic
            -> SymbolicMonad (RewriteProgram Node, TacticContext)
applyTactic v ir (pgm, gs) AST.Rewrite = do
  de <- getDagEngine
  rew <- liftIO $ mkRewriter pgm (deTermSemantics de)
  gs' <- forM gs $ \(vc, chks) -> liftIO $ do
    chks' <- forM chks $ \(check, goal) -> do
      when (v >= 2) $ putStrLn $ "Verify " ++ checkName check
      goal' <- reduce rew goal
      return (check, goal')
    return (vc, chks')
  return (pgm, gs')
applyTactic v ir (pgm, gs) AST.ABC = do
  de <- getDagEngine
  liftIO $ forM_ gs $ \(vc, chks) -> forM_ chks $ \(check, goal) -> do
    when (v >= 2) $ putStrLn $ "Verify " ++ checkName check
    runABC de v ir (vcInputs vc) check goal
  return (pgm, [])
applyTactic v ir (pgm, gs) (AST.QuickCheck n lim) = do
  de <- getDagEngine
  liftIO $ forM_ gs $ \(vc, _) -> testRandom de v ir n lim vc
  return (pgm, [])
applyTactic _ ir (pgm, gs) (AST.SmtLib ver nm) = do
  let enabledOps = nyi "vpEnabledOps vp"
  forM_ gs $ \(vc, chks) -> useSMTLIB ir ver nm enabledOps vc (map snd chks)
  return (pgm, [])
applyTactic _ ir (pgm, gs) (AST.Yices ti) = do
  forM_ gs $ \(vc, chks) -> useYices ir ti vc (map snd chks)
  return (pgm, [])
applyTactic _ _ (pgm, gs) (AST.AddRule nm vars l r) = do
  error "addrule not yet implemented."
  -- TODO: we should really be type-checking the rules in advance, and
  -- store compiled rules appropriate for passing to the addRule
  -- function.
  let lt = undefined
      rt = undefined
      pgm' = addRule pgm (mkRule nm lt rt)
  return (pgm', gs)
applyTactic _ _ _ _ = error "internal: verifyMethodTactic used invalid tactic."

type Verbosity = Int

testRandom :: DagEngine Node Lit -> Verbosity
           -> MethodSpecIR -> Int -> Maybe Int -> PathVC -> IO ()
testRandom de v ir test_num lim vc =
    do when (v >= 3) $
         putStrLn $ "Generating random tests: " ++ specName ir
       (passed,run) <- loop 0 0
       when (passed < test_num) $
         let m = text "QuickCheck: Failed to generate enough good inputs."
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
    vs   <- mapM (QuickCheck.pickRandom . termType . viNode) (pvcInputs vc)
    eval <- deConcreteEval (V.fromList vs)
    if not (toBool $ eval $ pvcAssumptions vc)
      then return passed
      else do when (v >= 4) $
                dbugM $ "Begin concrete DAG eval on random test case for all goals ("
                        ++ show (length $ vcChecks vc) ++ ")."
              forM_ (vcChecks vc) $ \goal ->
                do let goal_ok = toBool (eval (checkGoal de goal))
                   when (v >= 4) $ dbugM "End concrete DAG eval for one VC check."
                   unless goal_ok $ do
                     (vs1,goal1) <- QuickCheck.minimizeCounterExample
                                            isCounterExample vs goal
                     throwIOExecException (methodSpecPos ir)
                                          (msg eval vs1 goal1) ""
              return $! passed + 1

  isCounterExample vs =
    do eval <- deConcreteEval (V.fromList vs)
       return $ do guard $ toBool $ eval $ vcAssumptions vc
                   find (not . toBool . eval . checkGoal de) (vcChecks vc)

  msg eval vs g =
      text "Random testing found a counter example:"
    $$ nest 2 (vcat
     [ text "Method:" <+> text (specName ir)
     , case g of
         EqualityCheck n _c x y ->
            text "Unexpected value for:" <+> text n
            $$ nest 2 (text "Expected:" <+> ppCValueD Mixfix (eval y)
                    $$ text "Found:"    <+> ppCValueD Mixfix (eval x))
         PathCheck _ -> text "Invalid path constraint."
     , text "Random arguments:"
       $$ nest 2 (vcat (zipWith ppInput (pvcInputs vc) vs))
     ])

  ppInput inp value =
    case viExpr inp of
      t -> text (show t) <+> text "=" <+> ppCValueD Mixfix value
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

announce :: String -> SymbolicMonad ()
announce msg = whenVerbosity (>= 3) (liftIO (putStrLn msg))

useSMTLIB :: MethodSpecIR -> Maybe Int -> Maybe String -> Set OpIndex
          -> PathVC -> [Node] -> SymbolicMonad ()
useSMTLIB ir mbVer mbNm enabledOps vc gs =
  announce ("Translating to SMTLIB (version " ++ show version ++"): "
                                        ++ specName ir) >> liftIO (
  do let params = SmtLib.TransParams
                    { SmtLib.transName = name
                    , SmtLib.transInputs = map (termType . viNode) (pvcInputs vc)
                    , SmtLib.transAssume = pvcAssumptions vc
                    , SmtLib.transCheck = gs
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
  )

  where
  name = case mbNm of
           Just x  -> x
           Nothing -> specName ir

  version :: Int
  (version, ver_exr) = case mbVer of
                         Just n | n /= 1 -> (n, show n)
                         _      -> (1, "")   -- For now, we use 1 by default.

useYices :: MethodSpecIR -> Maybe Int -> Set OpIndex
            PathVC -> [Node] -> SymbolicMonad ()
useYices ir mbTime enabledOps vc gs =
  announce ("Using Yices2: " ++ specName ir) >> liftIO (
  do (script,info) <- SmtLib.translate SmtLib.TransParams
        { SmtLib.transName = "CheckYices"
        , SmtLib.transInputs = map (termType . viNode) (pvcInputs vc)
        , SmtLib.transAssume = pvcAssumptions vc
        , SmtLib.transCheck = gs
        , SmtLib.transEnabled = enabledOps
        , SmtLib.transExtArr = True
        }

     res <- Yices.yices mbTime script
     case res of
       Yices.YUnsat   -> return ()
       Yices.YUnknown -> yiFail (text "Failed to decide property.")
       Yices.YSat m   ->
         yiFail ( text "Found a counter example:"
               $$ nest 2 (vcat $ intersperse (text " ") $
                    zipWith ppIn (pvcInputs vc) (map (Yices.getIdent m)
                                                      (SmtLib.trInputs info)))
               $$ text " "
               $$ ppUninterp m (SmtLib.trUninterp info)

               $$ ppArgHist info m
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
  )

  where
  yiFail xs = fail $ show $ vcat $
                [ text "Yices: Verification failed."
                , text "*** Method:" <+> text (specName ir)
                , text "*** Location:" <+> text (show (specPos ir))
                , text "*** Details:"
                , nest 2 xs
                ]

  ppIn vi val =
    case viExpr vi of
      x -> Yices.ppVal (show x, val)

  ppUninterp _ [] = empty
  ppUninterp m us =
    text "Uninterpreted functions:"
    $$ nest 2 (vcat $
      [ Yices.ppVal (s, Yices.getIdent m i) $$ text " " | (i,s) <- us ]
    )

  varName base 0    = base
  varName base time = base ++ "@" ++ show time

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

  ppArgHist info model =
    case zipWith (ppHist model (SmtLib.trArrays info))
                 (map (show . viExpr) (pvcInputs vc))
                 (SmtLib.trInputs info) of
      [] -> empty
      ds -> text "Final values for array arguments:"
         $$ nest 2 (vcat (intersperse (text " ") ds))
         -}
