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
  , methodSpecName
  , methodSpecIRMethodClass
  , methodSpecValidationPlan
  , resolveMethodSpecIR
  , validateMethodSpec
  , VerifyParams(..)
  ) where

-- Imports {{{1

import Control.Applicative hiding (empty)
import qualified Control.Exception as CE
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Error (Error(..), ErrorT, runErrorT, throwError)
import Control.Monad.Identity
import Control.Monad.State
import Data.Either (lefts, rights)
import Data.IORef
import Data.List (foldl', intersperse)
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

-- | Return value of field in path state.
instanceFieldValue :: JSS.Ref -> JSS.FieldId -> JSS.PathState Node -> Maybe (JSS.Value Node)
instanceFieldValue r f ps = Map.lookup (r,f) (JSS.instanceFields ps)

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
mkInputWithType :: (SV.Storable l) => DagEngine n l -> DagType -> IO n
mkInputWithType de tp = do
 r <- let ?be = deBitEngine de in mkInputLitResultWithType tp
 deFreshInput de (Just r) tp

-- JSS.Type utility functions {{{1

-- | Returns value constructor from node.
mkJSSValue :: JSS.Type -> n -> JSS.Value n
mkJSSValue JSS.BooleanType n = JSS.IValue n
mkJSSValue JSS.ByteType    n = JSS.IValue n
mkJSSValue JSS.CharType    n = JSS.IValue n
mkJSSValue JSS.IntType     n = JSS.IValue n
mkJSSValue JSS.LongType    n = JSS.LValue n
mkJSSValue JSS.ShortType   n = JSS.IValue n
mkJSSValue _ _ = error "internal: illegal type"

-- EvalContext {{{1

-- | Contextual information needed to evaluate expressions.
data EvalContext n l = EvalContext {
         ecDagEngine :: DagEngine n l
       , ecThis :: Maybe JSS.Ref
       , ecArgs :: V.Vector (JSS.Value n)
       , ecPathState :: JSS.PathState n
       }

type ExprEvaluator a = ErrorT TC.JavaExpr Identity a

instance Error TC.JavaExpr where
  noMsg = error "noMsg called with TC.JavaExpr"

-- | Returns value associated to Java expression in this state if it is defined,
-- or undefined subexpression if not.
-- N.B. This method assumes that the Java path state is well-formed, the
-- the JavaExpression syntactically referes to the correct type of method
-- (static versus instance), and correct well-typed arguments.  It does
-- not assume that all the instanceFields in the JavaEvalContext are initialized.
evalJavaExpr :: EvalContext n l -> TC.JavaExpr -> ExprEvaluator (JSS.Value n)
evalJavaExpr ec expr = eval expr
  where eval (TC.This _) =
          case ecThis ec of
            Just r -> return (JSS.RValue r)
            Nothing -> error "internal: evalJavaExpr given TC.This for static method"
        eval (TC.Arg i _) = assert (i < V.length args) $ return (args V.! i)
          where args = ecArgs ec
        eval (TC.Local _ _) = nyi "evalJavaExpr TC.Local"
        eval e@(TC.InstanceField r f) = do
          JSS.RValue ref <- eval r
          case Map.lookup (ref, f) (JSS.instanceFields (ecPathState ec)) of
            Just v -> return v
            Nothing -> throwError e

-- | Evaluates a typed expression.
evalLogicExpr :: EvalContext n l -> TC.LogicExpr -> ExprEvaluator n
evalLogicExpr ec initExpr = eval initExpr
  where de = ecDagEngine ec
        eval (TC.Apply op exprs) =
          deApplyOp de op <$> V.mapM eval (V.fromList exprs)
        eval (TC.Cns c tp) = return $ deConstantTerm de c tp
        eval (TC.Var _ _) = error "internal: evalLogicExpr called with var"
        eval (TC.JavaValue expr _) = do
          val <- evalJavaExpr ec expr
          case val of
            JSS.RValue r ->
              case Map.lookup r (JSS.arrays (ecPathState ec)) of
                Nothing    -> throwError expr
                Just (_,n) -> return n
            JSS.IValue n -> return n
            JSS.LValue n -> return n
            _ -> error "internal: evalLogicExpr failed to evaluated Java expression"

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

-- | Return Java value associated with mixed expression.
jssEvalMixedExpr :: TypedTerm n
                 => EvalContext n l -> TC.MixedExpr -> ExprEvaluator (JSS.Value n)
jssEvalMixedExpr ec expr = do
  res <- evalMixedExpr ec expr
  case res of
    MVRef r -> return (JSS.RValue r)
    MVNode n ->
      case termType n of
        SymInt (widthConstant -> Just 32) -> return (JSS.IValue n)
        SymInt (widthConstant -> Just 64) -> return (JSS.LValue n)
        _ -> error "internal: mixedExprValue called with malformed result type."

-- Method specification overrides {{{1
-- OverrideComputation definition {{{2

-- | State for running the behavior specifications in a method override.
data OCState n l = OCState {
         ocsEvalContext :: EvalContext n l
       , ocsResultState :: JSS.PathState n
       }

-- | Monad used to execute statements in a behavior specification for a method
-- override. 
type OverrideComputation n l = StateT (OCState n l) (ErrorT String IO)

-- OverrideComputation utilities {{{2

-- | Runs an evaluate within an override computation.
ocEval :: (EvalContext n l -> a -> ExprEvaluator b) -> a -> OverrideComputation n l b
ocEval fn v = do
  ec <- gets ocsEvalContext
  let res = runIdentity (runErrorT (fn ec v))
  case res of
    Left expr -> throwError $ "Could not evaluate " ++ show expr ++ "."
    Right v -> return v

modifyResultState :: (JSS.PathState n -> OverrideComputation n l (JSS.PathState n))
                  -> OverrideComputation n l ()
modifyResultState m = do
  bcs <- get
  ps <- m (ocsResultState bcs)
  put bcs { ocsResultState = ps }

-- | Add assumption for predicate.
addAssumption :: ConstantProjection n => n -> OverrideComputation n l ()
addAssumption x =
  case getBool x of
    Just True -> return ()
    Just False -> throwError "Assumption is false."
    _ -> modifyResultState $ \ps -> do
           de <- gets (ecDagEngine . ocsEvalContext)
           let res = deApplyBinary de bAndOp (JSS.psAssumptions ps) x
           return ps { JSS.psAssumptions = res }

addEqAssumption :: (ConstantProjection n, TypedTerm n)
                => n -> n -> OverrideComputation n l ()
addEqAssumption x y = do
  de <- gets (ecDagEngine . ocsEvalContext)
  addAssumption (deApplyBinary de (eqOp (termType x)) x y)

setArrayValue :: TypedTerm n => TC.JavaExpr -> n -> OverrideComputation n l ()
setArrayValue lhsExpr rhsVal = do
  JSS.RValue lhsRef <- ocEval evalJavaExpr lhsExpr
  let SymArray (widthConstant -> Just (Wx w)) _ = termType rhsVal
  modifyResultState $ \ps ->
    return ps { JSS.arrays =
                  Map.insert lhsRef (fromIntegral w, rhsVal) (JSS.arrays ps) }

setInstanceField :: TC.JavaExpr -> JSS.FieldId -> JSS.Value n -> OverrideComputation n l ()
setInstanceField refExpr fid value = do
  JSS.RValue lhsRef <- ocEval evalJavaExpr refExpr
  modifyResultState $ \ps ->
    return ps { JSS.instanceFields = 
                  Map.insert (lhsRef,fid) value (JSS.instanceFields ps) }

-- ocStep {{{2

ocStep :: (ConstantProjection n, TypedTerm n, SV.Storable l)
       => Statement -> OverrideComputation n l ()
ocStep (AssertPred expr) =
  addAssumption =<< ocEval evalLogicExpr expr
ocStep (AssertValueEq lhs rhs) = do
  lhsVal <- ocEval evalMixedExpr (JE lhs)
  rhsVal <- ocEval evalMixedExpr rhs
  case (lhsVal, rhsVal) of
    (MVNode lhsNode, MVNode rhsNode) -> addEqAssumption lhsNode rhsNode
    (MVRef  lhsRef,  MVRef  rhsRef) ->
      when (lhsRef /= rhsRef) $ throwError "Incompatible references"
ocStep (AssertArrayEq lhs rhs) = do
  JSS.RValue lhsRef <- ocEval evalJavaExpr lhs
  rhsVal <- ocEval evalLogicExpr rhs
  ps <- gets (ecPathState . ocsEvalContext)
  case Map.lookup lhsRef (JSS.arrays ps) of
    Nothing -> throwError $ "The array associated to " ++ show lhs ++ " is undefined."
    Just (_,lhsVal) -> 
      --TODO: Verify length matches rhsVal's length.
      addEqAssumption lhsVal rhsVal

ocStep (EnsureInstanceField refExpr fid rhsExpr) =
  setInstanceField refExpr fid =<< ocEval jssEvalMixedExpr rhsExpr
ocStep (EnsureArray lhsExpr rhsExpr) =
  setArrayValue lhsExpr =<< ocEval evalLogicExpr rhsExpr

ocStep (ModifyInstanceField refExpr f) = do
  de <- gets (ecDagEngine . ocsEvalContext)
  let tp = JSS.fieldIdType f
  n <- liftIO $ mkIntInput de (JSS.stackWidth tp)
  setInstanceField refExpr f (mkJSSValue tp n)
ocStep (ModifyArray refExpr tp) = do
  de <- gets (ecDagEngine . ocsEvalContext)
  setArrayValue refExpr =<< liftIO (mkInputWithType de tp)

ocStep (Return expr) = do
  val <- ocEval jssEvalMixedExpr expr
  modifyResultState $ \ps ->
    return $
      case JSS.frames ps of
        []  -> ps { JSS.finalResult = JSS.ReturnVal val }
        f:r -> ps { JSS.frames = f { JSS.frmOpds = val : JSS.frmOpds f } : r }

-- Executing overrides {{{2

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
  let specName = methodSpecName ir
  -- Check class initialization.
  checkClassesInitialized pos specName (initializedClasses ir)
  -- TODO: Check references have correct type.
  -- Get state of current execution path in simulator.
  de <- JSS.liftSymbolic $ getDagEngine
  ps <- JSS.getPathState
  -- Get spec at PC 0.
  let Just bsl = Map.lookup 0 (methodSpecBehaviors ir)
  let ec = EvalContext { ecDagEngine = de
                       , ecThis = mbThis
                       , ecArgs = V.fromList args
                       , ecPathState = ps
                       }
      initBCS = OCState { ocsEvalContext = ec
                        , ocsResultState = ps
                        }
  res <- liftIO $ forM bsl $ \bs ->
    runErrorT $ flip evalStateT initBCS $ do
       -- Execute statements.
       mapM_ ocStep (bsStatements bs)

       gets ocsResultState
  case rights res of
    _:_:_ ->
      --TODO: Relax this internal limitation.
      let msg = "There are multiple potentially valid behaviors when executing "
                  ++ " the lemma generated for " ++ specName ++ "."
                  ++ "SAWScript currently requires that it can unconditionally "
                  ++ "determine the behavior of the method."
       in throwIOExecException pos (ftext msg) ""
    [] -> 
      let msg = "Encountered error when evaluating " ++ specName ++ ":\n" 
                   ++ head (lefts res)
       in throwIOExecException pos (ftext msg) ""
    [ps'] -> JSS.putPathState ps'

             {-TODO
execOverride pos ir mbThis args = do
  -- Check references have correct type.
  liftIO $ do
    seenRefsIORef <- liftIO $ newIORef (Map.empty :: Map JSS.Ref TC.JavaExpr)
    forM_ (specReferences ir) $ \(ec, _iv) -> do
      seenRefs <- liftIO $ readIORef seenRefsIORef
      refs <- forM ec $ \javaExpr -> do
                let Just (JSS.RValue r) = evalJavaExpr jec javaExpr
                case Map.lookup r seenRefs of
                  Nothing -> return ()
                  Just prevExpr -> do
                    let msg = "When using the method override for " ++ methodSpecName ir
                                ++ ", the expression " ++ show javaExpr
                                ++ " aliased a reference that was referred by "
                                ++ show prevExpr ++ "."
                        res = "Please use a \'mayAlias\' declaration when values may alias each other."
                     in throwIOExecException pos (ftext msg) res
                return r
      let newRefs = foldr (uncurry Map.insert) seenRefs (refs `zip` ec)
      writeIORef seenRefsIORef newRefs
      -}

-- | Add a method override for the given method to the simulator.
overrideFromSpec :: Pos -> MethodSpecIR -> JSS.Simulator SymbolicMonad ()
overrideFromSpec pos ir 
  | JSS.methodIsStatic method =
      JSS.overrideStaticMethod cName key $ \args ->
        execOverride pos ir Nothing args
  | otherwise =
      JSS.overrideInstanceMethod cName key $ \thisVal args ->
        execOverride pos ir (Just thisVal) args
 where cName = JSS.className (methodSpecIRMethodClass ir)
       method = methodSpecIRMethod ir
       key = JSS.methodKey method

-- Initial state generation {{{1

-- | State for running the behavior specifications in a method override.
data ISGState t l = ISGState {
         isgPathState :: JSS.PathState t
       }

-- | Monad used to execute statements in a behavior specification for a method
-- override. 
type InitialStateGenerator t l = StateT (ISGState t l) (ErrorT String IO)

{-
isgEval :: (EvalContext n l -> a -> ExprEvaluator b) -> a -> InitialStateGenerator n l b
isg

isgStep :: Statement -> InitialStateGenerator t l ()
isgStep (AssertPred expr) =
  addAssumption =<< isgEval evalLogicExpr expr
  -}

{-
ocStep (AssumeValueEq lhs rhs) = do
  -- Check left-hand side equals right-hand side.
  lhsVal <- ocEval evalMixedExpr (JE lhs)
  rhsVal <- ocEval evalMixedExpr rhs
  case (lhsVal, rhsVal) of
    (MVNode lhsNode, MVNode rhsNode) -> addEqAssumption lhsNode rhsNode
    (MVRef  lhsRef,  MVRef  rhsRef) -> do
      when (lhsRef /= rhsRef) $ throwError "Incompatible references"
ocStep (AssumeArrayEq lhs rhs) = do
  JSS.RValue lhsRef <- ocEval evalJavaExpr lhs
  rhsVal <- ocEval evalLogicExpr rhs
  --TODO ps <- gets (ecPathState . ocsEvalContext)
  ps <- undefined
  case Map.lookup lhsRef (JSS.arrays ps) of
    Nothing -> throwError $ "The array associated to " ++ show lhs ++ " is undefined."
    Just (_,lhsVal) -> 
      --TODO: Verify length matches rhsVal's length.
      addEqAssumption lhsVal rhsVal

ocStep (EnsureInstanceField refExpr fid rhsExpr) =
  setInstanceField refExpr fid =<< ocEval jssEvalMixedExpr rhsExpr
ocStep (EnsureArray lhsExpr rhsExpr) =
  setArrayValue lhsExpr =<< ocEval evalLogicExpr rhsExpr

isgStep (ModifyInstanceField refExpr f) = do
  de <- gets (ecDagEngine . ocsEvalContext)
  let tp = JSS.fieldIdType f
  n <- liftIO $ mkIntInput de (JSS.stackWidth tp)
  setInstanceField refExpr f (mkJSSValue tp n)
isgStep (ModifyArray refExpr tp) = do
  de <- gets (ecDagEngine . ocsEvalContext)
  setArrayValue refExpr =<< liftIO (mkInputWithType de tp)

isgStep (Return expr) = do
  val <- ocEval jssEvalMixedExpr expr
  modifyResultState $ \ps ->
    return $
      case JSS.frames ps of
        []  -> ps { JSS.finalResult = JSS.ReturnVal val }
        f:r -> ps { JSS.frames = f { JSS.frmOpds = val : JSS.frmOpds f } : r }
        -}

-- MethodSpec verification {{{1
-- EquivClassMap {{{2
type EquivClassMap = (Int, Map Int [TC.JavaExpr], Map Int TC.JavaActualType)

-- | Return list of class indices to initial values.
equivClassMapEntries :: EquivClassMap
                     -> V.Vector (Int, [TC.JavaExpr], TC.JavaActualType)
equivClassMapEntries (_,em,vm)
  = V.map (\(i,v) -> (i,em Map.! i,v))
  $ V.fromList $ Map.toList vm

-- JavaVerificationState {{{2
type InputEvaluator = SV.Vector Bool -> CValue

data VerificationInput = VerificationInput
  { viExprs :: [TC.JavaExpr]      -- ^ Many, because they may be aliased.
  , viNode  :: Node               -- ^ The node for the aliased group.
  , viEval  :: InputEvaluator     -- ^ Turn bit-patterns back into values.
  }

-- | JavaVerificationState contains information necessary to map between specification
-- expressions and the initial JVM state during verification.
data JavaVerificationState = JVS {
        -- | Name of method in printable form.
        jvsMethodName :: String
        -- | Maps Spec Java expression to value for that expression.
      , jvsExprValueMap :: Map TC.JavaExpr (JSS.Value Node)
        -- | Contains inputs to Java verifiction in reverse order that they appear.
      , jvsInputs :: [VerificationInput]
        -- | Maps JSS refs to name for that ref.
      , jvsRefExprMap :: Map JSS.Ref [TC.JavaExpr]
      }

{-
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
    -}

type JavaEvaluator = StateT JavaVerificationState (JSS.Simulator SymbolicMonad)

-- initializeJavaVerificationState {{{3

{-
-- | Initialize JavaVerificationState components involving references,
-- and create them in simulator.
createJavaEvalReferences :: EquivClassMap -> JavaEvaluator ()
createJavaEvalReferences cm = do
  de <- lift $ JSS.liftSymbolic $ getDagEngine
  V.forM_ (equivClassMapEntries cm) $ \(_idx, exprClass, initValue) -> do
    litCount <- liftIO $ beInputLitCount (deBitEngine de)
    let refName = ppJavaExprEquivClass exprClass
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
                VerificationInput
                   { viExprs = exprClass
                   , viNode = n
                   , viEval = inputEval
                   } : jvsInputs s
            , jvsRefNameMap = Map.insert ref refName (jvsRefNameMap s)
            , jvsArrayNodeList = (ref,exprClass,n):(jvsArrayNodeList s) }
            -}

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
          VerificationInput
             { viNode = n
             , viEval = \lits -> mkCIntFromLsbfV (SV.slice litCount w lits)
             , viExprs = [expr]
             } : jvsInputs s
      }
 
{-
-- | Initialize the Java verification state.
initializeJavaVerificationState :: MethodSpecIR
                                -> EquivClassMap
                                -> JSS.Simulator SymbolicMonad JavaVerificationState
initializeJavaVerificationState ir cm = do
  let initialState = JVS
        { jvsMethodName = methodSpecName ir
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
    forM_ (methodSpecJavaExprs ir) $ \expr -> do
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
  let clName = className (methodSpecIRMethodClass ir)
  let method = methodSpecIRMethod ir
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
  {-
  forM_ (Map.toList $ arrayPostconditions specs) $ \(javaExpr,post) ->
    evalArrayPost de ssi javaExpr post
  forM_ (instanceFieldPostconditions specs) $ \(refExpr,f,post) ->
    evalInstanceFieldPost ssi refExpr f post
  -}
  -- Retrieve the path state after these updates and build a new
  -- JEC based on it.
  newPS <- JSS.getPathState
  let jec' = jec { jecPathState = newPS }
  res <- JSS.run
  return (res, jec')
  -}

-- ExpectedStateDef {{{2

-- | Describes expected result of computation.
data ExpectedStateDef = ESD {
       -- | Expected assumptions
       esdAssumptions :: Node
       -- | Expected return value or Nothing if method returns void.
     , esdReturnValue :: Maybe (JSS.Value Node)
       -- | Maps instance fields to expected values.
     , esdInstanceFields :: Map (JSS.Ref, JSS.FieldId) (Maybe (JSS.Value Node))
       -- | Maps reference to expected node (or Nothing if value is arbitrary).
     , esdArrays :: Map JSS.Ref (Maybe Node)
     }

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

-- VerificationCheck {{{2

data VerificationCheck
  = PathCheck Node
  | EqualityCheck String -- ^ Name of value to compare
                  Node -- ^ Condition under which this equality should hold.
                  Node -- ^ Value returned by JVM symbolic simulator.
                  Node -- ^ Expected value in Spec.
  deriving (Eq, Ord, Show)

-- | Returns goal that one needs to prove.
checkGoal :: DagEngine Node Lit -> VerificationCheck -> Node
checkGoal _ (PathCheck n) = n
checkGoal de (EqualityCheck _ c x y) =
  deApplyBinary de bImpliesOp c $ deApplyBinary de (eqOp (termType x)) x y

checkName :: VerificationCheck -> String
checkName (PathCheck _) = "the path condition"
checkName (EqualityCheck nm _ _ _) = nm

-- | Returns documentation for check that fails.
checkCounterexample :: VerificationCheck -> (Node -> CValue) -> Doc
checkCounterexample (PathCheck n) evalFn =
  text "The path conditions were unsatisfied:" <+> prettyTermD n
checkCounterexample (EqualityCheck nm _cond jvmNode specNode) evalFn =
  text nm $$
    nest 2 (text "Encountered: " <> ppCValueD Mixfix (evalFn jvmNode)) $$
    nest 2 (text "Expected:    " <> ppCValueD Mixfix (evalFn specNode))

{-
-- | Returns assumptions in method spec.
methodAssumptions :: DagEngine Node Lit
                  -> MethodSpecIR
                  -> SpecEvalContext Node
                  -> Node
methodAssumptions de ir sec = do
  let spec = case jecInitPC (secJavaEvalContext sec) of
               0 -> returnSpec ir
               pc -> case Map.lookup pc (localSpecs ir) of
                       Nothing -> error $ "internal: no specification found for pc " ++ show pc
                       Just s -> s
   in foldl' (deApplyBinary de bAndOp) (mkCBool True) $
        map (evalLogicExpr de sec) (localAssumptions spec)
        -}

-- | Add verification condition to list.
addEqVC :: String -> Node -> Node -> Node -> State [VerificationCheck] ()
addEqVC name cond jvmNode specNode = do
  modify $ \l -> EqualityCheck name cond jvmNode specNode : l

{-
-- | Compare old and new states.
comparePathStates :: MethodSpecIR
                  -> JavaVerificationState
                  -> ExpectedStateDef
                  -> JSS.PathState Node
                  -> Maybe (JSS.Value Node)
                  -> [VerificationCheck]
comparePathStates ir jvs esd newPathState mbRetVal =
  flip execState [] $ do
    -- Check return value.
    case mbRetVal of
      Nothing -> return ()
      Just (JSS.IValue rv) -> do
        let Just (JSS.IValue expRetVal) = esdReturnValue esd
         in addEqVC "return value" c rv expRetVal
      Just (JSS.LValue rv) ->
        let Just (JSS.LValue expRetVal) = esdReturnValue esd
         in addEqVC "return value" c rv expRetVal
      Just _ ->  error "internal: The Java method has a return type unsupported by JavaVerifier."
    -- Check initialization
    do let specInits = Set.fromList (initializedClasses ir)
           jvmInits  = Set.fromList $ Map.keys $ JSS.initialization newPathState
           newInits = jvmInits `Set.difference` specInits
       unless (Set.null newInits) $ do
         let msg = "The JVM method \'" ++ mName ++ "\' initializes extra classes "
                    ++ "during execution.  This feature is not currently suported "
                    ++ "by JavaVerifier.  The extra classes are:"
             newInitNames = nest 2 (vcat (map (text . slashesToDots) (Set.toList newInits)))
         throwExecException pos (ftext msg $$ newInitNames) ""
    -- Check class objects
    do let jvmClassObjects = Set.fromList $ Map.keys $ JSS.classObjects newPathState
       unless (Set.null jvmClassObjects) $ do
         let msg = "The JVM method \'" ++ mName ++ "\' referenced class objects "
                    ++ "during execution.  This feature is not currently suported "
                    ++ "by JavaVerifier.  The extra class objects are:"
             newNames = nest 2
                      $ vcat (map (text . slashesToDots) (Set.toList jvmClassObjects))
         throwExecException pos (ftext msg $$ newNames) ""
    -- Check static fields
    do forM_ (Map.toList $ JSS.staticFields newPathState) $ \(fid,_jvmVal) -> do
         let clName = slashesToDots (fieldIdClass fid)
         let fName = clName ++ "." ++ fieldIdName fid
         let msg = "The JVM method \'" ++ mName ++ "\' has modified the "
                  ++ " static field " ++ fName ++ " during execution.  "
                  ++ "This feature is not currently suported by JavaVerifier."
          in throwExecException pos (ftext msg) ""
    -- Check instance fields
    forM_ (Map.toList $ JSS.instanceFields newPathState) $ \((ref,fid),jvmVal) -> do
      refName <- getRefName pos ref jvs
      let fieldName = refName ++ "." ++ fieldIdName fid
      specVal <-
        case Map.lookup (ref,fid) (esdInstanceFields esd) of
          Nothing -> do
            let msg = "The JVM method \'" ++ mName ++ "\' has written to the "
                       ++ "instance field \'" ++ fieldName
                       ++ "\' which was not defined in the specification."
                res = "Please ensure all relevant fields are defined in the specification."
            throwExecException pos (ftext msg) res
          Just v -> return v
      let throwIfModificationUnsupported fieldType =
            let msg = "The JVM method \'" ++ mName ++ "\' has modified a "
                       ++ fieldType ++ " instance field \'" ++ fieldName
                       ++ "\'.  JavaVerifier does not currently support "
                       ++ "modifications to this type of field."
             in throwExecException pos (ftext msg) ""
      case (jvmVal,specVal) of
        (_,Nothing) -> return ()
        (jv, Just sv) | jv == sv -> return ()
        (JSS.DValue _, _) -> throwIfModificationUnsupported "floating point"
        (JSS.FValue _, _) -> throwIfModificationUnsupported "floating point"
        (JSS.RValue _, _) -> throwIfModificationUnsupported "reference"
        (JSS.IValue jvmNode, Just (JSS.IValue specNode)) ->
          addEqVC fieldName c jvmNode specNode
        (JSS.LValue jvmNode, Just (JSS.LValue specNode)) ->
          addEqVC fieldName c jvmNode specNode
        (_, Just _) -> error "internal: comparePathStates encountered illegal field type."
    -- Check ref arrays
    do let jvmRefArrays = JSS.refArrays newPathState
       unless (Map.empty == jvmRefArrays) $ do
         let msg = "The JVM method \'" ++ mName ++ "\' has modified reference arrays "
                    ++ "during execution.  This feature is not currently suported "
                    ++ "by JavaVerifier."
         throwExecException pos (ftext msg) ""
    -- Check value arrays.
    forM_ (Map.toList (JSS.arrays newPathState)) $ \(ref,(_,jvmNode)) -> do
      --TODO: Verify length of array matches expected length.
      refName <- getRefName pos ref jvs
      case Map.lookup ref (esdArrays esd) of
        Nothing -> error "internal: Unexpected undefined array reference."
        Just Nothing -> return ()
        Just (Just specNode) ->
          addEqVC refName c jvmNode specNode
 where pos = methodSpecPos ir
       mName = methodSpecName ir
       --initialVCS  = [PathCheck (JSS.psAssumptions newPathState)]
       c = JSS.psAssumptions newPathState
       -}

-- verifyMethodSpec and friends {{{2

-- | Describes the verification result arising from one symbolic execution path.
data ExecutionResult = ExecutionResult {
          -- | Starting PC of execution run.
          erStartPC :: JSS.PC
          -- | End PC of execution run.
        , erEndPC :: Maybe JSS.PC
          -- | Inputs to verification problem (universally quantified).
        , erInputs :: [VerificationInput]
          -- | Assumptions on inputs.
        , erAssumptions :: Node
          -- | What to verify for this result.
        , erChecks :: [VerificationCheck]
          -- Describes which op indexes are enabled in this execution result.
          -- TODO: See if this can be moved somewhere else.
        , erEnabled :: Set OpIndex
        }

-- | Analyze the specification, and return a list of symbolic executions to run
-- that will generate the different paths we need to verify.
methodSpecVCs :: VerifyParams -> [SymbolicMonad [ExecutionResult]]
methodSpecVCs
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
  let executionParams = [ (bs,cl) | bs <- concat $ Map.elems $ methodSpecBehaviors ir
                                  , cl <- partitions (bsEquivClasses bs)]
  -- Return executions
  flip map executionParams $ \(bs,cl) -> do
    setVerbosity vrb
    JSS.runSimulator cb $ do
      setVerbosity vrb
      -- Log execution.
      -- TODO: Create initial Java state.
      firstPSS <- JSS.pathStSel <$> JSS.getPathState
      initPS <- nyi "create initial java state."
      let pc = bsPC bs
      let callFrame = JSS.Call { JSS.frmClass = JSS.className (methodSpecIRThisClass ir)
                               , JSS.frmMethod = methodSpecIRMethod ir
                               , JSS.frmPC = pc
                               , JSS.frmLocals = nyi "frmLocals" --(Map LocalVariableIndex (Value term))
                               , JSS.frmOpds = [] -- TODO: Support breakpoints at stack locations. 
                               }
          initPS =
            JSS.PathState {
                JSS.frames         = [callFrame]
              , JSS.finalResult    = JSS.Unassigned
              , JSS.initialization = Map.fromList $
                                       [ (cl, JSS.Initialized)
                                       | cl <- initializedClasses ir ]
              , JSS.staticFields   = Map.empty -- TODO: Support static fields.
              , JSS.instanceFields = nyi "Map InstanceFieldRef (Value term)"
              , JSS.arrays         = nyi "Map Ref (Int32, term)"
              , JSS.refArrays      = nyi "Map Ref (Array Int32 Ref)"
              , JSS.psAssumptions  = nyi "psAssumptions" -- !term
              , JSS.pathStSel      = firstPSS
              , JSS.classObjects   = Map.empty
              , JSS.startingPC     = pc -- TODO: Investigate if startingPC is set correctly.
              , JSS.breakpoints    = Set.fromList (nyi "JSS.breakpoints")
              , JSS.insnCount      = 0
              }
      JSS.putPathState initPS
      -- Add method spec overrides.
      mapM_ (overrideFromSpec pos) overrides
      -- Register breakpoints
      {-
      when (vrb >= 4 && not (null assertPCs)) $
         liftIO $ putStrLn $ "Registering breakpoints: " ++ show assertPCs
      JSS.registerBreakpoints $ map (\bpc -> (cls, meth, bpc)) assertPCs
      -}
      -- Execute code.
      when (vrb >= 3) $ do
        liftIO $ putStrLn $ "Executing " ++ methodSpecName ir ++ " at PC " ++ show pc ++ "."
      jssResults <- JSS.run
      forM jssResults $ \(pd, fr) -> do
        _finalPS <- JSS.getPathStateByName pd :: JSS.Simulator SymbolicMonad (JSS.PathState Node)  
        case fr of
          JSS.ReturnVal val -> do
            -- TODO: Build VCs by comparing finalPS with Ensure statements from bs. 
            nyi "methodSpecVCs ReturnVal" val
          JSS.Terminated -> nyi "methodSpecVCs Terminated"
          JSS.Breakpoint pc -> do
            -- TODO: Build VCs by
            -- * Comparing Ensures statements from bs with Ensures statement at pc.
            -- * Verifying assumptions hold at current breakpoint.
           nyi "methodSpecVCs Breakpoint" pc
          JSS.Exc JSS.SimExtErr{} -> nyi "methodSpecVCs SimExtErr"
          JSS.Exc JSS.JavaException{} -> nyi "methodSpecVCs JavaException"
          JSS.Aborted -> nyi "methodSpecVCs Aborted"
          JSS.Unassigned -> error "internal: run terminated before completing."
      nyi "methodSpecVCs merging VCS from all jssResults"


{-
  let refEquivClasses = 
  let assertPCs = Map.keys (localSpecs ir)
  let cls = JSS.className $ methodSpecIRThisClass ir
  let meth = JSS.methodKey $ methodSpecIRMethod ir
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
                method = methodSpecIRMethod ir
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
        let msg = "The Java method " ++ methodSpecName ir
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
                       methodSpecName ir ++
                       "[" ++ show pc ++ "->" ++ show bpc ++ "]"
                     _ -> case pc of
                            0 -> methodSpecName ir
                            _ -> methodSpecName ir ++
                                 "[" ++ show pc ++ "->end]"
        -- Build final equation and functions for generating counterexamples.
        newPathState <- JSS.getPathStateByName ps
        JSS.liftSymbolic $ do
          let esd = expectedStateDef de ir jvs jec fr
          liftIO $ when (vrb >= 6) $
            putStrLn $ "Creating verification conditions for " ++ name
          -- Create verification conditions from path states.
          let vcs = comparePathStates ir jvs esd newPathState returnVal
          return ExecutionResult {
                     erAssumptions = esdAssumptions esd
                   , erInputs = reverse (jvsInputs jvs)
                   , erChecks = vcs
                   , erEnabled = vpEnabledOps params
                   }
                   -}

runABC :: DagEngine Node Lit
       -> Int -- ^ Verbosity
       -> MethodSpecIR
       -> [VerificationInput]
       -> VerificationCheck
       -> Node
       -> IO ()
runABC de v ir inputs vc goal = do
  when (v >= 3) $ do
    putStrLn $ "Running ABC on " ++ methodSpecName ir
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
           in throwIOExecException (methodSpecPos ir) (ftext msg) ""
        Sat lits -> do
          -- Get doc showing inputs
          let docInput vi = map (\e -> text (show e) <+> equals <+> ppCValueD Mixfix c)
                                (viExprs vi)
                where c = viEval vi lits
          let inputDocs = concatMap docInput inputs
          -- Get differences between two.
          let inputValues = V.map (\vi -> viEval vi lits) (V.fromList inputs)
          diffDoc <- checkCounterexample vc <$> deConcreteEval inputValues
          let msg = ftext ("A counterexample was found by ABC when verifying "
                             ++ methodSpecName ir ++ ".\n\n") $$
                    ftext ("The inputs that generated the counterexample are:") $$
                    nest 2 (vcat inputDocs) $$
                    ftext ("Counterexample:") $$
                    nest 2 diffDoc
          throwIOExecException (methodSpecPos ir) msg ""

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
                        , vpPos = pos
                        , vpOpts = opts
                        , vpSpec = ir
                        , vpRules = rules
                        } = do
  let verb = verbose opts
  let specName = methodSpecName ir
  when (verb >= 2) $ putStrLn $ "Starting verification of " ++ specName
  let vcList = methodSpecVCs params
  forM_ vcList $ \vcGenerator -> do
    runSymbolic oc $ do
      results <- vcGenerator
      nyi "Not yet implemented: validateMethodSpec"


{- TODO: Fix below
  let pgm = foldl' addRule emptyProgram rules

  forM_ vcList $ \mVC -> do
    when (v >= 6) $
      liftIO $ putStrLn $ "Considering new alias configuration of " ++ methodSpecName ir
    runSymbolic oc $ do
      setVerbosity v
      de <- getDagEngine
      vcs <- mVC
      let goal vc check = deApplyBinary de bImpliesOp (erAssumptions vc) (checkGoal de check)
      case methodSpecVerifyCommands ir of
        [AST.Rewrite] -> liftIO $ do
          rew <- mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            forM_ (erChecks vc) $ \check -> do
              when (v >= 2) $
                putStrLn $ "Verify " ++ checkName check
              newGoal <- reduce rew (goal vc check)
              case getBool newGoal of
                Just True -> return ()
                _ -> do
                 let msg = ftext ("The rewriter failed to reduce the verification condition "
                                    ++ " generated from " ++ checkName check
                                    ++ " in the Java method " ++ methodSpecName ir
                                    ++ " to 'True'.\n\n") $$
                           ftext ("The remaining goal is:") $$
                           nest 2 (prettyTermD newGoal)
                     res = "Please add new rewrite rules or modify existing ones to reduce the goal to True."
                  in throwIOExecException pos msg res
        [AST.QuickCheck n lim] -> liftIO $ do
          forM_ vcs $ \vc ->
            testRandom de v ir n lim vc
        [AST.ABC] -> liftIO $ do
          forM_ vcs $ \vc -> do
            forM_ (erChecks vc) $ \check -> do
              when (v >= 2) $
                putStrLn $ "Verify " ++ checkName check
              runABC de v ir (erInputs vc) check (goal vc check)
        [AST.Rewrite, AST.ABC] -> liftIO $ do
          rew <- mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            forM_ (erChecks vc) $ \check -> do
              when (v >= 2) $
                liftIO $ putStrLn $ "Verify " ++ checkName check
              newGoal <- reduce rew (goal vc check)
              runABC de v ir (erInputs vc) check newGoal
        [AST.SmtLib ver nm] -> do
          forM_ vcs $ \vc -> do
            -- XXX: This is called multiple times, so when we save the
            -- smtlib file we should somehow parameterize on the configuration.
            let gs = map (checkGoal de) (erChecks vc)
            useSMTLIB ir ver nm vc gs
        [AST.Rewrite, AST.SmtLib ver nm] -> do
          rew <- liftIO $ mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            gs <- liftIO $ mapM (reduce rew . checkGoal de) (erChecks vc)
            useSMTLIB ir ver nm vc gs
        [AST.Yices ti]  -> do
          forM_ vcs $ \vc -> do
            let gs = map (checkGoal de) (erChecks vc)
            useYices ir ti vc gs
        [AST.Rewrite, AST.Yices ti] -> do
          rew <- liftIO $ mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            gs <- liftIO $ mapM (reduce rew . checkGoal de) (erChecks vc)
            useYices ir ti vc gs
        _ -> error "internal: verifyMethodTactic used invalid tactic."
        -}

type Verbosity = Int

testRandom :: DagEngine Node Lit -> Verbosity
           -> MethodSpecIR -> Int -> Maybe Int -> ExecutionResult -> IO ()
testRandom de v ir test_num lim vc =
    do when (v >= 3) $
         putStrLn $ "Generating random tests: " ++ methodSpecName ir
       (passed,run) <- loop 0 0
       when (passed < test_num) $
         let m = text "QuickCheck: Failed to generate enough good inputs."
                $$ nest 2 (vcat [ text "Attempts:" <+> int run
                                , text "Passed:" <+> int passed
                                , text "Goal:" <+> int test_num
                                ])
         in throwIOExecException (methodSpecPos ir) m ""
  where
  loop run passed | passed >= test_num      = return (passed,run)
  loop run passed | Just l <- lim, run >= l = return (passed,run)
  loop run passed = loop (run + 1) =<< testOne passed

  testOne passed = do
    vs   <- mapM (QuickCheck.pickRandom . termType . viNode) (erInputs vc)
    eval <- deConcreteEval (V.fromList vs)
    if not (toBool $ eval $ erAssumptions vc)
      then return passed
      else do forM_ (erChecks vc) $ \goal ->
                do let goal_ok = toBool (eval (checkGoal de goal))
                   unless goal_ok $ do
                     throwIOExecException (methodSpecPos ir)
                                          (msg eval vs goal) ""
              return $! passed + 1


  msg eval vs g =
      text "Random testing found a counter example:"
    $$ nest 2 (vcat
     [ text "Method:" <+> text (methodSpecName ir)
     , case g of
         EqualityCheck n _c x y ->
            text "Unexpected value for:" <+> text n
            $$ nest 2 (text "Expected:" <+> ppCValueD Mixfix (eval y)
                    $$ text "Found:"    <+> ppCValueD Mixfix (eval x))
         PathCheck _ -> text "Invalid path constraint."
     , text "Random arguments:"
       $$ nest 2 (vcat (zipWith ppInput (erInputs vc) vs))
     ])

  ppInput inp value =
    case viExprs inp of
      [t] -> text (show t) <+> text "=" <+> ppCValueD Mixfix value
      ts -> vcat [ text (show t) <+> text "=" | t <- ts ] <+> ppCValueD Mixfix value


  toBool (CBool b) = b
  toBool value = error $ unlines [ "Internal error in 'testRandom':"
                                 , "  Expected: boolean value"
                                 , "  Result:   " ++ ppCValue Mixfix value ""
                                 ]

announce :: String -> SymbolicMonad ()
announce msg = whenVerbosity (>= 3) (liftIO (putStrLn msg))

useSMTLIB :: MethodSpecIR -> Maybe Int -> Maybe String ->
              ExecutionResult -> [Node] -> SymbolicMonad ()
useSMTLIB ir mbVer mbNm vc gs =
  announce ("Translating to SMTLIB (version " ++ show version ++"): "
                                        ++ methodSpecName ir) >> liftIO (
  do let params = SmtLib.TransParams
                    { SmtLib.transName = name
                    , SmtLib.transInputs = map (termType . viNode) (erInputs vc)
                    , SmtLib.transAssume = erAssumptions vc
                    , SmtLib.transCheck = gs
                    , SmtLib.transEnabled = erEnabled vc
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
           Nothing -> methodSpecName ir

  version :: Int
  (version, ver_exr) = case mbVer of
                         Just n | n /= 1 -> (n, show n)
                         _      -> (1, "")   -- For now, we use 1 by default.


useYices :: MethodSpecIR -> Maybe Int ->
            ExecutionResult -> [Node] -> SymbolicMonad ()
useYices ir mbTime vc gs =
  announce ("Using Yices2: " ++ methodSpecName ir) >> liftIO (
  do (script,info) <- SmtLib.translate SmtLib.TransParams
        { SmtLib.transName = "CheckYices"
        , SmtLib.transInputs = map (termType . viNode) (erInputs vc)
        , SmtLib.transAssume = erAssumptions vc
        , SmtLib.transCheck = gs
        , SmtLib.transEnabled = erEnabled vc
        , SmtLib.transExtArr = True
        }

     res <- Yices.yices mbTime script
     case res of
       Yices.YUnsat   -> return ()
       Yices.YUnknown -> yiFail (text "Failed to decide property.")
       Yices.YSat m   ->
         yiFail ( text "Found a counter example:"
               $$ nest 2 (vcat $ intersperse (text " ") $
                    zipWith ppIn (erInputs vc) (map (Yices.getIdent m)
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
                , text "*** Method:" <+> text (methodSpecName ir)
                , text "*** Location:" <+> text (show (methodSpecPos ir))
                , text "*** Details:"
                , nest 2 xs
                ]

  ppIn vi val =
    case viExprs vi of
      [x] -> Yices.ppVal (show x, val)
      xs  -> vcat [ text (show x) <+> text "=" | x <- init xs ]
          $$ Yices.ppVal (show (last xs), val)

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
                 (map (show . last . viExprs) (erInputs vc))
                 (SmtLib.trInputs info) of
      [] -> empty
      ds -> text "Final values for array arguments:"
         $$ nest 2 (vcat (intersperse (text " ") ds))
