{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
module SAWScript.MethodSpec
  ( MethodSpecIR
  , methodSpecName
  , methodSpecIRMethodClass
  , methodSpecVerificationTactics
  , resolveMethodSpecIR
  , verifyMethodSpec
  , VerifyParams(..)
  ) where

-- Imports {{{1

import Control.Applicative hiding (empty)
import qualified Control.Exception as CE
import Control.Monad
import Control.Monad.State (State, execState)
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
import System.Random(randomIO, randomRIO)
import System.Directory(doesFileExist)

import qualified Execution.Codebase as JSS
import qualified Execution.JavaSemantics as Sem
import JavaParser as JSS
import MethodSpec (partitions)
import qualified SAWScript.SmtLib as SmtLib
import qualified SAWScript.SmtLib2 as SmtLib2
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
import Verinf.Utils.IOStateT
import Verinf.Utils.LogMonad

import qualified SMTLib1 as SmtLib
import qualified SMTLib2 as SmtLib2

-- Utilities {{{1

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
mkInputLitResultWithType _ = error "internal: mkInputLitResultWithType called with unsupported type."

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

-- JavaEvalContext {{{1

-- | Stores information about a particular Java state.
data JavaEvalContext n = EvalContext {
         jecThis :: Maybe JSS.Ref
       , jecArgs :: V.Vector (JSS.Value n)
       , jecPathState :: JSS.PathState n
       , jecInitPC :: JSS.PC
       }

-- | Returns value associated to Java expression in this state if it is defined,
-- or Nothing if the expression is undefined.
-- N.B. This method assumes that the Java path state is well-formed, the
-- the JavaExpression syntactically referes to the correct type of method
-- (static versus instance), and correct well-typed arguments.  It does
-- not assume that all the instanceFields in the JavaEvalContext are initialized.
evalJavaExpr :: JavaEvalContext Node -> TC.JavaExpr -> Maybe (JSS.Value Node)
evalJavaExpr jec (TC.This _) =
  case jecThis jec of
    Just r -> Just (JSS.RValue r)
    Nothing -> error "internal: evalJavaExpr given TC.This for static method"
evalJavaExpr jec (TC.Arg i _) =
  CE.assert (i < V.length (jecArgs jec)) $
    Just (jecArgs jec V.! i)
evalJavaExpr jec (TC.InstanceField e f) = do
  JSS.RValue r <- evalJavaExpr jec e
  Map.lookup (r,f) (JSS.instanceFields (jecPathState jec))
evalJavaExpr jec (TC.Local _ idx _) =
  case JSS.frames (jecPathState jec) of
    f : _ -> Map.lookup idx (JSS.frmLocals f)
    _ -> Nothing

-- SpecEvalContext {{{1

-- | Provides information for evaluation expressions with respect to a
-- particular Java state.
data SpecEvalContext n = SSI {
         secJavaEvalContext :: JavaEvalContext n
        -- | Maps names appearing in let bindings to the corresponding Node.
       , secLetNodeBindings :: Map String (MixedValue n)
       }

-- | Create spec state info from a Java state info and method spec IR.
createSpecEvalContext :: DagEngine Node Lit 
                      -> MethodSpecIR
                      -> JavaEvalContext Node
                      -> SpecEvalContext Node
createSpecEvalContext de ir jec =
  flip execState initialState $
    forM_ (methodLetBindings ir) $ \(name,expr) -> do
      sec <- get
      let n = evalMixedExpr de sec expr
      modify $ \s -> s { secLetNodeBindings = Map.insert name n (secLetNodeBindings s) }
 where initialState = SSI jec Map.empty

-- | Evaluates a typed expression.
evalLogicExpr :: DagEngine Node Lit -> SpecEvalContext Node -> TC.LogicExpr -> Node
evalLogicExpr de sec (TC.Apply op exprs) =
  deApplyOp de op (V.map (evalLogicExpr de sec) (V.fromList exprs))
evalLogicExpr de _   (TC.Cns c tp) = deConstantTerm de c tp
evalLogicExpr _  (secJavaEvalContext -> jec) (TC.JavaValue javaExpr _) =
  case evalJavaExpr jec javaExpr of
    Just (JSS.RValue r) ->
      let Just (_,n) = Map.lookup r (JSS.arrays (jecPathState jec))
       in n
    Just (JSS.IValue n) -> n
    Just (JSS.LValue n) -> n
    _ -> error "internal: evalLogicExpr failed to evaluated Java expression"
evalLogicExpr _  sec (TC.Var name _tp) = do
  case Map.lookup name (secLetNodeBindings sec) of
    Nothing -> error $ "internal: evalLogicExpr given invalid variable " ++ name
    Just (MVNode n) -> n
    Just (MVRef _) -> error "internal: evalLogicExpr given an invalid reference."

-- | Value of mixed expression in a particular context.
data MixedValue n
   = MVNode n 
   | MVRef JSS.Ref

-- | Return value from expression.
-- TODO: Support references.
evalMixedExpr :: DagEngine Node Lit
              -> SpecEvalContext Node
              -> TC.MixedExpr
              -> MixedValue Node
evalMixedExpr de sec (LE expr) = MVNode $ evalLogicExpr de sec expr
evalMixedExpr  _ sec (JE expr) =
  case evalJavaExpr (secJavaEvalContext sec) expr of 
    Just (JSS.AValue _) -> error "internal: evalMixedExpr given address value"
    Just (JSS.DValue _) -> error "internal: evalMixedExpr given floating point value"
    Just (JSS.FValue _) -> error "internal: evalMixedExpr given floating point value"
    Just (JSS.IValue n) -> MVNode n
    Just (JSS.LValue n) -> MVNode n
    Just (JSS.RValue r) -> MVRef r
    Nothing -> error "internal: evalMixedExpr given value that could not be evaluated."

-- | Return Java value associated with mixed expression.
mixedExprValue :: DagEngine Node Lit -> SpecEvalContext Node -> TC.MixedExpr -> JSS.Value Node
mixedExprValue de sec expr =
  case evalMixedExpr de sec expr of
    MVRef r -> JSS.RValue r
    MVNode n ->
      case termType n of
        SymInt (widthConstant -> Just 32) -> JSS.IValue n
        SymInt (widthConstant -> Just 64) -> JSS.LValue n
        _ -> error "internal: mixedExprValue called with malformed result type."

-- Method specification overrides {{{1

execOverride :: Pos
             -> String
             -> MethodSpecIR
             -> Maybe JSS.Ref
             -> [JSS.Value Node]
             -> JSS.Simulator SymbolicMonad ()
execOverride pos nm ir mbThis args = do
  -- Check Java expressions referenced in IR are defined in the path state.
  ps <- JSS.getPathState
  -- Create JavaEvalContext and SpecEvalContext from current simulator state.
  de <- JSS.liftSymbolic getDagEngine
  let jec = EvalContext {
                  jecThis = mbThis
                , jecArgs = V.fromList args
                , jecPathState = ps
                , jecInitPC = 0
                }
  let sec = createSpecEvalContext de ir jec
  forM_ (methodSpecJavaExprs ir) $ \javaExpr -> do
    when (isNothing (evalJavaExpr jec javaExpr)) $ do
      let msg = "The override for \'" ++ methodSpecName ir
                  ++ "\' was called while symbolically simulating " ++ nm
                  ++ ".  However, the method specification of \'"
                  ++ methodSpecName ir ++ "\' requires that the value of \'"
                  ++ show javaExpr ++ "\' is defined."
          res = "Please add a \'var\' or \'const\' declaration as appropriate "
                  ++ "to the specification of \'" ++ nm
                  ++ "\' to define \'" ++ show javaExpr ++ "\'."
       in throwIOExecException pos (ftext msg) res
  -- Check initialization status
  forM_ (initializedClasses ir) $ \c -> do
    status <- JSS.getInitializationStatus c
    when (status /= Just JSS.Initialized) $
      let msg = "The method spec " ++ methodSpecName ir ++ " requires that the class "
                  ++ slashesToDots c ++ " is initialized.  SAWScript does not "
                  ++ "support new initialized classes yet."
       in throwIOExecException pos (ftext msg) ""
  let rs = returnSpec ir
  -- Assume all assumptions
  mapM_ (\e -> JSS.assume (evalLogicExpr de sec e)) (localAssumptions rs)
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
  -- Check constants are really constants.
  forM_ (specConstants ir) $ \(javaExpr,c,tp) -> do
    let jvmNode =
          case evalJavaExpr jec javaExpr of
            Just (JSS.IValue n) -> n
            Just (JSS.LValue n) -> n
            Just (JSS.RValue r) -> -- Must be an array.
              let Just (_,n) = Map.lookup r (JSS.arrays (jecPathState jec))
               in n
            Just _ -> error $ "internal: javaExprNode given an invalid expression \'" ++ show javaExpr ++ ".\'"
            Nothing -> error $ "internal: javaExprNode given an undefined expression \'" ++ show javaExpr ++ ".\'"
    JSS.assume =<< JSS.liftSymbolic (applyEq jvmNode =<< makeConstant c tp)
  let spec = returnSpec ir
  -- Update arrayPostconditions
  forM_ (Map.toList $ arrayPostconditions spec) $ \(javaExpr,post) ->
    evalArrayPost de sec javaExpr post
  -- Update instance fields
  forM_ (instanceFieldPostconditions spec) $ \(refExpr,f,pc) ->
    evalInstanceFieldPost sec refExpr f pc
  -- Update return type.
  case JSS.methodReturnType (methodSpecIRMethod ir) of
    Nothing -> return ()
    Just _ -> do
      let Just returnExpr = returnValue ir
      JSS.pushValue $ mixedExprValue de sec returnExpr

-- | Add a method override for the given method to the simulator.
overrideFromSpec :: Pos -> String -> MethodSpecIR -> JSS.Simulator SymbolicMonad ()
overrideFromSpec pos nm ir = do
  let cName = className (methodSpecIRMethodClass ir)
  let method = methodSpecIRMethod ir
  let key = JSS.methodKey method
  if methodIsStatic method
    then JSS.overrideStaticMethod cName key $ \args ->
           execOverride pos nm ir Nothing args
    else JSS.overrideInstanceMethod cName key $ \thisVal args ->
           execOverride pos nm ir (Just thisVal) args

evalArrayPost :: DagEngine Node Lit
              -> SpecEvalContext Node
              -> JavaExpr
              -> Postcondition
              -> JSS.Simulator SymbolicMonad ()
evalArrayPost de sec javaExpr pc =
  case pc of
    PostUnchanged -> return ()
    PostArbitrary tp     -> JSS.setSymbolicArray r =<< liftIO (mkInputWithType de tp)
    PostResult (LE expr) -> JSS.setSymbolicArray r (evalLogicExpr de sec expr)
    PostResult (JE _) -> error "internal: Encountered Java expression in evalArrayPost"
  where Just (JSS.RValue r) = evalJavaExpr (secJavaEvalContext sec) javaExpr

evalInstanceFieldPost :: SpecEvalContext Node
                      -> JavaExpr
                      -> JSS.FieldId
                      -> Postcondition
                      -> JSS.Simulator SymbolicMonad ()
evalInstanceFieldPost sec refExpr f pc = do
  let Just (JSS.RValue r) = evalJavaExpr (secJavaEvalContext sec) refExpr
  let tp = JSS.fieldIdType f
  de <- JSS.liftSymbolic getDagEngine
  case pc of
    PostUnchanged -> return ()
    PostArbitrary _ -> do
      n <- liftIO $ mkIntInput de (JSS.stackWidth tp)
      JSS.setInstanceFieldValue r f (mkJSSValue tp n)
    PostResult expr -> do
      JSS.setInstanceFieldValue r f (mixedExprValue de sec expr)

evalLocalPost :: SpecEvalContext Node
              -> JSS.LocalVariableIndex
              -> JSS.Type
              -> Postcondition
              -> JSS.Simulator SymbolicMonad ()
evalLocalPost sec idx tp pc = do
  de <- JSS.liftSymbolic getDagEngine
  case pc of
    PostUnchanged -> return ()
    PostArbitrary _ -> do
      n <- liftIO $ mkIntInput de (JSS.stackWidth tp)
      JSS.setLocal idx (mkJSSValue tp n)
    PostResult expr -> JSS.setLocal idx (mixedExprValue de sec expr)

evalImpAssumption :: SpecEvalContext Node
                  -> TC.JavaExpr
                  -> TC.MixedExpr
                  -> JSS.Simulator SymbolicMonad ()
evalImpAssumption sec (Local _ i tp) rhs = do
  de <- JSS.liftSymbolic getDagEngine
  JSS.setLocal i (mixedExprValue de sec rhs)
evalImpAssumption _ lhs _ = error "unsupported imperative assumption type"

-- MethodSpec verification {{{1
-- EquivClassMap {{{2
type EquivClassMap = (Int, Map Int [TC.JavaExpr], Map Int TC.JavaRefActualType)

-- | Return list of class indices to initial values.
equivClassMapEntries :: EquivClassMap
                     -> V.Vector (Int, [TC.JavaExpr], TC.JavaRefActualType)
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
      , jvsRefNameMap :: Map JSS.Ref String
        -- | List of array references, the associated equivalence class, and the initial value.
      , jvsArrayNodeList :: [(JSS.Ref,JavaExprEquivClass,Node)]
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

-- initializeJavaVerificationState {{{3

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
 
-- | Create an evaluator state with the initial JVM state form the IR and
-- equivalence class map.
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

-- Java execution {{{2



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
  let specs = Map.findWithDefault emptyLocalSpec pc (localSpecs ir)
  if methodIsStatic method
    then JSS.invokeStaticMethod clName (methodKey method) args
    else do
      let Just thisRef = jecThis jec
      JSS.invokeInstanceMethod clName (methodKey method) thisRef args
  Sem.setPc pc
  -- Update the starting state with any imperative assumptions located
  -- at the starting PC.
  mapM_ (uncurry (evalImpAssumption sec)) (methodImpAssumptions ir sec)
  -- Retrieve the path state after these updates and build a new
  -- JEC based on it.
  newPS <- JSS.getPathState
  let jec' = jec { jecPathState = newPS }
  res <- JSS.run
  return (res, jec')

-- ExpectedStateDef {{{2

-- | Stores expected values in symbolic state after execution.
data ExpectedStateDef = ESD {
       -- | Expected return value or Nothing if method returns void.
       esdReturnValue :: Maybe (JSS.Value Node)
       -- | Maps instance fields to expected values.
     , esdInstanceFields :: Map (JSS.Ref, JSS.FieldId) (Maybe (JSS.Value Node))
       -- | Maps reference to expected node (or Nothing if value is arbitrary).
     , esdArrays :: Map JSS.Ref (Maybe Node)
     }

-- | Create a expected state definition from method spec and eval state.
expectedStateDef :: DagEngine Node Lit
                 -> MethodSpecIR
                 -> JavaVerificationState
                 -> SpecEvalContext Node
                 -> JSS.FinalResult Node
                 -> ExpectedStateDef
expectedStateDef de ir jvs sec fr = do
  ESD { esdReturnValue = mixedExprValue de sec <$> returnValue ir
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
 where jec = secJavaEvalContext sec
       spec = case fr of
                JSS.Breakpoint bpc ->
                  case Map.lookup bpc (localSpecs ir) of
                    Nothing -> error $
                               "internal: no intermediate specifications for pc " ++
                               show bpc
                    Just s -> s
                _ -> returnSpec ir

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

-- | Returns imperative assumptions in method spec.
methodImpAssumptions :: MethodSpecIR
                     -> SpecEvalContext Node
                     -> [(TC.JavaExpr, TC.MixedExpr)]
methodImpAssumptions ir sec = do
  let spec = case jecInitPC (secJavaEvalContext sec) of
               0 -> returnSpec ir
               pc -> case Map.lookup pc (localSpecs ir) of
                       Nothing -> error $ "internal: no specification found for pc " ++ show pc
                       Just s -> s
   in localImpAssumptions spec

-- | Add verification condition to list.
addEqVC :: String -> Node -> Node -> Node -> State [VerificationCheck] ()
addEqVC name cond jvmNode specNode = do
  modify $ \l -> EqualityCheck name cond jvmNode specNode : l

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
    -- Get array equations and counterexample parse functions
    forM_ (Map.toList (JSS.arrays newPathState)) $ \(ref,(_,jvmNode)) -> do
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

-- verifyMethodSpec and friends {{{2

data VerificationContext = VContext {
          vcAssumptions :: Node
        , vcInputs :: [VerificationInput]
        , vcChecks :: [VerificationCheck]
        , vcEnabled :: Set OpIndex
        }

-- | Attempt to verify method spec using verification method specified.
methodSpecVCs :: VerifyParams -> [SymbolicMonad [VerificationContext]]
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
  let refEquivClasses = partitions (specReferences ir)
  let assertPCs = Map.keys (localSpecs ir)
  let cls = JSS.className $ methodSpecIRThisClass ir
  let meth = JSS.methodKey $ methodSpecIRMethod ir
  flip concatMap refEquivClasses $ \cm -> flip map (0 : assertPCs) $ \pc -> do
    de <- getDagEngine
    -- initial state for some of them.
    setVerbosity vrb
    JSS.runSimulator cb $ do
      setVerbosity vrb
      when (vrb >= 6) $
         liftIO $ putStrLn $
           "Creating evaluation state for simulation of " ++ methodSpecName ir
      -- Add method spec overrides.
      mapM_ (overrideFromSpec pos (methodSpecName ir)) overrides
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
      when (vrb >= 6) $ do
         liftIO $ putStrLn $ "Executing " ++ methodSpecName ir
         when (pc /= 0) $
              liftIO $ putStrLn $ "  starting from PC " ++ show pc
      -- Register breakpoints
      when (vrb >= 4 && not (null assertPCs)) $
         liftIO $ putStrLn $ "Registering breakpoints: " ++ show assertPCs
      JSS.registerBreakpoints $ map (\bpc -> (cls, meth, bpc)) assertPCs
      -- Execute method.
      when (vrb >= 3) $
        liftIO $ putStrLn $ "Running " ++ methodSpecName ir ++
                            " starting from " ++ show pc
      (jssResult, jec) <- runMethod ir sec'
      let sec = createSpecEvalContext de ir jec
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
          let esd = expectedStateDef de ir jvs sec fr
          liftIO $ when (vrb >= 6) $
            putStrLn $ "Creating verification conditions for " ++ name
          -- Create verification conditions from path states.
          let vcs = comparePathStates ir jvs esd newPathState returnVal
          return VContext {
                     vcAssumptions = methodAssumptions de ir sec
                   , vcInputs = reverse (jvsInputs jvs)
                   , vcChecks = vcs
                   , vcEnabled = vpEnabledOps params
                   }

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
verifyMethodSpec :: VerifyParams -> IO ()
verifyMethodSpec
  (VerifyParams
    { vpSpec = MSIR { methodSpecVerificationTactics = [AST.Skip] }
    }
  ) = return ()

verifyMethodSpec
  params@(VerifyParams
    { vpOpCache = oc
    , vpPos = pos
    , vpOpts = opts
    , vpSpec = ir
    , vpRules = rules
    }
  ) = do

  let v = verbose opts
  let pgm = foldl' addRule emptyProgram rules
  when (v >= 2) $
    liftIO $ putStrLn $ "Starting verification of " ++ methodSpecName ir
  let vcList = methodSpecVCs params
  forM_ vcList $ \mVC -> do
    when (v >= 6) $
      liftIO $ putStrLn $ "Considering new alias configuration of " ++ methodSpecName ir
    runSymbolic oc $ do
      setVerbosity v
      de <- getDagEngine
      vcs <- mVC
      let goal vc check = deApplyBinary de bImpliesOp (vcAssumptions vc) (checkGoal de check)
      case methodSpecVerificationTactics ir of
        [AST.Rewrite] -> liftIO $ do
          rew <- mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            forM_ (vcChecks vc) $ \check -> do
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
            forM_ (vcChecks vc) $ \check -> do
              when (v >= 2) $
                putStrLn $ "Verify " ++ checkName check
              runABC de v ir (vcInputs vc) check (goal vc check)
        [AST.Rewrite, AST.ABC] -> liftIO $ do
          rew <- mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            forM_ (vcChecks vc) $ \check -> do
              when (v >= 2) $
                liftIO $ putStrLn $ "Verify " ++ checkName check
              newGoal <- reduce rew (goal vc check)
              runABC de v ir (vcInputs vc) check newGoal
        [AST.SmtLib ver nm] -> do
          forM_ vcs $ \vc -> do
            -- XXX: This is called multiple times, so when we save the
            -- smtlib file we should somehow parameterize on the configuration.
            let gs = map (checkGoal de) (vcChecks vc)
            useSMTLIB ir ver nm vc gs
        [AST.Rewrite, AST.SmtLib ver nm] -> do
          rew <- liftIO $ mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            gs <- liftIO $ mapM (reduce rew . checkGoal de) (vcChecks vc)
            useSMTLIB ir ver nm vc gs
        [AST.Yices ti]  -> do
          forM_ vcs $ \vc -> do
            let gs = map (checkGoal de) (vcChecks vc)
            useYices ir ti vc gs
        [AST.Rewrite, AST.Yices ti] -> do
          rew <- liftIO $ mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            gs <- liftIO $ mapM (reduce rew . checkGoal de) (vcChecks vc)
            useYices ir ti vc gs
        _ -> error "internal: verifyMethodTactic used invalid tactic."

type Verbosity = Int

testRandom :: DagEngine Node Lit -> Verbosity
           -> MethodSpecIR -> Int -> Maybe Int -> VerificationContext -> IO ()
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
    vs   <- mapM (pickRandom . termType . viNode) (vcInputs vc)
    eval <- deConcreteEval (V.fromList vs)
    if not (toBool $ eval $ vcAssumptions vc)
      then return passed
      else do forM_ (vcChecks vc) $ \goal ->
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
       $$ nest 2 (vcat (zipWith ppInput (vcInputs vc) vs))
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

-- Or, perhaps, short, tall, grande, venti :-)
data RandomSpec = Least | Small | Medium | Large | Largest


pickRandomSize :: DagType -> RandomSpec -> IO CValue
pickRandomSize ty spec =
  case ty of

    SymBool -> CBool `fmap`
      case spec of
        Least   -> return False
        Small   -> return False
        Medium  -> randomIO
        Large   -> return True
        Largest -> return True

    SymInt w ->
      case widthConstant w of
         Just n  -> mkCInt n `fmap`
           do let least   = 0
                  largest = bitWidthSize n - 1
              case spec of
                Least   -> return least
                Small   -> randomRIO (least, min largest (least + 100))
                Medium  -> randomRIO (least, largest)
                Large   -> randomRIO (max least (largest - 100), largest)
                Largest -> return largest

         Nothing -> qcFail "integers of non-constant size"

    SymArray els ty1 ->
      case widthConstant els of
        Just n    -> (CArray . V.fromList) `fmap`
                     replicateM (numBits n) (pickRandomSize ty1 spec)
        Nothing   -> qcFail "arrays of non-constant size"

    SymRec _ _    -> qcFail "record values"
    SymShapeVar _ -> qcFail "polymorphic values"
  where
  qcFail x = fail $
                "QuickCheck: Generating random " ++ x ++ " is not supported."

-- Distribution of tests.  The choice is somewhat arbitrary.
pickRandom :: DagType -> IO CValue
pickRandom ty = pickRandomSize ty =<< ((`pick` distr) `fmap` randomRIO (0,99))
  where
  pick n ((x,s) : ds) = if n < x then s else pick (n-x) ds
  pick _ _            = Medium

  distr :: [(Int, RandomSpec)]
  distr = [ (5,  Least)
          , (30, Small)
          , (30, Medium)
          , (30, Large)
          , (5,  Largest)
          ]


announce :: String -> SymbolicMonad ()
announce msg = whenVerbosity (>= 3) (liftIO (putStrLn msg))

useSMTLIB :: MethodSpecIR -> Maybe Int -> Maybe String ->
              VerificationContext -> [Node] -> SymbolicMonad ()
useSMTLIB ir mbVer mbNm vc gs =
  announce ("Translating to SMTLIB (version " ++ show version ++"): "
                                        ++ methodSpecName ir) >> liftIO (
  do let params = SmtLib.TransParams
                    { SmtLib.transName = name
                    , SmtLib.transInputs = map (termType . viNode) (vcInputs vc)
                    , SmtLib.transAssume = vcAssumptions vc
                    , SmtLib.transCheck = gs
                    , SmtLib.transEnabled = vcEnabled vc
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
            VerificationContext -> [Node] -> SymbolicMonad ()
useYices ir mbTime vc gs =
  announce ("Using Yices2: " ++ methodSpecName ir) >> liftIO (
  do (script,info) <- SmtLib.translate SmtLib.TransParams
        { SmtLib.transName = "CheckYices"
        , SmtLib.transInputs = map (termType . viNode) (vcInputs vc)
        , SmtLib.transAssume = vcAssumptions vc
        , SmtLib.transCheck = gs
        , SmtLib.transEnabled = vcEnabled vc
        , SmtLib.transExtArr = True
        }

     res <- Yices.yices mbTime script
     case res of
       Yices.YUnsat   -> return ()
       Yices.YUnknown -> yiFail (text "Failed to decide property.")
       Yices.YSat m   ->
         yiFail ( text "Found a counter example:"
               $$ nest 2 (vcat $ intersperse (text " ") $
                    zipWith ppIn (vcInputs vc) (map (Yices.getIdent m)
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
                 (map (show . last . viExprs) (vcInputs vc))
                 (SmtLib.trInputs info) of
      [] -> empty
      ds -> text "Final values for array arguments:"
         $$ nest 2 (vcat (intersperse (text " ") ds))
