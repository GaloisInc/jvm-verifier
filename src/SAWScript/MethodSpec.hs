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
import JavaParser as JSS
import MethodSpec (partitions)
import qualified SAWScript.SmtLib as SmtLib
import qualified SAWScript.SmtLib2 as SmtLib2
import qualified SAWScript.QuickCheck as QuickCheck
import qualified SAWScript.Yices  as Yices
import qualified SAWScript.MethodAST as AST
import qualified SAWScript.TypeChecker as TC
import qualified Simulation as JSS
import SAWScript.Utils
import SAWScript.TypeChecker
import Utils.Common

import Verinf.Symbolic
import Verinf.Symbolic.Lit.Functional
import Verinf.Utils.IOStateT
import Verinf.Utils.LogMonad

import qualified SMTLib1 as SmtLib
import qualified SMTLib2 as SmtLib2

-- Utilities {{{1

-- | Insert multiple keys that map to the same value in a map.
mapInsertKeys :: Ord k => [k] -> a -> Map k a -> Map k a
mapInsertKeys keys val m = foldr (\i -> Map.insert i val) m keys

-- | Returns the value bound to the first key in the map, or
-- Nothing if none of the keys are in the map.
mapLookupAny :: Ord k => [k] -> Map k a -> Maybe a
mapLookupAny keys m = listToMaybe $ catMaybes $ map (\k -> Map.lookup k m) keys

int32DagType :: DagType
int32DagType = SymInt (constantWidth 32)

int64DagType :: DagType
int64DagType = SymInt (constantWidth 64)

instanceFieldValue :: JSS.Ref -> JSS.FieldId -> JSS.PathState Node -> Maybe (JSS.Value Node)
instanceFieldValue r f ps = Map.lookup (r,f) (JSS.instanceFields ps)

-- | Create a node with given number of bits.
createSymbolicIntNode :: DagEngine Node Lit -> Int -> IO Node
createSymbolicIntNode de w = do
  let ?be = deBitEngine de
  lv <- LV <$> SV.replicateM w lMkInput
  deFreshInput de (Just lv) (SymInt (constantWidth (Wx w)))

mkSymbolicInt :: SV.Storable l => DagEngine Node l -> BitWidth -> IO Node
mkSymbolicInt de (Wx w) = do
  bits <- let ?be = deBitEngine de in SV.replicateM w lMkInput
  deFreshInput de (Just (LV bits)) int32DagType

createLitVectorFromType :: (?be :: BitEngine l, SV.Storable l)
                        => DagType -> IO (LitResult l)
createLitVectorFromType (SymInt (widthConstant -> Just (Wx w))) = 
  LV <$> SV.replicateM w lMkInput
createLitVectorFromType (SymArray (widthConstant -> Just (Wx l)) eltTp) =
  LVN <$> V.replicateM l (createLitVectorFromType eltTp)
createLitVectorFromType _ = error "internal: createLitVectorFromType called with unsupported type."

-- | Create a node with given number of bits.
createSymbolicFromType :: DagEngine Node Lit -> DagType -> IO Node
createSymbolicFromType de tp = do
 r <- let ?be = deBitEngine de in createLitVectorFromType tp
 deFreshInput de (Just r) tp

-- | Throw IO exception indicating name was previously defined.
throwNameAlreadyDefined :: MonadIO m => Pos -> Pos -> String -> m ()
throwNameAlreadyDefined pos absPrevPos name = do
  relPos <- liftIO $ posRelativeToCurrentDirectory absPrevPos
  throwIOExecException pos
                       (ftext "The name " <+> quotes (text name)
                          <+> ftext "has already been defined at "
                          <+> text (show relPos) <> char '.')
                       ("Please ensure all names are distinct.")

-- SpecJavaType {{{1

-- | A parsed type from AST.JavaType
data SpecJavaType
  = SpecRefClass !(JSS.Class) -- ^ Specific class for a reference.
  | SpecIntArray !Int
  | SpecLongArray !Int
  | SpecInt
  | SpecLong

-- | Pretty print SpecJavaType
ppSpecJavaType :: SpecJavaType -> String
ppSpecJavaType (SpecRefClass cl) = className cl
ppSpecJavaType (SpecIntArray l)  = "int[" ++ show l ++ "]"
ppSpecJavaType (SpecLongArray l) = "long[" ++ show l ++ "]"
ppSpecJavaType SpecInt  = "int"
ppSpecJavaType SpecLong = "long"

-- | Returns JSS Type of SpecJavaType
getJSSTypeOfSpecJavaType :: SpecJavaType -- ^ Spec Java reference to get type of.
                         -> JSS.Type -- ^ Java type
getJSSTypeOfSpecJavaType (SpecRefClass cl) = JSS.ClassType (className cl)
getJSSTypeOfSpecJavaType (SpecIntArray _) = JSS.ArrayType JSS.IntType
getJSSTypeOfSpecJavaType (SpecLongArray _) = JSS.ArrayType JSS.LongType
getJSSTypeOfSpecJavaType SpecInt  = JSS.IntType
getJSSTypeOfSpecJavaType SpecLong = JSS.LongType

-- | Converts an int into a Java array length.
checkedGetArrayLength :: MonadIO m => Pos -> Int -> m Int
checkedGetArrayLength pos l = do
  unless (0 <= l && toInteger l < toInteger (maxBound :: Int32)) $
    let msg  = "Array length " ++ show l ++ " is invalid."
     in throwIOExecException pos (ftext msg) ""
  return $ fromIntegral l

-- | Parse AST Type to SpecJavaType.
parseASTType :: JSS.HasCodebase m => AST.JavaType -> m SpecJavaType
parseASTType (AST.RefType pos names) = do
  let nm = intercalate "/" names
  fmap SpecRefClass $ lookupClass pos nm
parseASTType (AST.IntArray pos l) =
  fmap SpecIntArray $ checkedGetArrayLength pos l
parseASTType (AST.LongArray pos l) =
  fmap SpecLongArray $ checkedGetArrayLength pos l
-- Boolean AST expressions are internally treated as 32-bit integers by JVM
parseASTType (AST.BoolScalar  _) = return SpecInt
parseASTType (AST.IntScalar  _) = return SpecInt
parseASTType (AST.LongScalar  _) = return SpecLong

-- MethodSpecIR Translation {{{1

-- MethodSpec translation common defines {{{2

-- | Java expression initial value.
data SpecJavaRefInitialValue
  = RIVArrayConst JSS.Type -- ^ Java symbolic simulator type.
                  (V.Vector CValue) -- ^ Value of array elements.
                  DagType -- ^ Type of array at symbolic level.
  | RIVClass JSS.Class
  | RIVIntArray !Int
  | RIVLongArray !Int
  deriving (Show)

instance Eq SpecJavaRefInitialValue where
  RIVArrayConst tp1 v1 _ == RIVArrayConst tp2 v2 _ = tp1 == tp2 && v1 == v2
  RIVClass c1 == RIVClass c2 = className c1 == className c2
  RIVIntArray l1 == RIVIntArray l2 = l1 == l2
  RIVLongArray l1 == RIVLongArray l2 = l1 == l2
  _ == _ = False

data SpecExprPostcondition
  = PostUnchanged
  | PostArbitrary DagType
  | PostResult TC.MixedExpr
  deriving (Show)

type JavaExprEquivClass = [TC.JavaExpr]

ppJavaExprEquivClass :: JavaExprEquivClass -> String
ppJavaExprEquivClass [] = error "internal: ppJavaExprEquivClass"
ppJavaExprEquivClass [expr] = show expr
ppJavaExprEquivClass cl = "{ " ++ intercalate ", " (map show (sort cl)) ++ " }"

-- LocalSpecs {{{2

data LocalSpecs = LocalSpecs {
    assumptions :: [TC.LogicExpr]
  , instanceFieldPostconditions :: [(TC.JavaExpr, JSS.FieldId, SpecExprPostcondition)]
  , arrayPostconditions :: Map TC.JavaExpr SpecExprPostcondition
  } deriving (Show)

emptyLocalSpecs :: LocalSpecs
emptyLocalSpecs = LocalSpecs [] [] Map.empty

-- MethodSpecTranslation immediate state {{{2

-- | Method spec translator state
-- N.B. keys in refTypeMap and constExprMap are disjoint.
data MethodSpecTranslatorState = MSTS {
         mstsOpCache :: OpCache
         -- | Position of method spec decalaration.
       , mstsPos :: Pos
       , mstsGlobalBindings :: TC.GlobalBindings
         -- | Class we are currently parsing.
       , specClass :: JSS.Class
       , mstsMethod :: JSS.Method
       -- | Expressions seen in var or const expressions.
       , seenJavaExprs :: Set TC.JavaExpr
         -- | Maps Java expressions referenced in type expressions
         -- to their associated type.
       , refTypeMap :: Map TC.JavaExpr SpecJavaRefInitialValue
         -- | Maps Java references to to associated constant expression.
       , constExprMap :: Map TC.JavaExpr (CValue,DagType)
       -- | Maps Java ref expression to associated equivalence class.
       , mayAliasRefs :: Map TC.JavaExpr JavaExprEquivClass
       -- | List of mayAlias classes in reverse order that they were created.
       , revAliasSets :: [(JavaExprEquivClass, SpecJavaRefInitialValue)]
       -- | Maps let bindings already seen to position they were defined.
       , definedLetBindingMap :: Map String Pos
       -- | List of let bindings encountered in reverse order.
       , reversedLetBindings :: [(String, TC.MixedExpr)]
       -- | Lift of assumptions parsed so far in reverse order.
       , currentAssumptions :: [TC.LogicExpr]
       -- | Set of expressions that have had ensures expressions declared.
       , ensuredExprs :: Set TC.JavaExpr
       -- | Map from Java expressions to typed expression in ensures clause.
       -- or nothing if an arbitrary expression for term has been given.
       , scalarEnsures :: Map TC.JavaExpr (Pos,SpecExprPostcondition)
       -- | Map from Java expressions to typed expression in ensures clause.
       -- or nothing if an arbitrary expression for term has been given.
       , mstsArrayPostconditions :: Map TC.JavaExpr (Pos, SpecExprPostcondition)
       -- | List of local specs parsed so far in reverse order.
       , currentLocalSpecs :: Map PC LocalSpecs
       -- | Return value found during resolution.
       , currentReturnValue :: Maybe (Pos, TC.MixedExpr)
       -- Verification method chosen.
       , verificationTactics :: Maybe (Pos, [AST.VerificationTactic])
       }

type MethodSpecTranslator = StateT MethodSpecTranslatorState IO

instance JSS.HasCodebase MethodSpecTranslator where
  getCodebase = gets (TC.codeBase . mstsGlobalBindings)

checkJSSTypeIsRef :: MonadIO m => Pos -> JSS.Type -> m ()
checkJSSTypeIsRef _ (ArrayType _) = return ()
checkJSSTypeIsRef _ (ClassType _) = return ()
checkJSSTypeIsRef pos tp =
  let msg = "SAWScript only requires reference types to be annotated "
            ++ "with type information, and currently only supports "
            ++ "methods with array and reference values as arguments.  "
            ++ "The type " ++ show tp ++ " is not a reference type."
      res = "Please modify the Java code to only use int or long array "
            ++ "types."
   in throwIOExecException pos (ftext msg) res

-- | Check that the reference type is not mentioned in a constant declaration.
checkConstUndefined :: Pos -> TC.JavaExpr -> String -> MethodSpecTranslator ()
checkConstUndefined pos ref note = do
  m <- gets constExprMap
  when (Map.member ref m) $
    let msg = "The Java expression \'" ++ show ref
              ++ "\' was previously used in a const declaration.  "  ++ note
     in throwIOExecException pos (ftext msg) ""

-- | Check that the Java expression type is undefined.
checkTypeIsUndefined :: Pos -> TC.JavaExpr -> String -> MethodSpecTranslator ()
checkTypeIsUndefined pos ref note = do
  s <- gets seenJavaExprs
  when (Set.member ref s) $
    let msg = "The type of the Java expresssion \'" ++ show ref
                ++ "\' has been already defined.  " ++ note
     in throwIOExecException pos (ftext msg) ""

-- | Check that a type declaration has been provided for this expression.
checkJavaTypeIsDefined :: Pos -> TC.JavaExpr -> MethodSpecTranslator ()
checkJavaTypeIsDefined pos javaExpr = do
  s <- gets seenJavaExprs
  unless (Set.member javaExpr s) $
    let msg = "The type of " ++ show javaExpr ++ " has not been defined."
        res = "Please add a \'type\' declaration to indicate the concrete type of this Java expression."
     in throwIOExecException pos (ftext msg) res

-- | Throw io exception indicating that a reference type is incompatible with
-- an expression type.
throwIncompatibleExprType :: MonadIO m => Pos -> String -> JSS.Type -> String -> m ()
throwIncompatibleExprType pos lhsExpr refType specTypeName =
  let msg = "The type of " ++ lhsExpr ++ " is " ++ show refType
             ++ ", which is incompatible with the specification type "
             ++ specTypeName ++ "."
   in throwIOExecException pos (ftext msg) ""

javaExprDefinedType :: MethodSpecTranslatorState -> TC.JavaExpr -> Maybe TC.DefinedJavaExprType
javaExprDefinedType msts e = do
   case Map.lookup e (refTypeMap msts) of
     Just (RIVArrayConst _ _ tp) -> Just (TC.DefinedType tp)
     Just (RIVClass cl) -> Just (TC.DefinedClass cl)
     Just (RIVIntArray l) ->
       let arrayTp = SymArray (constantWidth (Wx l)) (SymInt (constantWidth 32))
        in Just (TC.DefinedType arrayTp)
     Just (RIVLongArray l) ->
       let arrayTp = SymArray (constantWidth (Wx l)) (SymInt (constantWidth 64))
        in Just (TC.DefinedType arrayTp)
     Nothing ->
       case Map.lookup e (constExprMap msts) of
         Nothing ->
           case getJSSTypeOfJavaExpr e of
             IntType -> Just (TC.DefinedType (SymInt (constantWidth 32)))
             LongType -> Just (TC.DefinedType (SymInt (constantWidth 64)))
             _ -> Nothing
         Just (_,tp) -> Just (TC.DefinedType tp)

-- | Typecheck expression at global level.
methodParserConfig :: MethodSpecTranslatorState -> TC.TCConfig
methodParserConfig msts =
  TC.TCC { TC.opCache = mstsOpCache msts
         , TC.globalBindings = mstsGlobalBindings msts
         , TC.methodInfo = Just (mstsMethod msts, specClass msts)
         , TC.localBindings = Map.fromList (reversedLetBindings msts)
         , TC.toJavaExprType = Just (javaExprDefinedType msts)
         }

-- | Typecheck expression at global level.
typecheckJavaExpr :: AST.Expr -> MethodSpecTranslator TC.JavaExpr
typecheckJavaExpr astExpr = do
  msts <- get
  lift $ TC.tcJavaExpr (methodParserConfig msts) astExpr

-- | Typecheck expression at global level.
typecheckLogicExpr :: AST.Expr -> MethodSpecTranslator TC.LogicExpr
typecheckLogicExpr astExpr = do
  msts <- get
  lift $ TC.tcExpr (methodParserConfig msts) astExpr

-- Check that the Java spec reference has a type compatible with typedExpr.
checkJavaExprCompat :: Pos -> String -> JSS.Type -> DagType -> MethodSpecTranslator ()
checkJavaExprCompat pos exprName exprType dagType = do
  case (exprType, dagType) of
    (BooleanType, SymBool) ->
      let msg = "The type of \'" ++ exprName ++ "\' is in the Java \'Boolean\' type "
                 ++ ", which internally to the JVM is treated as a 32-bit integer, and not "
                 ++ " a \'Boolean\' predicate as in traditional mathematics."
          res = "Please modify the expression to denote a 32-bit integer."
       in throwIOExecException pos (ftext msg) res
    (BooleanType, _) | dagType == int32DagType -> return ()
    (IntType, _) | dagType == int32DagType -> return ()
    (LongType, _) | dagType == int64DagType -> return ()
    (ArrayType IntType, SymArray (widthConstant -> Just _) eltTp)
      | eltTp == int32DagType -> return ()
    (ArrayType LongType, SymArray (widthConstant -> Just _) eltTp)
      | eltTp == int64DagType -> return ()
    (_, specType) -> throwIncompatibleExprType pos exprName exprType (ppType specType)

-- | Check that no 'ensures' or 'arbitrary' statement has been added for the
-- given reference is undefined.
checkPostconditionUndefined :: Pos -> TC.JavaExpr -> MethodSpecTranslator ()
checkPostconditionUndefined pos expr = do
  exprSet <- gets ensuredExprs
  when (Set.member expr exprSet) $ do
    let msg = "Multiple postconditions defined for " ++ show expr ++ "."
    throwIOExecException pos (ftext msg) ""

-- | Returns equivalence class of Java expression in translator step.
getJavaEquivClass :: TC.JavaExpr -> MethodSpecTranslator JavaExprEquivClass
getJavaEquivClass expr = do
  Map.findWithDefault [expr] expr <$> gets mayAliasRefs

-- | Check that no 'ensures' or 'arbitrary' statement has been added for the
-- given reference is undefined.
checkArrayPostconditionUndefined :: Pos -> TC.JavaExpr -> MethodSpecTranslator ()
checkArrayPostconditionUndefined pos expr = do
  equivClass <- getJavaEquivClass expr
  exprSet <- gets mstsArrayPostconditions
  when (any (flip Map.member exprSet) equivClass) $ do
    let msg = "Multiple postconditions defined for " ++ show expr ++ "."
    throwIOExecException pos (ftext msg) ""

-- | Get type assigned to SpecJavaRef, or throw exception if it is not assigned.
lookupRefType :: Pos -> TC.JavaExpr -> MethodSpecTranslator SpecJavaRefInitialValue
lookupRefType pos ref = do
  m <- gets refTypeMap
  case Map.lookup ref m of
    Just tp -> return tp
    Nothing ->
      let msg = "The type of " ++ show ref ++ "must be specified "
                  ++ "before referring to it or one of its fields."
          res = "Please add a \'type\' declaration for this expression before this refence."
       in throwIOExecException pos (ftext msg) res

-- | Check that a reference is unaliased.
checkRefIsUnaliased :: Pos -> TC.JavaExpr -> String -> MethodSpecTranslator ()
checkRefIsUnaliased pos ref res = do
  s <- gets mayAliasRefs
  when (Map.member ref s) $ do
    let msg = "\'" ++ show ref ++ "\'was previously mentioned in a mayAlias"
              ++ " declaration."
    throwIOExecException pos (ftext msg) res

-- | Check that Java expression is of a type that can be assigned.
checkJavaExprIsModifiable :: Pos -> TC.JavaExpr -> String -> MethodSpecTranslator ()
checkJavaExprIsModifiable pos expr declName =
  case TC.getJSSTypeOfJavaExpr expr of
    JSS.ClassType _ ->
      let msg = declName ++ " declaration given a constant value " ++ show expr
                ++ "SAWScript currently requires constant values are unmodified by method calls."
          res = "Please modify the spec and/or Java code so that it does not "
                ++ "modify a reference value."
       in throwIOExecException pos (ftext msg) res
    _ -> return ()

-- | Add ensures statement mappping Java expr to given postcondition.
addEnsures :: Pos
           -> TC.JavaExpr
           -> SpecExprPostcondition
           -> MethodSpecTranslator ()
addEnsures pos javaExpr post = do
  equivClass <- getJavaEquivClass javaExpr
  modify $ \s ->
    s { ensuredExprs = Set.insert javaExpr (ensuredExprs s)
      , scalarEnsures = Map.insert javaExpr (pos, post) (scalarEnsures s)
      }

-- | Parses arguments from Array expression and returns JavaExpression.
-- throws exception if expression cannot be parsed.
parseArrayPostconditionExpr :: Pos -> [AST.Expr] -> MethodSpecTranslator (TC.JavaExpr, DagType)
parseArrayPostconditionExpr pos [astExpr] = do
  expr <- typecheckJavaExpr astExpr
  -- Typecheck type is defined.
  checkJavaTypeIsDefined pos expr
  -- Typecheck post condition is undefined.
  checkArrayPostconditionUndefined pos expr
  -- Typecheck this is an array.
  javaEltType <- 
    case TC.getJSSTypeOfJavaExpr expr of
      ArrayType tp -> return tp
      _ -> let msg = "Type of " ++ show expr ++ " is not an array."
            in throwIOExecException pos (ftext msg) ""
  -- Get expected element type.
  let throwRefUnsupported =
        let msg = "SAWScript does not support specifying the value of arrays of references."
         in throwIOExecException pos (ftext msg) ""
  let throwFloatUnsupported =
        let msg = "SAWScript does not support specifying the value of arrays of floats."
         in throwIOExecException pos (ftext msg) ""
  expectedEltType <-
    case javaEltType of
      ArrayType _ -> throwRefUnsupported 
      BooleanType -> return int32DagType
      ByteType    -> return int32DagType
      CharType    -> return int32DagType
      ClassType _ -> throwRefUnsupported
      DoubleType  -> throwFloatUnsupported
      FloatType   -> throwFloatUnsupported
      IntType     -> return int32DagType
      LongType    -> return int64DagType
      ShortType   -> return int32DagType
  return (expr, expectedEltType)
parseArrayPostconditionExpr pos args =
  let msg = "Unexpected number of arguments to \"array\"."
   in throwIOExecException pos (ftext msg) ""

setArrayPostcondition :: Pos -> TC.JavaExpr -> SpecExprPostcondition -> MethodSpecTranslator ()
setArrayPostcondition pos expr post =
  modify $ \s ->
    s { mstsArrayPostconditions = Map.insert expr (pos, post) (mstsArrayPostconditions s) }

-- | Code for parsing a method spec declaration.
resolveDecl :: AST.MethodSpecDecl -> MethodSpecTranslator ()
resolveDecl (AST.Type pos astExprs astTp) = do
  specType <- parseASTType astTp
  forM_ astExprs $ \astExpr -> do
    javaExpr <- typecheckJavaExpr astExpr
    -- Check type has not already been assigned.
    checkTypeIsUndefined pos javaExpr $
      "Multiple type declarations on the same Java expression "
        ++ "are not allowed."
    -- Check type is not a const.
    checkConstUndefined pos javaExpr $
       "Type declarations and const declarations on the same Java expression "
         ++ "are not allowed."
    let javaExprType = TC.getJSSTypeOfJavaExpr javaExpr
        tgtType = getJSSTypeOfSpecJavaType specType
    -- Check that type of ref and the type of tp are compatible.
    b <- JSS.isSubtype tgtType javaExprType
    unless b $
      throwIncompatibleExprType pos (show javaExpr) javaExprType (ppSpecJavaType specType)
    modify $ \s ->
      let s' = s { seenJavaExprs = Set.insert javaExpr (seenJavaExprs s) }
       in case specType of
            SpecRefClass cl -> s' { refTypeMap = Map.insert javaExpr (RIVClass cl)    (refTypeMap s') }
            SpecIntArray l  -> s' { refTypeMap = Map.insert javaExpr (RIVIntArray l)  (refTypeMap s') }
            SpecLongArray l -> s' { refTypeMap = Map.insert javaExpr (RIVLongArray l) (refTypeMap s') }
            _ -> s'
resolveDecl (AST.MayAlias _ []) = error "internal: mayAlias set is empty"
resolveDecl (AST.MayAlias pos astRefs) = do
  let tcASTJavaRef astRef = do
        ref <- typecheckJavaExpr astRef
        lift $ checkJSSTypeIsRef pos (TC.getJSSTypeOfJavaExpr ref)
        checkPostconditionUndefined pos ref
        return ref
  refs@(firstRef:restRefs) <- mapM tcASTJavaRef astRefs
  -- Check types of references are the same. are the same.
  firstType <- lookupRefType pos firstRef
  forM_ restRefs $ \r -> do
    restType <- lookupRefType pos r
    unless (firstType  == restType) $
      let msg = "The type assigned to " ++ show r
                  ++ " differs from the type assigned "
                  ++ show r ++ "."
          res = "All references that may alias must be assigned the same type."
       in throwIOExecException pos (ftext msg) res
  -- Check refs have not already appeared in a mayAlias set.
  do forM_ refs $ \ref -> do
       checkRefIsUnaliased pos ref $
         "Please merge mayAlias declarations as needed so that each "
           ++ "reference is mentioned at most once."
       checkConstUndefined pos ref $
         "Java expressions appearing in const declarations may not be aliased."
  -- Add mayAlias to state.
  modify $ \s ->
     s { mayAliasRefs = mapInsertKeys refs refs (mayAliasRefs s)
       , revAliasSets = (refs,firstType) : revAliasSets s }
resolveDecl (AST.Const pos astJavaExpr astValueExpr) = do
  -- Typecheck and validate javaExpr.
  javaExpr <- typecheckJavaExpr astJavaExpr
  checkTypeIsUndefined pos javaExpr $
    "Type declarations and const declarations on the same Java expression are not allowed."
  checkConstUndefined pos javaExpr $
    "Multiple const declarations on the same Java expression are not allowed."
  -- Parse expression (must be global since this is a constant.
  valueExpr <- do
    oc <- gets mstsOpCache
    bindings <- gets mstsGlobalBindings
    let config = TC.mkGlobalTCConfig oc bindings Map.empty
    lift $ TC.tcExpr config astValueExpr
  let val = TC.globalEval valueExpr
  let tp = TC.getTypeOfExpr valueExpr
  -- Check ref and expr have compatible types.
  checkJavaExprCompat pos (show javaExpr) (TC.getJSSTypeOfJavaExpr javaExpr) tp
  -- Add ref to refTypeMap
  modify $ \s -> s { constExprMap = Map.insert javaExpr (val,tp) (constExprMap s)
                   , seenJavaExprs = Set.insert javaExpr (seenJavaExprs s) }
resolveDecl (AST.MethodLet pos name astExpr) = do
  -- Check var is not already bound within method.
  do locals <- gets definedLetBindingMap
     case Map.lookup name locals of
       Just prevPos -> throwNameAlreadyDefined pos prevPos name
       Nothing -> return ()
  expr <- typecheckLogicExpr astExpr
  -- Add binding to let bindings
  modify $ \s ->
    s { definedLetBindingMap = Map.insert name pos (definedLetBindingMap s)
      , reversedLetBindings = (name,LE expr) : reversedLetBindings s }
resolveDecl (AST.Assume _pos astExpr) = do
  --TODO: Check expression has correct type.
  expr <- typecheckLogicExpr astExpr
  modify $ \s -> s { currentAssumptions = expr : currentAssumptions s }
resolveDecl (AST.Ensures pos (AST.ApplyExpr apos "valueOf" args) astValueExpr) = do
  -- Get Java expression and element type in Java array.
  (javaExpr, expectedEltType) 
    <- parseArrayPostconditionExpr apos args
  -- Get value expression and type.
  valueExpr <- typecheckLogicExpr astValueExpr
  let exprType = TC.getTypeOfExpr valueExpr
  exprEltType
     <- case exprType of
          SymArray (widthConstant -> Just _) tp -> return tp
          _ -> let msg = "Type of expression is not an array."
                in throwIOExecException pos (ftext msg) ""
  -- Check types compatibility
  unless (expectedEltType == exprEltType) $ do
    throwIncompatibleExprType pos 
                              ("array(" ++ show javaExpr ++ ")")
                              (TC.getJSSTypeOfJavaExpr javaExpr)
                              (ppType exprType)
  -- Update final type.
  setArrayPostcondition pos javaExpr (PostResult (LE valueExpr))
resolveDecl (AST.Ensures pos astJavaExpr astValueExpr) = do
  liftIO $ putStrLn "Running non-array ensures"
  msts <- get
  javaExpr <- liftIO $ TC.tcJavaExpr (methodParserConfig msts) astJavaExpr
  checkJavaTypeIsDefined pos javaExpr
  checkJavaExprIsModifiable pos javaExpr "\'ensures\'"
  checkPostconditionUndefined pos javaExpr
  -- Resolve astValueExpr
  valueExpr <- typecheckLogicExpr astValueExpr
  -- Check javaExpr and valueExpr have compatible types.
  let javaExprType = TC.getJSSTypeOfJavaExpr javaExpr
      valueExprType = TC.getTypeOfExpr valueExpr
  checkJavaExprCompat pos (show javaExpr) javaExprType valueExprType
  addEnsures pos javaExpr (PostResult (LE valueExpr))
resolveDecl (AST.Modifies pos astExprs) = do
  let resolveExpr (AST.ApplyExpr apos "valueOf" args) = do
        (javaExpr,eltType) <- parseArrayPostconditionExpr apos args
        setArrayPostcondition pos javaExpr (PostArbitrary eltType)
      resolveExpr astExpr = do
        javaExpr <- typecheckJavaExpr astExpr
        let throwIsReference =
              let msg = "Modifies postconditions applied to references may not be applied to reference types."
               in throwIOExecException pos (ftext msg) ""
        case TC.getJSSTypeOfJavaExpr javaExpr of
          JSS.ArrayType _ -> throwIsReference
          JSS.ClassType _ -> throwIsReference
          _ -> return ()
        checkJavaTypeIsDefined pos javaExpr
        checkPostconditionUndefined pos javaExpr
        msts <- get
        case javaExprDefinedType msts javaExpr of
          Just (TC.DefinedType tp) ->
            addEnsures pos javaExpr (PostArbitrary tp)
          _ -> error "internal: resolveDecl Modifies given bad javaExpr"
  mapM_ resolveExpr astExprs
resolveDecl (AST.Returns pos astValueExpr) = do
  -- Check return value is undefined.
  do rv <- gets currentReturnValue
     case rv of
       Nothing -> return ()
       Just (absPrevPos, _) -> do
         relPos <- liftIO $ posRelativeToCurrentDirectory absPrevPos
         let msg = "Multiple return values specified in a single method spec.  The "
                   ++ "previous return value was given at " ++ show relPos ++ "."
         throwIOExecException pos (ftext msg) ""
  -- Resolve astValueExpr
  valueExpr <- typecheckLogicExpr astValueExpr
  -- Check javaExpr and valueExpr have compatible types.
  method <- gets mstsMethod
  case JSS.methodReturnType method of
    Nothing ->
      let msg = "Return value specified for \'" ++ JSS.methodName method ++ "\', but "
                 ++ "method returns \'void\'."
       in throwIOExecException pos (ftext msg) ""
    Just returnType ->
      let valueExprType = TC.getTypeOfExpr valueExpr
       in checkJavaExprCompat pos "the return value" returnType valueExprType
  -- Update state with return value.
  modify $ \s -> s { currentReturnValue = Just (pos, LE valueExpr) }
resolveDecl (AST.LocalSpec _pos pc specs) = do
  s <- get
  let initState = s { currentAssumptions = []
                    , ensuredExprs = Set.empty
                    , scalarEnsures = Map.empty
                    , mstsArrayPostconditions = Map.empty
                    }
  locState <- lift $ execStateT (mapM_ resolveDecl specs) initState
  let locSpecs = mkLocalSpecs locState
  put $ s { currentLocalSpecs =
              Map.insert (fromIntegral pc) locSpecs (currentLocalSpecs s) }
resolveDecl (AST.VerifyUsing pos tactics) = do
  -- Check verification method has not been assigned.
  vm <- gets verificationTactics
  case vm of
   Nothing -> return ()
   Just (_oldPos,_) ->
     let msg = "The verification tactic is set multiple times in the same "
                ++ "method specification."
         res = "Please include at most one verification tactic in a single specification."
      in throwIOExecException pos (ftext msg) res
  case tactics of
    [AST.Skip] -> return ()
    [AST.Rewrite] -> return ()
    [AST.QuickCheck _ _] -> return ()
    [AST.ABC] -> return ()
    [AST.Rewrite, AST.ABC] -> return ()
    [AST.SmtLib {}] -> return ()
    [AST.Rewrite, AST.SmtLib {}] -> return ()
    [AST.Yices _] -> return ()
    [AST.Rewrite, AST.Yices _] -> return ()
    _ -> let defList args = nest 2 (vcat (map (\(d,m) -> quotes (text d) <> char '.' <+> text m) args))
             msg = ftext "The tactic specified in verifyUsing is unsupported." 
                   <+> ftext "SAWScript currently supports the following tactics:\n"
                   $+$ defList [ ("skip",       "Skip verification of this method.")
                               , ("rewriter",   "Applies rewriting with the currently enabled rules.")
                               , ("quickcheck", "Uses quickcheck testing to validate, but not verify method.")
                               , ("abc",           "Uses abc to verify proof obligations.")
                               , ("rewriter, abc", "Uses rewriting to simplify proof obligations, and uses abc to discharge result.")
                               , ("smtLib [name]", "Generates SMTLib file with the given name that can be discharged independently.")
                               , ("rewriter, smtLib [name]", "Uses rewriting to simplify proof obligations, and generates smtLib file with result.")
                               , ("yices [version]", "Generates SMTLib and runs Yices to discharge goals.")
                               , ("rewriter, yices [version]", "Uses rewriting to simplify proof obligations, and uses Yices to discharge result.")
                               ]
          in throwIOExecException pos msg ""
  -- Assign verification method.
  modify $ \s -> s { verificationTactics = Just (pos, tactics) }

-- MethodSpecIR {{{2

data MethodSpecIR = MSIR {
    methodSpecPos :: Pos
  , methodSpecIRThisClass :: JSS.Class
    -- | Class where method is defined.
  , methodSpecIRMethodClass :: JSS.Class
    -- | Method to verify.
  , methodSpecIRMethod :: JSS.Method
    -- | Class names expected to be initialized using JVM "/" separators.
    -- (as opposed to Java "." path separators).
  , initializedClasses :: [String]
    -- | References in specification with alias information and reference info.
  , specReferences :: [(JavaExprEquivClass, SpecJavaRefInitialValue)]
    -- | List of non-reference input variables that must be available.
  , specScalarInputs :: [TC.JavaExpr]
    -- | List of constants expected in input.
  , specConstants :: [(TC.JavaExpr,CValue,DagType)]
    -- | Let bindings
  , methodLetBindings :: [(String, TC.MixedExpr)]
  -- | Local specifications for method.
  , localSpecs :: Map PC LocalSpecs
    -- | Specification at return.
  , returnSpec :: LocalSpecs
  -- | Return value if any (is guaranteed to be compatible with method spec).
  , returnValue :: Maybe TC.MixedExpr
  -- | Verification method for method.
  , methodSpecVerificationTactics :: [AST.VerificationTactic]
  } deriving (Show)

-- | Return user printable name of method spec (currently the class + method name).
methodSpecName :: MethodSpecIR -> String
methodSpecName ir =
 let clName = className (methodSpecIRThisClass ir)
     mName = methodName (methodSpecIRMethod ir)
  in slashesToDots clName ++ ('.' : mName)

-- | Returns all Java expressions referenced in specification.
methodSpecJavaExprs :: MethodSpecIR -> [TC.JavaExpr]
methodSpecJavaExprs ir = concat (map fst (specReferences ir)) ++ specScalarInputs ir

isScalarType :: JSS.Type -> Bool
isScalarType (ArrayType _) = False
isScalarType BooleanType = True
isScalarType ByteType = True
isScalarType CharType = True
isScalarType (ClassType _) = False
isScalarType DoubleType = error "internal: floating point is unsupported"
isScalarType FloatType = error "internal: floating point is unsupported"
isScalarType IntType = True
isScalarType LongType = True
isScalarType ShortType = True

mkLocalSpecs :: MethodSpecTranslatorState -> LocalSpecs
mkLocalSpecs st =
  LocalSpecs {
      assumptions = currentAssumptions st
    , instanceFieldPostconditions =
        [ ( r
          , f
          , case Map.lookup expr (scalarEnsures st) of
              Nothing -> PostUnchanged
              Just (_pos,cond) -> cond
          )
        | expr@(TC.InstanceField r f) <- Set.toList (seenJavaExprs st) ]
    , arrayPostconditions = Map.map snd (mstsArrayPostconditions st)
    }

-- | Interprets AST method spec commands to construct an intermediate
-- representation that
resolveMethodSpecIR :: OpCache
                    -> TC.GlobalBindings
                    -> Pos
                    -> JSS.Class
                    -> String
                    -> [AST.MethodSpecDecl]
                    -> IO MethodSpecIR
resolveMethodSpecIR oc gb pos thisClass mName cmds = do
  let st = MSTS { mstsOpCache = oc
                , mstsPos = pos
                , mstsGlobalBindings = gb
                , specClass = thisClass
                , mstsMethod = undefined
                , seenJavaExprs = Set.empty
                , refTypeMap = Map.empty
                , constExprMap = Map.empty
                , mayAliasRefs = Map.empty
                , revAliasSets = []
                , definedLetBindingMap = Map.empty
                , reversedLetBindings = []
                , currentAssumptions = []
                , ensuredExprs = Set.empty
                , scalarEnsures = Map.empty
                , mstsArrayPostconditions = Map.empty
                , currentLocalSpecs = Map.empty
                , currentReturnValue = Nothing
                , verificationTactics = Nothing
                }
  flip evalStateT st $ do
    -- Initialize necessary values in translator state.
    (methodClass,method) <- findMethod pos mName thisClass
    modify $ \s -> s { mstsMethod = method }
    unless (methodIsStatic method) $
      modify $ \s ->
        s { refTypeMap =
              Map.singleton (TC.This (className thisClass)) (RIVClass thisClass) }
    -- Perform resolution
    mapM_ resolveDecl cmds
    -- Get final state
    st' <- get
    -- Get list of initial superclasses.
    superClasses <- JSS.supers thisClass
    -- Check that each declaration of a field does not have the base
    -- object in the mayAlias class.
    let allRefs = Map.keysSet (refTypeMap st') `Set.union` Map.keysSet (constExprMap st')
    let checkRef (TC.This _) = return ()
        checkRef (TC.Arg _ _) = return ()
        checkRef (TC.InstanceField lhs f) = do
          when (Map.member lhs (mayAliasRefs st')) $
            let msg = "This specification contains a mayAlias declaration "
                     ++ "containing \'" ++ show lhs ++ "\' and an additional "
                     ++ "declaration that references its field \'"
                     ++ JSS.fieldIdName f ++ "\'.  The current SAWScript "
                     ++ "implementation does not support this."
                res = "Please remove the mayAlias declaration of \'" ++ show lhs
                     ++ "\' and alter the Java code as needed."
             in throwIOExecException pos (ftext msg) res
          checkRef lhs
    mapM_ checkRef (Set.toList allRefs)
    -- Define specReferences
    let unaliasedRefs
          = map (\(r,tp) -> ([r], tp))
          $ filter (\(r,_) -> Map.notMember r (mayAliasRefs st'))
          $ Map.toList (refTypeMap st')
    let constRefs
          = catMaybes
          $ map (\(r,(c,tp)) ->
                   let javaTp = TC.getJSSTypeOfJavaExpr r
                    in case (javaTp,c) of
                         (ArrayType _,CArray v) -> Just ([r], RIVArrayConst javaTp v tp)
                         _ -> Nothing)
          $ Map.toList (constExprMap st')
    let specReferences = revAliasSets st' ++ unaliasedRefs ++ constRefs
    --TODO: Validate that types or constants for all arguments have been provided.
    -- Get specConstants
    let specConstants
          = catMaybes
          $ map (\(r,(c,tp)) ->
                   case tp of
                     SymInt _ -> Just (r, c, tp)
                     _ -> Nothing)
          $ Map.toList (constExprMap st')
    -- returnValue
    returnValue <-
      case JSS.methodReturnType method of
        Nothing -> return Nothing
        Just _ -> do
          let throwUndefinedReturnValue =
                let msg = "The Java method \'" ++ methodName method
                          ++ "\' has a return value, but the spec does not define it."
                 in throwIOExecException pos (ftext msg) ""
          maybe throwUndefinedReturnValue (return . Just . snd) $
                (currentReturnValue st')
    -- Get verification method.
    tactics <-
      case verificationTactics st' of
        Just (_,tactics) -> return tactics
        Nothing -> 
          let msg = "The verification method for \'" ++ methodName method
                    ++ "\' is undefined."
              res = "Please specify a verification method.  Use \'skip\' to skip verification."
           in throwIOExecException pos (ftext msg) res
    -- Return IR.
    return MSIR { methodSpecPos = pos
                , methodSpecIRThisClass = thisClass
                , methodSpecIRMethodClass = methodClass
                , methodSpecIRMethod = method
                , initializedClasses = map className superClasses
                , specReferences
                , specScalarInputs = filter (isScalarType . getJSSTypeOfJavaExpr)
                                   $ Set.toList (seenJavaExprs st')
                , specConstants
                , methodLetBindings = reverse (reversedLetBindings st')
                , returnSpec = mkLocalSpecs st'
                , returnValue
                , localSpecs = currentLocalSpecs st'
                , methodSpecVerificationTactics = tactics
                }

-- JavaEvalContext {{{1

-- | Stores information about a particular Java state.
data JavaEvalContext n = JSI {
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
javaExprValue :: JavaEvalContext Node -> TC.JavaExpr -> Maybe (JSS.Value Node)
javaExprValue jec (TC.This _) =
  case jecThis jec of
    Just r -> Just (JSS.RValue r)
    Nothing -> error "internal: javaExprValue given TC.This for static method"
javaExprValue jec (TC.Arg i _) =
  CE.assert (i < V.length (jecArgs jec)) $
    Just (jecArgs jec V.! i)
javaExprValue jec (TC.InstanceField e f) = do
  JSS.RValue r <- javaExprValue jec e
  Map.lookup (r,f) (JSS.instanceFields (jecPathState jec))

-- SpecStateInfo {{{1

-- | Provides information for evaluation expressions with respect to a
-- particular Java state.
data SpecStateInfo n = SSI {
         ssiJavaEvalContext :: JavaEvalContext n
        -- | Maps names appearing in let bindings to the corresponding Node.
       , ssiLetNodeBindings :: Map String (MixedValue n)
       }

-- | Create spec state info from a Java state info and method spec IR.
createSpecStateInfo :: DagEngine Node Lit 
                    -> MethodSpecIR
                    -> JavaEvalContext Node
                    -> SpecStateInfo Node
createSpecStateInfo de ir jec =
  flip execState initialState $
    forM_ (methodLetBindings ir) $ \(name,expr) -> do
      ssi <- get
      let n = evalMixedExpr de ssi expr
      modify $ \s -> s { ssiLetNodeBindings = Map.insert name n (ssiLetNodeBindings s) }
 where initialState = SSI jec Map.empty

-- | Evaluates a typed expression.
evalExpr :: DagEngine Node Lit -> SpecStateInfo Node -> TC.LogicExpr -> Node
evalExpr de ssi (TC.Apply op exprs) =
  deApplyOp de op (V.map (evalExpr de ssi) (V.fromList exprs))
evalExpr de _   (TC.Cns c tp) = deConstantTerm de c tp
evalExpr _  ssi (TC.ArrayValue javaExpr _) = n
  where jec = ssiJavaEvalContext ssi
        Just (JSS.RValue r) = javaExprValue jec javaExpr
        Just (_,n) = Map.lookup r (JSS.arrays (jecPathState jec))
evalExpr _  ssi (TC.Var name _tp) = do
  case Map.lookup name (ssiLetNodeBindings ssi) of
    Nothing -> error $ "internal: evalExpr given invalid variable " ++ name
    Just (MVNode n) -> n

-- | Value of mixed expression in a particular context.
data MixedValue n
   = MVNode n 
   | MVRef JSS.Ref

-- | Return value from expression.
-- TODO: Support references.
evalMixedExpr :: DagEngine Node Lit
              -> SpecStateInfo Node
              -> TC.MixedExpr
              -> MixedValue Node
evalMixedExpr de ssi (LE expr) = MVNode $ evalExpr de ssi expr

-- | Return Java value associated with mixed expression.
mixedExprValue :: DagEngine Node Lit -> SpecStateInfo Node -> TC.MixedExpr -> JSS.Value Node
mixedExprValue de ssi expr =
  case evalMixedExpr de ssi expr of
    MVRef r -> JSS.RValue r
    MVNode n ->
      case termType n of
        SymInt (widthConstant -> Just 32) -> JSS.IValue n
        SymInt (widthConstant -> Just 64) -> JSS.LValue n
        _ -> error "internal: mixedExprValue called with malformed result type."


-- Method specification overrides {{{1

-- | Returns value constructor from node.
jssTypeValueFn :: JSS.Type -> n -> JSS.Value n
jssTypeValueFn JSS.BooleanType = JSS.IValue
jssTypeValueFn JSS.IntType     = JSS.IValue
jssTypeValueFn JSS.LongType    = JSS.LValue
jssTypeValueFn _ = error "internal: illegal type"

jssTypeStackWidth :: JSS.Type -> BitWidth
jssTypeStackWidth (ArrayType _) = error "internal: jssTypeStackWidth given ArrayType."
jssTypeStackWidth BooleanType = 32
jssTypeStackWidth ByteType = 32
jssTypeStackWidth CharType = 32
jssTypeStackWidth (ClassType _) = error "internal: class type is unsupported."
jssTypeStackWidth DoubleType = error "internal: floating point is unsupported"
jssTypeStackWidth FloatType = error "internal: floating point is unsupported"
jssTypeStackWidth IntType = 32
jssTypeStackWidth LongType = 64
jssTypeStackWidth ShortType = 32

execOverride :: Pos
             -> String
             -> MethodSpecIR
             -> Maybe JSS.Ref
             -> [JSS.Value Node]
             -> JSS.Simulator SymbolicMonad ()
execOverride pos nm ir mbThis args = do
  -- Check Java expressions referenced in IR are defined in the path state.
  ps <- JSS.getPathState
  -- Create JavaEvalContext and SpecStateInfo from current simulator state.
  let jec = JSI { jecThis = mbThis
                , jecArgs = V.fromList args
                , jecPathState = ps
                , jecInitPC = 0
                }
  forM_ (methodSpecJavaExprs ir) $ \javaExpr -> do
    when (isNothing (javaExprValue jec javaExpr)) $ do
      let msg = "The override for \'" ++ methodSpecName ir
                  ++ "\' was called while symbolically simulating " ++ nm
                  ++ ".  However, the method specification of \'"
                  ++ methodSpecName ir ++ "\' requires that the value of \'"
                  ++ show javaExpr ++ "\' is defined."
          res = "Please add a \'var\' or \'const\' declaration as appropriate "
                  ++ "to the specification of \'" ++ nm
                  ++ "\' to define \'" ++ show javaExpr ++ "\'."
       in throwIOExecException pos (ftext msg) res
  de <- JSS.liftSymbolic getDagEngine
  let ssi = createSpecStateInfo de ir jec
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
  mapM_ (\e -> JSS.assume (evalExpr de ssi e)) (assumptions rs)
  -- Check references have correct type.
  liftIO $ do
    seenRefsIORef <- liftIO $ newIORef (Map.empty :: Map JSS.Ref TC.JavaExpr)
    forM_ (specReferences ir) $ \(ec, _iv) -> do
      seenRefs <- liftIO $ readIORef seenRefsIORef
      refs <- forM ec $ \javaExpr -> do
                let Just (JSS.RValue r) = javaExprValue jec javaExpr
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
          case javaExprValue jec javaExpr of
            Just (JSS.IValue n) -> n
            Just (JSS.LValue n) -> n
            Just (JSS.RValue r) ->
              let Just (_,n) = Map.lookup r (JSS.arrays (jecPathState jec)) in n
            Just _ -> error $ "internal: javaExprNode given an invalid expression \'" ++ show javaExpr ++ ".\'"
            Nothing -> error $ "internal: javaExprNode given an undefined expression \'" ++ show javaExpr ++ ".\'"
    JSS.assume =<< JSS.liftSymbolic (applyEq jvmNode =<< makeConstant c tp)
  let spec = returnSpec ir
  -- Update arrayPostconditions
  forM_ (Map.toList $ arrayPostconditions spec) $ \(javaExpr,pc) ->
    evalArrayPost de ssi javaExpr pc
  -- Update instance fields
  forM_ (instanceFieldPostconditions spec) $ \(refExpr,f,pc) ->
    evalInstanceFieldPost ssi refExpr f pc
  -- Update return type.
  case JSS.methodReturnType (methodSpecIRMethod ir) of
    Nothing -> return ()
    Just _ -> do
      let Just returnExpr = returnValue ir
      JSS.pushValue $ mixedExprValue de ssi returnExpr

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
              -> SpecStateInfo Node
              -> JavaExpr
              -> SpecExprPostcondition
              -> JSS.Simulator SymbolicMonad ()
evalArrayPost de ssi javaExpr pc =
  case pc of
    PostUnchanged -> return ()
    PostArbitrary tp ->
      JSS.setSymbolicArray r =<< liftIO (createSymbolicFromType de tp)
    PostResult (LE expr) ->
      JSS.setSymbolicArray r (evalExpr de ssi expr)
    PostResult (JE _) -> error "internal: Encountered Java expression in evalArrayPost"
  where Just (JSS.RValue r) = javaExprValue (ssiJavaEvalContext ssi) javaExpr

evalInstanceFieldPost :: SpecStateInfo Node
                      -> JavaExpr
                      -> JSS.FieldId
                      -> SpecExprPostcondition
                      -> JSS.Simulator SymbolicMonad ()
evalInstanceFieldPost ssi refExpr f pc = do
  let Just (JSS.RValue r) = javaExprValue (ssiJavaEvalContext ssi) refExpr
  let tp = JSS.fieldIdType f
  de <- JSS.liftSymbolic getDagEngine
  case pc of
    PostUnchanged -> return ()
    PostArbitrary _ -> do
      --TODO: May want to verify that tp is a scalar.
      n <- liftIO $ mkSymbolicInt de (jssTypeStackWidth tp)
      JSS.setInstanceFieldValue r f (jssTypeValueFn tp n)
    PostResult expr -> do
      JSS.setInstanceFieldValue r f (mixedExprValue de ssi expr)

-- MethodSpec verification {{{1
-- EquivClassMap {{{2
type EquivClassMap = (Int, Map Int [TC.JavaExpr], Map Int SpecJavaRefInitialValue)

-- | Return list of class indices to initial values.
equivClassMapEntries :: EquivClassMap
                     -> V.Vector (Int, [TC.JavaExpr], SpecJavaRefInitialValue)
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
    let -- create array input node with length and int width.
        createInputArrayNode l w = do
          -- Create input array node.
          n <- liftIO $ do
            let arrType = SymArray (constantWidth (Wx l)) (SymInt (constantWidth (Wx w)))
            let ?be = deBitEngine de
            lv <- V.replicateM l $ LV <$> SV.replicateM w lMkInput
            deFreshInput de (Just (LVN lv)) arrType
          -- Create Java reference for creating array node.
          ref <- lift $ JSS.newSymbolicArray (JSS.ArrayType JSS.IntType) (fromIntegral l) n
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
    case initValue of
      RIVArrayConst javaTp v tp -> do
        let n = deConstantTerm de (CArray v) tp
        let l = V.length v
        ref <- lift $ JSS.newSymbolicArray javaTp (fromIntegral l) n
        modify $ \s -> s
          { jvsExprValueMap = mapInsertKeys exprClass (JSS.RValue ref) (jvsExprValueMap s)
          , jvsRefNameMap = Map.insert ref refName (jvsRefNameMap s)
          , jvsArrayNodeList = (ref,exprClass,n):(jvsArrayNodeList s)
          }
      RIVClass cl -> do
        ref <- lift $ JSS.genRef (ClassType (className cl))
        modify $ \s -> s
          { jvsExprValueMap = mapInsertKeys exprClass (JSS.RValue ref) (jvsExprValueMap s)
          , jvsRefNameMap = Map.insert ref refName (jvsRefNameMap s)
          }
      RIVIntArray l ->  createInputArrayNode l 32
      RIVLongArray l -> createInputArrayNode l 64

createJavaEvalScalars :: MethodSpecIR -> JavaEvaluator ()
createJavaEvalScalars ir = do
  -- Create symbolic inputs from specScalarInputs.
  de <- lift $ JSS.liftSymbolic $ getDagEngine
  forM_ (specScalarInputs ir) $ \expr -> do
    litCount <- liftIO $ beInputLitCount (deBitEngine de)
    let addScalarNode node inputEval value =
          modify $ \s ->
            s { jvsExprValueMap = Map.insert expr value (jvsExprValueMap s)
              , jvsInputs =
                  VerificationInput
                     { viNode = node
                     , viEval = inputEval
                     , viExprs = [expr]
                     } : jvsInputs s
              }
    case TC.getJSSTypeOfJavaExpr expr of
      JSS.BooleanType -> do
        -- Treat JSS.Boolean as a 32-bit integer.
        n <- liftIO $ createSymbolicIntNode de 32
        let inputEval lits = mkCIntFromLsbfV $ SV.slice litCount 32 lits
        addScalarNode n inputEval (JSS.IValue n)
      JSS.IntType -> do
        n <- liftIO $ createSymbolicIntNode de 32
        let inputEval lits = mkCIntFromLsbfV $ SV.slice litCount 32 lits
        addScalarNode n inputEval (JSS.IValue n)
      JSS.LongType -> do
        n <- liftIO $ createSymbolicIntNode de 64
        let inputEval lits = mkCIntFromLsbfV $ SV.slice litCount 64 lits
        addScalarNode n inputEval (JSS.LValue n)
      _ -> error "internal: createSpecSymbolicInputs Illegal spectype."

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
    createJavaEvalScalars ir
    -- Set field values.
    do m <- gets jvsExprValueMap
       let fieldExprs = [ (r,f,v) | (TC.InstanceField r f,v) <- Map.toList m ]
       forM_ fieldExprs $ \(refExpr, f, v) -> do
         let Just (JSS.RValue r) = Map.lookup refExpr m
         lift $ JSS.setInstanceFieldValue r f v

-- Java execution {{{2

-- Run method and get final path state
runMethod :: MethodSpecIR
          -> JavaEvalContext Node
          -> JSS.Simulator SymbolicMonad [(JSS.PathDescriptor, JSS.FinalResult Node)]
runMethod ir jec = do
  let clName = className (methodSpecIRMethodClass ir)
  let method = methodSpecIRMethod ir
  let args = V.toList (jecArgs jec)
  if methodIsStatic method
    then JSS.invokeStaticMethod clName (methodKey method) args
    else do
      let Just thisRef = jecThis jec
      JSS.invokeInstanceMethod clName (methodKey method) thisRef args
  Sem.setPc (jecInitPC jec)
  JSS.run

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
                 -> SpecStateInfo Node
                 -> JSS.FinalResult Node
                 -> ExpectedStateDef
expectedStateDef de ir jvs ssi fr = do
  ESD { esdReturnValue = mixedExprValue de ssi <$> returnValue ir
      , esdInstanceFields = Map.fromList
          [ ( (ref,fid)
            , case cond of
                PostUnchanged -> instanceFieldValue ref fid (jecPathState jec)
                PostArbitrary _ -> Nothing -- arbitrary case
                PostResult expr -> Just $ mixedExprValue de ssi expr
            )
          | (refExpr,fid,cond) <- instanceFieldPostconditions spec
          , let Just (JSS.RValue ref) = javaExprValue jec refExpr
          ]
      , esdArrays = Map.fromList
          [ ( r
            , case mapLookupAny refEquivClass (arrayPostconditions spec) of
                Nothing -> Just initValue
                Just PostUnchanged -> Just initValue
                Just (PostArbitrary _) -> Nothing
                Just (PostResult (LE expr)) -> Just $ evalExpr de ssi expr
                Just (PostResult (JE _)) -> error "internal: illegal post result for array"
            )
          | (r,refEquivClass,initValue) <- jvsArrayNodeList jvs ]
      }
 where jec = ssiJavaEvalContext ssi
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
                  Node -- ^ Value returned by JVM symbolic simulator.
                  Node -- ^ Expected value in Spec.
  deriving (Eq, Ord, Show)

-- | Returns goal that one needs to prove.
checkGoal :: DagEngine Node Lit -> VerificationCheck -> Node
checkGoal _ (PathCheck n) = n
checkGoal de (EqualityCheck _ x y) = deApplyBinary de (eqOp (termType x)) x y

checkName :: VerificationCheck -> String
checkName (PathCheck _) = "the path condition"
checkName (EqualityCheck nm _ _) = nm

-- | Returns documentation for check that fails.
checkCounterexample :: VerificationCheck -> (Node -> CValue) -> Doc
checkCounterexample (PathCheck _) _ = text "The path conditions were unsatisfied."
checkCounterexample (EqualityCheck nm jvmNode specNode) evalFn =
  text nm $$
    nest 2 (text "Encountered: " <> ppCValueD Mixfix (evalFn jvmNode)) $$
    nest 2 (text "Expected:    " <> ppCValueD Mixfix (evalFn specNode))

-- | Returns assumptions in method spec.
methodAssumptions :: DagEngine Node Lit
                  -> MethodSpecIR
                  -> SpecStateInfo Node
                  -> Node
methodAssumptions de ir ssi = do
  let spec = case jecInitPC (ssiJavaEvalContext ssi) of
               0 -> returnSpec ir
               pc -> case Map.lookup pc (localSpecs ir) of
                       Nothing -> error $ "internal: no specification found for pc " ++ show pc
                       Just s -> s
   in foldl' (deApplyBinary de bAndOp) (mkCBool True) $
        map (evalExpr de ssi) (assumptions spec)

-- | Add verification condition to list.
addEqVC :: String -> Node -> Node -> State [VerificationCheck] ()
addEqVC name jvmNode specNode = do
  modify $ \l -> EqualityCheck name jvmNode specNode : l

-- | Compare old and new states.
comparePathStates :: MethodSpecIR
                  -> JavaVerificationState
                  -> ExpectedStateDef
                  -> JSS.PathState Node
                  -> Maybe (JSS.Value Node)
                  -> [VerificationCheck]
comparePathStates ir jvs esd newPathState mbRetVal =
  flip execState initialVCS $ do
    -- Check return value.
    case mbRetVal of
      Nothing -> return ()
      Just (JSS.IValue rv) -> do
        let Just (JSS.IValue expRetVal) = esdReturnValue esd
         in addEqVC "return value" rv expRetVal
      Just (JSS.LValue rv) -> 
        let Just (JSS.LValue expRetVal) = esdReturnValue esd
         in addEqVC "return value" rv expRetVal
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
          addEqVC fieldName jvmNode specNode
        (JSS.LValue jvmNode, Just (JSS.LValue specNode)) ->
          addEqVC fieldName jvmNode specNode
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
          addEqVC refName jvmNode specNode
 where pos = methodSpecPos ir
       mName = methodSpecName ir
       initialVCS  = [PathCheck (JSS.psAssumptions newPathState)]

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
      -- Create map from specification entries to JSS simulator values.
      jvs <- initializeJavaVerificationState ir cm
      -- JavaEvalContext for inital verification state.
      initialPS <- JSS.getPathState
      let specs = Map.findWithDefault emptyLocalSpecs pc (localSpecs ir)
          jec =
            -- TODO: does any of this need to be different for an
            -- intermediate starting state?
            let evm = jvsExprValueMap jvs
                mbThis = case Map.lookup (TC.This cls) evm of
                           Nothing -> Nothing
                           Just (JSS.RValue r) -> Just r
                           Just _ -> error "internal: Unexpected value for This"
                method = methodSpecIRMethod ir
                args = V.map (evm Map.!)
                     $ V.map (uncurry TC.Arg)
                     $ V.fromList
                     $ [0..] `zip` methodParameterTypes method
             in JSI { jecThis = mbThis
                    , jecArgs = args
                    , jecPathState = initialPS
                    , jecInitPC = pc 
                    }
      -- Add method spec overrides.
      mapM_ (overrideFromSpec pos (methodSpecName ir)) overrides
      -- Register breakpoints
      JSS.registerBreakpoints $ map (\bpc -> (cls, meth, bpc)) assertPCs
      when (vrb >= 6) $ do
         liftIO $ putStrLn $ "Executing " ++ methodSpecName ir
         when (pc /= 0) $
              liftIO $ putStrLn $ "  starting from PC " ++ show pc
      let ssi = createSpecStateInfo de ir jec
      -- Update local arrayPostconditions for starting PC
      forM_ (Map.toList $ arrayPostconditions specs) $ \(javaExpr,post) ->
        evalArrayPost de ssi javaExpr post
      -- Update local scalarPostconditions for starting PC
      forM_ (instanceFieldPostconditions specs) $ \(refExpr,f,post) ->
        evalInstanceFieldPost ssi refExpr f post
      -- Execute method.
      jssResult <- runMethod ir jec
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
                     _ -> methodSpecName ir
        -- Build final equation and functions for generating counterexamples.
        newPathState <- JSS.getPathStateByName ps
        JSS.liftSymbolic $ do
          let esd = expectedStateDef de ir jvs ssi fr
          liftIO $ when (vrb >= 6) $
            putStrLn $ "Creating verification conditions for " ++ name
          -- Create verification conditions from path states.
          let vcs = comparePathStates ir jvs esd newPathState returnVal
          return VContext {
                     vcAssumptions = methodAssumptions de ir ssi
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
  when (v >= 3) $
    putStrLn $ "Running ABC on " ++ methodSpecName ir
  let LV value = deBitBlast de goal
  unless (SV.length value == 1) $
    error "internal: Unexpected number of in verification condition"
  let be = deBitEngine de
  case beCheckSat be of
    Nothing -> error "internal: Bit engine does not support SAT checking."
    Just checkSat -> do
      b <- checkSat (beNeg be (value SV.! 0))
      case b of
        UnSat -> return ()
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
      let goal de vc check = deApplyBinary de bImpliesOp (vcAssumptions vc) (checkGoal de check)
      -- Run verification
      case methodSpecVerificationTactics ir of
        [AST.Rewrite] -> liftIO $ do
          rew <- mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            forM_ (vcChecks vc) $ \check -> do
              when (v >= 2) $
                putStrLn $ "Verify " ++ checkName check
              newGoal <- reduce rew (goal de vc check)
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
          liftIO $ forM_ vcs $ \vc ->
            testRandom de v ir n lim vc
        [AST.ABC] -> liftIO $ do
          forM_ vcs $ \vc -> do
            forM_ (vcChecks vc) $ \check -> do
              when (v >= 2) $
                liftIO $ putStrLn $ "Verify " ++ checkName check
              runABC de v ir (vcInputs vc) check (goal de vc check)
        [AST.Rewrite, AST.ABC] -> liftIO $ do
          rew <- mkRewriter pgm (deTermSemantics de)
          forM_ vcs $ \vc -> do
            forM_ (vcChecks vc) $ \check -> do
              when (v >= 2) $
                liftIO $ putStrLn $ "Verify " ++ checkName check
              newGoal <- reduce rew (goal de vc check)
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
    vs   <- mapM (QuickCheck.pickRandom . termType . viNode) (vcInputs vc)
    eval <- deConcreteEval (V.fromList vs)
    if not (toBool $ eval $ vcAssumptions vc)
      then return passed
      else do forM_ (vcChecks vc) $ \goal ->
                do let goal_ok = toBool (eval (checkGoal de goal))
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
     [ text "Method:" <+> text (methodSpecName ir)
     , case g of
         EqualityCheck n x y ->
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
      []  -> text "No arguments."
      tsUnsorted ->
        let tsSorted = sortBy cmp tsUnsorted
            t0       = last tsSorted
            ts       = init tsSorted

            cmp (Arg a _) (Arg b _) = compare a b
            cmp _ _                 = EQ

        in vcat [ text (show t) <+> text "=" <+> text (show t0) | t <- ts ]
           $$ text (show t0) <+> text "=" <+> ppCValueD Mixfix value


  toBool (CBool b) = b
  toBool value = error $ unlines [ "Internal error in 'testRandom':"
                                 , "  Expected: boolean value"
                                 , "  Result:   " ++ ppCValue Mixfix value ""
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





