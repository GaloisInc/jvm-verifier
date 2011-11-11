-- | Provides 
{- |
Module           : $Header$
Description      : Provides typechecked representation for method specifications and function for creating it from AST representation.
Stability        : provisional
Point-of-contact : jhendrix, atomb
-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module SAWScript.MethodSpecIR 
  ( Postcondition(..)
  , LocalSpecs(..)
  , emptyLocalSpec
  , JavaExprEquivClass
  , ppJavaExprEquivClass
  , MethodSpecIR(..)
  , methodSpecName
  , resolveMethodSpecIR
  ) where

-- Imports {{{1

import Control.Applicative
import Control.Monad
import Data.List (intercalate, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.PrettyPrint.HughesPJ

import Verinf.Symbolic
import Verinf.Utils.IOStateT

import qualified Execution.Codebase as JSS
import qualified JavaParser as JSS
import qualified SAWScript.MethodAST as AST
import qualified SAWScript.TypeChecker as TC
import SAWScript.Utils
import Utils.Common (slashesToDots)


--}}}1

-- Utility definitions {{{1

int8DagType :: DagType
int8DagType = SymInt (constantWidth 8)


int16DagType :: DagType
int16DagType = SymInt (constantWidth 16)

int32DagType :: DagType
int32DagType = SymInt (constantWidth 32)

int64DagType :: DagType
int64DagType = SymInt (constantWidth 64)


-- MethodSpec translation common defines {{{1

data Postcondition
  = PostUnchanged
  | PostArbitrary !DagType
  | PostResult TC.MixedExpr
  deriving (Show)

type JavaExprEquivClass = [TC.JavaExpr]

ppJavaExprEquivClass :: JavaExprEquivClass -> String
ppJavaExprEquivClass [] = error "internal: ppJavaExprEquivClass"
ppJavaExprEquivClass [expr] = show expr
ppJavaExprEquivClass cl = "{ " ++ intercalate ", " (map show (sort cl)) ++ " }"

-- LocalSpecs {{{1

data LocalSpecs = LocalSpecs {
    localAssumptions :: [TC.LogicExpr]
  , localImpAssumptions :: [(TC.JavaExpr, TC.MixedExpr)]
  , localPostconditions :: [(Int, Postcondition)]
  , instanceFieldPostconditions :: [(TC.JavaExpr, JSS.FieldId, Postcondition)]
  , arrayPostconditions :: Map TC.JavaExpr Postcondition
  , localChoiceBlocks :: [(LocalSpecs, LocalSpecs)]
  -- TODO: postconditions on static fields.
  } deriving (Show)

emptyLocalSpec :: LocalSpecs
emptyLocalSpec = 
  LocalSpecs { localAssumptions = []
             , localImpAssumptions = []
             , localPostconditions = []
             , instanceFieldPostconditions = []
             , arrayPostconditions = Map.empty
             , localChoiceBlocks = []
             }

-- MethodSpecTranslation immediate state {{{1

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
       , refTypeMap :: Map TC.JavaExpr TC.JavaRefActualType
       -- | Maps Java ref expression to associated equivalence class.
       , mayAliasRefs :: Map TC.JavaExpr JavaExprEquivClass
       -- | List of mayAlias classes in reverse order that they were created.
       , revAliasSets :: [(JavaExprEquivClass, TC.JavaRefActualType)]
       -- | Maps let bindings already seen to position they were defined.
       , definedLetBindingMap :: Map String Pos
       -- | List of let bindings encountered in reverse order.
       , reversedLetBindings :: [(String, TC.MixedExpr)]
       -- | List of assumptions parsed so far in reverse order.
       , currentAssumptions :: [TC.LogicExpr]
       -- | List of imperative assumptions parsed so far in reverse order.
       , currentImpAssumptions :: [(TC.JavaExpr, TC.MixedExpr)]
         -- | Maps Java expressions to associated constant expression.
       , constExprMap :: Map TC.JavaExpr (CValue, DagType)
       -- | Map Java expressions to typed expression in ensures clause.
       -- or nothing if an arbitrary expression for term has been given.
       , mstsValuePostconditions :: Map TC.JavaExpr (Pos, Postcondition)
       -- | Map from Java expressions to typed expression in ensures clause.
       -- or nothing if an arbitrary expression for term has been given.
       , mstsArrayPostconditions :: Map TC.JavaExpr (Pos, Postcondition)
       -- | List of local specs parsed so far in reverse order.
       , currentLocalSpecs :: Map JSS.PC LocalSpecs
       -- | List of choice blocks parsed so far, in reverse order.
       , currentChoiceBlocks :: [(LocalSpecs, LocalSpecs)]
       }

type MethodSpecTranslator = StateT MethodSpecTranslatorState IO

-- Typechecking utilities {{{1

-- | Throws exception warning floating point types are not supported.
throwFloatUnsupported :: MonadIO m => Pos -> m ()
throwFloatUnsupported p =
  let msg = "SAWScript does not support floating point types."
   in throwIOExecException p (ftext msg) ""

typecheckerConfig :: MethodSpecTranslatorState -> TC.TCConfig
typecheckerConfig msts =
  TC.TCC { TC.opCache = mstsOpCache msts
         , TC.globalBindings = mstsGlobalBindings msts
         , TC.methodInfo = Just (mstsMethod msts, specClass msts)
         , TC.localBindings = Map.fromList (reversedLetBindings msts)
         , TC.toJavaExprType = Just (flip Map.lookup (refTypeMap msts))
         }

type JavaExprTypeChecker = TC.TCConfig -> AST.Expr -> IO TC.JavaExpr

-- | Typecheck a Java expression and verify that is has not been seen.
typecheckNewJavaExpr :: JavaExprTypeChecker
                     -> AST.Expr -> MethodSpecTranslator TC.JavaExpr
typecheckNewJavaExpr typeChecker astExpr = do
  msts <- get
  expr <- lift $ typeChecker (typecheckerConfig msts) astExpr
  let pos = AST.exprPos astExpr
  -- Check expression has not been recorded.
  when (Set.member expr (seenJavaExprs msts)) $
    let msg = "The type of the Java expresssion \'" ++ show expr
              ++ "\' has been already defined."
     in throwIOExecException pos (ftext msg) ""
  -- Check type is supported by SAWScript.
  case TC.jssTypeOfJavaExpr expr of
    JSS.DoubleType -> throwFloatUnsupported pos
    JSS.FloatType  -> throwFloatUnsupported pos
    JSS.ArrayType eltType -> do
      when (JSS.isFloatType eltType) $ do
        throwFloatUnsupported pos
      when (JSS.isRefType eltType) $ do
        let msg = "SAWScript does not support arrays of references."
         in throwIOExecException pos (ftext msg) ""
    _ -> return ()
  -- Add expression to list of seen expressions.
  put msts { seenJavaExprs = Set.insert expr (seenJavaExprs msts) }
  return expr

-- | Check that a type declaration has been provided for this expression.
typecheckRecordedJavaExpr :: JavaExprTypeChecker
                          -> AST.Expr -> MethodSpecTranslator TC.JavaExpr
typecheckRecordedJavaExpr typeChecker astExpr = do
  let pos = AST.exprPos astExpr
  msts <- get
  expr <- lift $ typeChecker (typecheckerConfig msts) astExpr
  unless (Set.member expr (seenJavaExprs msts)) $
    let msg = "The type of " ++ show expr ++ " has not been defined."
        res = "Please add a declaration to indicate the concrete type of this Java expression."
     in throwIOExecException pos (ftext msg) res
  return expr

typecheckGlobalLogicExpr :: AST.Expr -> MethodSpecTranslator TC.LogicExpr 
typecheckGlobalLogicExpr astExpr = do
  oc <- gets mstsOpCache
  bindings <- gets mstsGlobalBindings
  let cfg = TC.mkGlobalTCConfig oc bindings Map.empty
  lift $ TC.tcLogicExpr cfg astExpr

-- Actual type utilities {{{1

-- | Returns actual type of Java expression (which it checks is in fact a reference).
getRefActualType :: Pos -> TC.JavaExpr -> MethodSpecTranslator TC.JavaRefActualType
getRefActualType pos expr = do
  unless (JSS.isRefType (TC.jssTypeOfJavaExpr expr)) $ do
    let msg = "Found primitive value " ++ show expr ++ " where reference is expected."
    throwIOExecException pos (ftext msg) ""
  rtm <- gets refTypeMap
  case Map.lookup expr rtm of
    Just tp -> return tp
    Nothing -> 
      let msg = "The type of " ++ show expr ++ " has not been defined."
       in throwExecException pos (ftext msg) ""

-- | Updates actual type of Java expression.
setRefActualType :: TC.JavaExpr -> TC.JavaRefActualType -> MethodSpecTranslator ()
setRefActualType expr tp =
  modify $ \s -> s { refTypeMap = Map.insert expr tp (refTypeMap s) }


-- Aliasing utilities {{{1

-- | Returns equivalence class of Java expression in translator step.
-- N.B. If this expression denotes a primitive type or does not belong to an
-- equivalence class, this returns a singleton list containing only the
-- expression.
getJavaEquivClass :: TC.JavaExpr -> MethodSpecTranslator JavaExprEquivClass
getJavaEquivClass expr = Map.findWithDefault [expr] expr <$> gets mayAliasRefs

checkRefEquivClassIsUndefined :: Pos -> TC.JavaExpr -> MethodSpecTranslator ()
checkRefEquivClassIsUndefined pos expr = do
  s <- gets mayAliasRefs
  when (Map.member expr s) $ do
    let msg = "\'" ++ show expr ++ "\' is mentioned in multiple mayAlias declarations."
    let res = "MayAlias declarations should be merged so that each reference is mentioned at most once."
    throwIOExecException pos (ftext msg) res

-- Let utilities {{{1

-- | Throw IO exception indicating name was previously defined.
throwVarIsPreviouslyDefined :: MonadIO m => Pos -> Pos -> String -> m ()
throwVarIsPreviouslyDefined pos absPrevPos name = do
  relPos <- liftIO $ posRelativeToCurrentDirectory absPrevPos
  throwIOExecException pos
                       (ftext "The variable " <+> quotes (text name)
                          <+> ftext "is already bound at "
                          <+> text (show relPos) <> char '.')
                       ("Please ensure all names are distinct.")

-- Check var is not already bound within method.
checkVarIsUndefined :: Pos -> String -> MethodSpecTranslator ()
checkVarIsUndefined pos var = do
  locals <- gets definedLetBindingMap
  case Map.lookup var locals of
    Just prevPos -> throwVarIsPreviouslyDefined pos prevPos var
    Nothing -> return ()

-- Assignment utilities {{{1

throwInvalidAssignment :: MonadIO m => Pos -> String -> String -> m ()
throwInvalidAssignment pos lhs tp =
  let msg = lhs ++ " cannot be assigned a value with type " ++ tp ++ "."
   in throwIOExecException pos (ftext msg) ""

coercePrimitiveExpr :: Pos -> TC.LogicExpr -> JSS.Type
                -> MethodSpecTranslator TC.LogicExpr
coercePrimitiveExpr pos expr javaType = do
  oc <- gets mstsOpCache
  let valueType = TC.typeOfLogicExpr expr
  case javaType of
    JSS.BooleanType 
      | valueType == SymBool ->
         let op = iteOp int32DagType
             t = TC.Cns (mkCInt 32 1) int32DagType
             f = TC.Cns (mkCInt 32 0) int32DagType
          in return (TC.Apply op [expr, t, f])
      | valueType == int32DagType -> return expr
    JSS.ByteType 
      | valueType == int8DagType -> 
         return $ TC.Apply (signedExtOp oc (constantWidth 8) 32) [expr]
      | valueType == int32DagType -> return expr
    JSS.CharType 
      | valueType == int16DagType ->
         return $ TC.Apply (unsignedExtOp oc (constantWidth 16) 32) [expr]
      | valueType == int32DagType -> return expr
    JSS.IntType
      | valueType == int32DagType -> return expr
    JSS.LongType
      | valueType == int64DagType -> return expr
    JSS.ShortType 
      | valueType == int16DagType ->
         return $ TC.Apply (signedExtOp oc (constantWidth 16) 32) [expr]
      | valueType == int32DagType -> return expr
    _ -> 
      let msg = ftext $ "A variable with type " ++ show javaType
                          ++ " cannot be assigned a value with type "
                          ++ ppType valueType ++ "."
       in throwIOExecException pos msg ""

-- Postcondition utilities {{{1

throwMultiplePostconditions :: Pos -> TC.JavaExpr -> MethodSpecTranslator ()
throwMultiplePostconditions pos expr =
  let msg = "Multiple postconditions defined for " ++ show expr ++ "."
   in throwIOExecException pos (ftext msg) ""

setConstExpr :: TC.JavaExpr -> CValue -> DagType -> MethodSpecTranslator ()
setConstExpr lhs cns tp = do
  modify $ \s -> s { constExprMap = Map.insert lhs (cns,tp) (constExprMap s) }

-- | Check that no value postcondition has been defined for expression.
checkValuePostconditionIsUndefined :: Pos -> TC.JavaExpr -> MethodSpecTranslator ()
checkValuePostconditionIsUndefined pos expr = do
  equivClass <- getJavaEquivClass expr
  cem <- gets constExprMap
  vpm <- gets mstsValuePostconditions
  let postIsDefined e =  Map.member e cem || Map.member e vpm
  --TODO: Fix this to properly handle expressions correctly.
  when (any postIsDefined equivClass) $
    throwMultiplePostconditions pos expr

getValuePostcondition :: TC.JavaExpr -> MethodSpecTranslatorState -> Postcondition
getValuePostcondition expr msts =
  case Map.lookup expr (mstsValuePostconditions msts) of
    Nothing -> PostUnchanged
    Just (_,cond) -> cond

-- | Set array postcondition and verify that it has not yet been assigned.
setValuePostcondition :: Pos -> TC.JavaExpr -> Postcondition
                      -> MethodSpecTranslator ()
setValuePostcondition pos expr post = do
  checkValuePostconditionIsUndefined pos expr
  modify $ \s ->
    s { mstsValuePostconditions =
          Map.insert expr (pos, post) (mstsValuePostconditions s) }
         
-- | Check that array post condition is undefined.
checkArrayPostconditionIsUndefined :: Pos -> TC.JavaExpr -> MethodSpecTranslator ()
checkArrayPostconditionIsUndefined pos expr = do
  equivClass <- getJavaEquivClass expr
  cem <- gets constExprMap
  apm <- gets mstsArrayPostconditions
  let postIsDefined e = Map.member e cem || Map.member e apm
  when (any postIsDefined equivClass) $
    throwMultiplePostconditions pos expr

-- | Set array postcondition and verify that it has not yet been assigned.
setArrayPostcondition :: Pos -> TC.JavaExpr -> Postcondition
                      -> MethodSpecTranslator ()
setArrayPostcondition pos expr post = do
  checkArrayPostconditionIsUndefined pos expr
  modify $ \msts -> 
    msts { mstsArrayPostconditions 
             = Map.insert expr (pos, post) (mstsArrayPostconditions msts) }

-- | Set imperative assumption.
setImpAssumption :: TC.JavaExpr -> TC.MixedExpr
                 -> MethodSpecTranslator ()
setImpAssumption lhs rhs =
  modify $ \msts ->
    msts { currentImpAssumptions = (lhs, rhs) : (currentImpAssumptions msts) }

-- | Parses arguments from Array expression and returns JavaExpression.
-- throws exception if expression cannot be parsed.
typecheckArrayPostconditionExpr :: AST.Expr
                                -> MethodSpecTranslator (TC.JavaExpr,Int,JSS.Type)
typecheckArrayPostconditionExpr astExpr = do
  let pos = AST.exprPos astExpr
  expr <- typecheckRecordedJavaExpr TC.tcValueOfExpr astExpr
  -- Check array type has been assigned expression.
  rct <- getRefActualType pos expr
  case rct of
    TC.ArrayInstance l tp _ -> return (expr,l,tp)
    _ -> do
      let msg = ftext $ "Type of " ++ show expr ++ " is not an array."
       in throwIOExecException pos msg ""

-- resolveType {{{1

-- | Resolve method spec type declaration.
resolveType :: AST.Expr -> AST.JavaType -> MethodSpecTranslator ()
resolveType astExpr astTp = do
  cb <- gets (TC.codeBase . mstsGlobalBindings)
  -- Typecheck Java expression.
  javaExpr <- typecheckNewJavaExpr TC.tcJavaExpr astExpr
  -- Check that type of astExpr and the type of tp are compatible.
  do let javaExprType = TC.jssTypeOfJavaExpr javaExpr
         tgtType = TC.jssTypeOfASTJavaType astTp
     b <- liftIO $ JSS.isSubtype cb tgtType javaExprType
     unless b $ do
      let msg = ftext ("The expression " ++ show javaExpr ++ " is incompatible with")
                 <+> TC.ppASTJavaType astTp <> char '.'
      throwIOExecException (AST.exprPos astExpr) msg ""
  -- Update actual type if this is a reference.
  mat <- liftIO $ TC.tcActualType cb astTp
  case mat of
    Just at -> setRefActualType javaExpr at
    Nothing -> return ()

-- resolveMayAlias {{{1
resolveMayAlias :: [AST.Expr] -> MethodSpecTranslator ()
resolveMayAlias [] = error "internal: mayAlias set is empty"
resolveMayAlias astRefs = do
  -- Typecheck all expressions.
  ((firstPos,firstRef):restRefs) <- forM astRefs $ \astExpr -> do
    -- Typecheck expression.
    let astPos = AST.exprPos astExpr
    expr <- typecheckRecordedJavaExpr TC.tcJavaExpr astExpr
    -- Check expression is a reference.
    unless (JSS.isRefType (TC.jssTypeOfJavaExpr expr)) $ do
      let msg = "\'mayAlias\' provided a non-reference value."
       in throwIOExecException astPos (ftext msg) ""
    -- Check expression has not already appeared in a mayAlias set.
    checkRefEquivClassIsUndefined astPos expr
    -- Return typechecked expression.
    return (astPos,expr)
  -- Check types of references are the same.
  firstType <- getRefActualType firstPos firstRef
  forM_ restRefs $ \(pos,r) -> do
    rct <- getRefActualType pos r
    when (rct /= firstType) $ do
      let msg = "Different types are assigned to " ++ show firstRef ++ " and " ++ show r ++ "."
          res = "All references that may alias must be assigned the same type."
      throwIOExecException pos (ftext msg) res
  -- Add mayAlias to state.
  let refs = firstRef : map snd restRefs
  modify $ \s ->
     s { mayAliasRefs = mapInsertKeys refs refs (mayAliasRefs s)
       , revAliasSets = (refs,firstType) : revAliasSets s }

-- resolveConst {{{1

resolveConst :: AST.Expr -> AST.Expr -> MethodSpecTranslator ()
resolveConst astJavaExpr@(AST.ApplyExpr _ "valueOf" _) astValueExpr = do
  let pos = AST.exprPos astJavaExpr
  -- Typecheck and validate javaExpr.
  javaExpr <- typecheckNewJavaExpr TC.tcValueOfExpr astJavaExpr
  -- Parse expression (must be global since this is a constant.)
  valueExpr <- typecheckGlobalLogicExpr astValueExpr
  -- Check javaExpr and valueExpr have compatible types.
  let formattedLhs = "\'" ++ show javaExpr ++ "\'"
  let valueType = TC.typeOfLogicExpr valueExpr
  case valueType of
    SymArray (widthConstant -> Just (Wx l)) tp -> do
      case TC.jssTypeOfJavaExpr javaExpr of
        JSS.ArrayType eltType | jssArrayEltType eltType == tp -> do
          let val = TC.globalEval valueExpr
          setRefActualType javaExpr (TC.ArrayInstance l eltType (Just val))
          setConstExpr javaExpr val valueType
        --TODO: Figure out way to support const arrays pointing to class objecs.
        _ -> throwInvalidAssignment pos formattedLhs (ppType valueType)
       -- Evaluate value expression.
    _ -> throwInvalidAssignment pos formattedLhs (ppType valueType)
  -- Update state.
resolveConst astLhsExpr astValueExpr = do
  -- Typecheck and validate javaExpr.
  let pos = AST.exprPos astLhsExpr
  lhsExpr <- typecheckNewJavaExpr TC.tcJavaExpr astLhsExpr
  let javaType = TC.jssTypeOfJavaExpr lhsExpr
  -- Check type is not a reference.
  when (JSS.isRefType javaType) $ do
    let msg = "References may not be assigned constant values."
        res = "To assign a constant value to an array, pass the variable to 'valueOf'."
     in throwIOExecException pos (ftext msg) res
  -- Coerce valueExpr to compatible type.
  valueExpr <- typecheckGlobalLogicExpr astValueExpr
  -- Evaluate expression.
  val <- TC.globalEval <$>
           coercePrimitiveExpr (AST.exprPos astValueExpr) valueExpr javaType
  -- Update state.
  setConstExpr lhsExpr val (jssStackDagType javaType)

-- resolveDecl {{{1

-- | Code for parsing a method spec declaration.
resolveDecl :: AST.MethodSpecDecl -> MethodSpecTranslator ()
resolveDecl (AST.Type _ _ _) = return ()
resolveDecl (AST.MayAlias _ _) = return ()
resolveDecl (AST.Const _ _ _) = return ()
resolveDecl (AST.MethodLet pos name astExpr) = do
  -- Check var is not already bound within method.
  checkVarIsUndefined pos name
  -- Typecheck rhs.
  cfg <- gets typecheckerConfig
  expr <- lift $ TC.tcMixedExpr cfg astExpr
  -- Add binding to let bindings
  modify $ \s ->
    s { definedLetBindingMap = Map.insert name pos (definedLetBindingMap s)
      , reversedLetBindings = (name,expr) : reversedLetBindings s }
resolveDecl (AST.Assume _ astExpr) = do
  -- Typecheck expression.
  cfg <- gets typecheckerConfig
  expr <- lift $ TC.tcLogicExpr cfg astExpr
  -- Check astExpr is a Boolean
  case TC.typeOfLogicExpr expr of
    SymBool -> return ()
    _ -> let msg = "Expression does not denote a predicate."
          in throwIOExecException (AST.exprPos astExpr) (ftext msg) ""
  -- Update current expressions
  modify $ \s -> s { currentAssumptions = expr : currentAssumptions s }
resolveDecl (AST.AssumeImp pos astLhsExpr astRhsExpr) = do
  -- TODO: reduce redundancy with type checking of Ensures
  -- Typecheck lhs expression.
  let lhsPos = AST.exprPos astLhsExpr
  lhsExpr <- typecheckRecordedJavaExpr TC.tcJavaExpr astLhsExpr
  -- TODO: Check that we don't have another assumption for this lhs?
  -- Get rhs position.
  let rhsPos = AST.exprPos astRhsExpr
  -- Add ensures depending on whether we are assigning to reference.
  let lhsType = TC.jssTypeOfJavaExpr lhsExpr
  res <-
    if JSS.isRefType lhsType then do
      -- Typecheck rhs expression.
      rhsExpr <- typecheckRecordedJavaExpr TC.tcJavaExpr astRhsExpr
      -- Check lhs can be assigned value on rhs.
      -- TODO: Consider if this is too strong.
      cb <- gets (TC.codeBase . mstsGlobalBindings)
      lhsActualType <- getRefActualType lhsPos lhsExpr
      rhsActualType <- getRefActualType rhsPos rhsExpr
      typeOk <- liftIO $ TC.isActualSubtype cb rhsActualType lhsActualType
      unless (typeOk) $ do
        let formattedLhs = "\'" ++ show lhsExpr ++ "\'"
        throwInvalidAssignment lhsPos formattedLhs (TC.ppActualType rhsActualType)
      return (TC.JE rhsExpr)
    else do
      -- Typecheck rhs expression.
      cfg <- gets typecheckerConfig
      rhsExpr <- lift $ TC.tcLogicExpr cfg astRhsExpr
      -- Check lhs can be assigned value in rhs (coercing if necessary.
      TC.LE <$> coercePrimitiveExpr rhsPos rhsExpr lhsType
  setImpAssumption lhsExpr res
resolveDecl (AST.Ensures _ astJavaExpr@(AST.ApplyExpr _ "valueOf" _) astValueExpr) = do
  let pos = AST.exprPos astJavaExpr
  -- Typecheck lhs
  (javaExpr, javaLen, javaEltType) <- typecheckArrayPostconditionExpr astJavaExpr
  -- Parse expression to assign to array.
  cfg <- gets typecheckerConfig
  valueExpr <- lift $ TC.tcLogicExpr cfg astValueExpr
  -- Check type compatibility of assignment.
  let javaValueType = jssArrayDagType javaLen javaEltType
  let valueType = TC.typeOfLogicExpr valueExpr
  when (javaValueType /= valueType) $ do
    let formattedExpr = "\'valueOf(" ++ show javaExpr ++ ")\'"
    throwInvalidAssignment pos formattedExpr (ppType valueType)
  -- Update final type.
  setArrayPostcondition pos javaExpr (PostResult (TC.LE valueExpr))

resolveDecl (AST.Ensures pos astLhsExpr astRhsExpr) = do
  -- Typecheck lhs expression.
  let lhsPos = AST.exprPos astLhsExpr
  lhsExpr <- typecheckRecordedJavaExpr TC.tcJavaExpr astLhsExpr
  -- Check lhs postcondition is undefined.
  checkValuePostconditionIsUndefined pos lhsExpr
  -- Get rhs position.
  let rhsPos = AST.exprPos astRhsExpr
  -- Add ensures depending on whether we are assigning to reference.
  let lhsType = TC.jssTypeOfJavaExpr lhsExpr
  res <- 
    if JSS.isRefType lhsType then do
      -- Typecheck rhs expression.
      rhsExpr <- typecheckRecordedJavaExpr TC.tcJavaExpr astRhsExpr
      -- Check lhs can be assigned value on rhs.
      -- TODO: Consider if this is too strong.
      cb <- gets (TC.codeBase . mstsGlobalBindings)
      lhsActualType <- getRefActualType lhsPos lhsExpr
      rhsActualType <- getRefActualType rhsPos rhsExpr
      typeOk <- liftIO $ TC.isActualSubtype cb rhsActualType lhsActualType
      unless (typeOk) $ do
        let formattedLhs = "\'" ++ show lhsExpr ++ "\'"
        throwInvalidAssignment lhsPos formattedLhs (TC.ppActualType rhsActualType)
      return (TC.JE rhsExpr)
    else do
      -- Typecheck rhs expression.
      cfg <- gets typecheckerConfig
      rhsExpr <- lift $ TC.tcLogicExpr cfg astRhsExpr
      -- Check lhs can be assigned value in rhs (coercing if necessary.
      TC.LE <$> coercePrimitiveExpr rhsPos rhsExpr lhsType
  -- Add postcondition
  setValuePostcondition pos lhsExpr (PostResult res)

resolveDecl (AST.Modifies _ astExprs) = mapM_ resolveExpr astExprs
  where resolveExpr astExpr@(AST.ApplyExpr _ "valueOf" _) = do
          (javaExpr, l, eltType) <- typecheckArrayPostconditionExpr astExpr
          let post = PostArbitrary (jssArrayDagType l eltType)
          setArrayPostcondition (AST.exprPos astExpr) javaExpr post
        resolveExpr astExpr = do
          let pos = AST.exprPos astExpr
          javaExpr <- typecheckRecordedJavaExpr TC.tcJavaExpr astExpr
          let javaExprType = TC.jssTypeOfJavaExpr javaExpr
          when (JSS.isRefType javaExprType)  $ do
            let msg = "Modifies postconditions may not be applied to reference types."
            throwIOExecException pos (ftext msg) ""
          let resultType = jssStackDagType javaExprType
          setValuePostcondition pos javaExpr (PostArbitrary resultType)
resolveDecl (AST.LocalSpec _ pc specs) = do
  s <- get
  let initState = s { currentAssumptions = []
                    , mstsValuePostconditions = Map.empty
                    , mstsArrayPostconditions = Map.empty
                    }
  locState <- lift $ execStateT (mapM_ resolveDecl specs) initState
  let locSpecs = mkLocalSpecs locState
  put $ s { currentLocalSpecs =
              Map.insert (fromIntegral pc) locSpecs (currentLocalSpecs s) }
resolveDecl (AST.Choice pos specs specs') = do
  s <- get
  let initState = s { currentAssumptions = []
                    , mstsValuePostconditions = Map.empty
                    , mstsArrayPostconditions = Map.empty
                    }
  locState <- lift $ execStateT (mapM_ resolveDecl specs) initState
  locState' <- lift $ execStateT (mapM_ resolveDecl specs') initState
  let locSpecs = mkLocalSpecs locState
      locSpecs' = mkLocalSpecs locState'
  put $ s { currentChoiceBlocks =
              (locSpecs, locSpecs') : currentChoiceBlocks s }
resolveDecl (AST.Returns _ _) = return ()
resolveDecl (AST.VerifyUsing _ _) = return ()

-- resolveReturns {{{1
resolveReturns :: Pos
               -> JSS.Method
               -> [AST.MethodSpecDecl]
               -> MethodSpecTranslator (Maybe TC.MixedExpr)
resolveReturns mpos method cmds
  | Just returnType <- JSS.methodReturnType method = do
     case returns of
       [] -> do
         let msg = "The Java method \'" ++ JSS.methodName method
                     ++ "\' has a return value, but the spec does not define it."
         throwIOExecException mpos (ftext msg) ""
       (prevExpr:expr:_) -> do
         relPos <- liftIO $ posRelativeToCurrentDirectory (AST.exprPos prevExpr)
         let msg = "Multiple return values specified in a single method spec.  "
                       ++ "The previous return value was given at "
                       ++ show relPos ++ "."
         throwIOExecException (AST.exprPos expr) (ftext msg) ""
       [astExpr] 
         | JSS.isRefType returnType -> do
            -- Typecheck return expression.
            expr <- typecheckRecordedJavaExpr TC.tcJavaExpr astExpr
            let exprType = TC.jssTypeOfJavaExpr expr
            cb <- gets (TC.codeBase . mstsGlobalBindings)
            typeOk <- liftIO $ JSS.isSubtype cb exprType returnType
            unless (typeOk) $ do
              let formattedLhs = "The return value"
              throwInvalidAssignment (AST.exprPos astExpr) formattedLhs (show exprType)
            -- Return expression
            return $ Just (TC.JE expr)
         | otherwise -> do
            -- Typecheck rhs expression.
            cfg <- gets typecheckerConfig
            expr <- lift $ TC.tcLogicExpr cfg astExpr
            -- Check lhs can be assigned value in rhs (coercing if necessary.
            (Just . TC.LE) <$> coercePrimitiveExpr (AST.exprPos astExpr) expr returnType
  | otherwise = -- No return expected
     case returns of
       [] -> return Nothing
       astExpr:_ ->
         let msg = "Return value specified for \'" ++ JSS.methodName method ++ "\', but method returns \'void\'."
          in throwIOExecException (AST.exprPos astExpr) (ftext msg) ""
 where returns = [ e | AST.Returns _ e <- cmds ]

-- resolveVerifyUsing {{{1

areValidTactics :: [AST.VerificationTactic] -> Bool
areValidTactics tactics =
  case tactics of
   [AST.Skip] -> True
   [AST.Rewrite] -> True
   [AST.QuickCheck _ _] -> True
   [AST.ABC] -> True
   [AST.Rewrite, AST.ABC] -> True
   [AST.SmtLib {}] -> True
   [AST.Rewrite, AST.SmtLib {}] -> True
   [AST.Yices _] -> True
   [AST.Rewrite, AST.Yices _] -> True
   _ -> False

resolveVerifyUsing :: Pos -> JSS.Method -> [AST.MethodSpecDecl] -> IO [AST.VerificationTactic]
resolveVerifyUsing mpos method cmds =
  case [ (pos,tactics) | AST.VerifyUsing pos tactics <- cmds ] of
   [] -> let msg = "The verification method for \'" ++ JSS.methodName method ++ "\' is undefined."
             res = "Please specify a verification method.  Use \'skip\' to skip verification."
          in throwIOExecException mpos (ftext msg) res
   [(pos,tactics)] -> do
     let defList args = nest 2 (vcat (map (\(d,m) -> quotes (text d) <> char '.' <+> text m) args))
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
     unless (areValidTactics tactics) $ throwIOExecException pos msg ""
     return tactics
   _ -> let msg = "The verification tactic is set multiple times in the same method specification."
            res = "Please include at most one verification tactic in a single specification."
         in throwIOExecException mpos (ftext msg) res

-- MethodSpecIR {{{1

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
    -- All expressions in specification.
  , methodSpecJavaExprs :: [TC.JavaExpr]
    -- | References in specification with alias information and reference info.
  , specReferences :: [(JavaExprEquivClass, TC.JavaRefActualType)]
    -- | List of constants expected in input (this is used for executing method overrides).
  , specConstants :: [(TC.JavaExpr,CValue,DagType)]
    -- | Let bindings
  , methodLetBindings :: [(String, TC.MixedExpr)]
  -- | Local specifications for method.
  , localSpecs :: Map JSS.PC LocalSpecs
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
 let clName = JSS.className (methodSpecIRThisClass ir)
     mName = JSS.methodName (methodSpecIRMethod ir)
  in slashesToDots clName ++ ('.' : mName)

mkLocalSpecs :: MethodSpecTranslatorState -> LocalSpecs
mkLocalSpecs msts =
  LocalSpecs {
      localAssumptions = currentAssumptions msts
    , localImpAssumptions = currentImpAssumptions msts
    , localPostconditions =
        [ (undefined, getValuePostcondition expr msts)
          --TODO: Find map from names to integers.
        | expr@(TC.Local _ _)  <- Set.toList (seenJavaExprs msts) ]
    , instanceFieldPostconditions =
        [ (r, f, getValuePostcondition expr msts)
        | expr@(TC.InstanceField r f) <- Set.toList (seenJavaExprs msts) ]
    , arrayPostconditions = Map.map snd (mstsArrayPostconditions msts)
    , localChoiceBlocks = reverse $ currentChoiceBlocks msts
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
  let cb = TC.codeBase gb
  (methodClass,method) <- findMethod cb pos mName thisClass
  -- Get list of initial superclasses.
  superClasses <- JSS.supers cb thisClass
  let st = MSTS { mstsOpCache = oc
                , mstsPos = pos
                , mstsGlobalBindings = gb
                , specClass = thisClass
                , mstsMethod = method
                , seenJavaExprs = Set.empty
                , refTypeMap =
                    if JSS.methodIsStatic method then
                      Map.empty
                    else
                      Map.singleton (TC.This (JSS.className thisClass))
                                    (TC.ClassInstance thisClass)
                , mayAliasRefs = Map.empty
                , revAliasSets = []
                , definedLetBindingMap = Map.empty
                , reversedLetBindings = []
                , currentAssumptions = []
                , currentImpAssumptions = []
                , constExprMap = Map.empty
                , mstsValuePostconditions = Map.empty
                , mstsArrayPostconditions = Map.empty
                , currentLocalSpecs = Map.empty
                , currentChoiceBlocks = []
                }
  flip evalStateT st $ do
    -- Initialize necessary values in translator state.
    -- Perform resolution
    -- 1. Resolve Type expressions.
    sequence_ [ mapM_ (flip resolveType astTp) astExprs
              | AST.Type _ astExprs astTp <- cmds ]
    -- 2. Resolve may alias expressions.
    sequence_ [ resolveMayAlias astRefs | AST.MayAlias _ astRefs <- cmds ]
    -- 3. Resolve constants.
    sequence_ [ resolveConst astJavaExpr astValueExpr
              | AST.Const _ astJavaExpr astValueExpr <- cmds ]
    -- 4. Resolve other declarations tactic.
    mapM_ resolveDecl cmds
    -- Get final state
    st' <- get
    -- Check that each declaration of a field does not have the base
    -- object in the mayAlias class.
    let allRefs = Map.keysSet (refTypeMap st')
    let checkRef (TC.This _) = return ()
        checkRef (TC.Arg _ _) = return ()
        checkRef (TC.Local _ _) = return () -- TODO: is this right?
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
    let specReferences = revAliasSets st' ++ unaliasedRefs
    --TODO: Validate that types or constants for all arguments have been provided.
    -- returnValue
    returnValue <- resolveReturns pos method cmds
    -- Get verification method.
    tactics <- liftIO $ resolveVerifyUsing pos method cmds
    -- Return IR.
    return MSIR { methodSpecPos = pos
                , methodSpecIRThisClass = thisClass
                , methodSpecIRMethodClass = methodClass
                , methodSpecIRMethod = method
                , initializedClasses = map JSS.className superClasses
                , methodSpecJavaExprs = Set.toList (seenJavaExprs st')
                , specReferences
                , specConstants = 
                    [ (e,c,tp) | (e,(c,tp)) <- Map.toList (constExprMap st') ]
                , methodLetBindings = reverse (reversedLetBindings st')
                , returnSpec = mkLocalSpecs st'
                , returnValue
                , localSpecs = currentLocalSpecs st'
                , methodSpecVerificationTactics = tactics
                }
