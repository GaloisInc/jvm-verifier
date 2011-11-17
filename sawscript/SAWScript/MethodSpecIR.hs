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
  ( Statement(..)
  , BehaviorSpec(..)
  , JavaExprEquivClass
  , ppJavaExprEquivClass
  , VerifyCommand(..)
  , ValidationPlan(..)
  , MethodSpecIR(..)
  , methodSpecName
  , resolveMethodSpecIR
  ) where

-- Imports {{{1

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List (intercalate, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.PrettyPrint.HughesPJ

import Verinf.Symbolic

import qualified Execution.Codebase as JSS
import qualified JavaParser as JSS
import qualified SAWScript.MethodAST as AST
import qualified SAWScript.TypeChecker as TC
import SAWScript.Utils
import Utils.Common (slashesToDots)

-- Utility definitions {{{1

int8DagType :: DagType
int8DagType = SymInt (constantWidth 8)

int16DagType :: DagType
int16DagType = SymInt (constantWidth 16)

int32DagType :: DagType
int32DagType = SymInt (constantWidth 32)

int64DagType :: DagType
int64DagType = SymInt (constantWidth 64)

typecheckPC :: MonadIO m => Pos -> Integer -> m JSS.PC
typecheckPC pos pc = do
  -- TODO: Check valid instruction locations to ensure pc is valid.
  when (pc <= 0) $ do
    let msg = ftext $ "Invalid program counter."
     in throwIOExecException pos msg ""
  return (fromInteger pc)

-- ExprActualTypeMap {{{1

-- | Maps Java expressions for references to actual type.
type ExprActualTypeMap = Map TC.JavaExpr TC.JavaActualType

checkJavaExprIsDefined :: MonadIO m => Pos -> TC.JavaExpr -> ExprActualTypeMap -> m ()
checkJavaExprIsDefined pos expr seenExprs =
  when (Map.notMember expr seenExprs) $ do
    let msg = "The type of " ++ show expr ++ " has not been defined."
        res = "Please add a declaration to indicate the concrete type of this Java expression."
     in throwIOExecException pos (ftext msg) res

-- | Returns actual type of Java expression (which it checks is in fact a reference).
getRefActualType :: MonadIO m => ExprActualTypeMap -> Pos -> TC.JavaExpr -> m TC.JavaActualType
getRefActualType refTypeMap pos expr = do
  unless (JSS.isRefType (TC.jssTypeOfJavaExpr expr)) $ do
    let msg = "Found primitive value " ++ show expr ++ " where reference is expected."
    throwIOExecException pos (ftext msg) ""
  case Map.lookup expr refTypeMap of
    Just tp -> return tp
    Nothing -> do
      let msg = "The type of " ++ show expr ++ " has not been defined."
      throwIOExecException pos (ftext msg) ""

-- MethodSpecTypecheckContext {{{1

-- | Global context for method spec typechecker.
data MethodSpecTypecheckContext = MSTC {
         -- | Position of method spec decalaration.
         mstcPos :: Pos
         -- Bindings at global level.
       , mstcGlobalBindings :: TC.GlobalBindings
         -- | Class we are currently parsing.
       , mstcClass :: JSS.Class
         -- | Method that spec is for.
       , mstcMethod :: JSS.Method
         -- | Names of rules (used for resolving verify commands).
       , mstcRuleNames :: Set String
       }

typecheckerConfig :: MethodSpecTypecheckContext -- ^ Context for typechecker
                  -> ExprActualTypeMap -- ^ Maps Java expressions for references to actual type.
                  -> Map String TC.MixedExpr -- ^ Local bindings
                  -> TC.TCConfig
typecheckerConfig mstc actualTypeMap localBindings =
  TC.TCC { TC.globalBindings = mstcGlobalBindings mstc
         , TC.methodInfo = Just (mstcMethod mstc, mstcClass mstc)
         , TC.localBindings = localBindings
         , TC.toJavaExprType = Just (\e -> Map.lookup e actualTypeMap)
         }

-- Alias definitions {{{1

type JavaExprEquivClass = [TC.JavaExpr]

-- | Returns a name for the equivalence class.
ppJavaExprEquivClass :: JavaExprEquivClass -> String
ppJavaExprEquivClass [] = error "internal: ppJavaExprEquivClass"
ppJavaExprEquivClass [expr] = show expr
ppJavaExprEquivClass cl = "{ " ++ intercalate ", " (map show (sort cl)) ++ " }"

type MayAliasMap = Map TC.JavaExpr JavaExprEquivClass

-- MethodSpecTypechecker {{{1

-- Definitions {{{2

data MethodSpecTypecheckState = MSTS {
         -- | Maps Java expressions referenced in type expressions
         -- to their associated type.
         mstsActualTypeMap :: ExprActualTypeMap
         -- | Maps Java ref expression to associated equivalence class.
       , mstsMayAliasMap :: MayAliasMap
         -- | List of mayAlias classes in reverse order that they were created.
       , mstsRevAliasSets :: [(JavaExprEquivClass, TC.JavaActualType)]
       }

type MethodSpecTypechecker = StateT MethodSpecTypecheckState (ReaderT MethodSpecTypecheckContext IO)

runMethodSpecTypechecker :: MethodSpecTypecheckContext -> MethodSpecTypechecker a -> IO a
runMethodSpecTypechecker mstc m = runReaderT (evalStateT m initMSTS) mstc
  where initRefTypeMap
          | JSS.methodIsStatic (mstcMethod mstc) = Map.empty
          | otherwise = 
              Map.singleton (TC.This (JSS.className (mstcClass mstc)))
                            (TC.ClassInstance (mstcClass mstc))
        initMSTS = 
          MSTS { mstsActualTypeMap = initRefTypeMap
               , mstsMayAliasMap = Map.empty
               , mstsRevAliasSets = []
               }

type ExprTypechecker a = TC.TCConfig -> AST.Expr -> IO a
type JavaExprTypechecker = ExprTypechecker TC.JavaExpr

-- Utilities {{{2

-- | Throws exception warning floating point types are not supported.
throwFloatUnsupported :: MonadIO m => Pos -> m ()
throwFloatUnsupported p =
  let msg = "SAWScript does not support floating point types."
   in throwIOExecException p (ftext msg) ""

-- | Verify that type is supported by SAWScript.
checkIsSupportedType :: Pos -> JSS.Type -> IO ()
checkIsSupportedType pos tp =
  case tp of
    JSS.DoubleType -> throwFloatUnsupported pos
    JSS.FloatType  -> throwFloatUnsupported pos
    JSS.ArrayType eltType -> do
      when (JSS.isFloatType eltType) $ do
        throwFloatUnsupported pos
      when (JSS.isRefType eltType) $ do
        let msg = "SAWScript does not support arrays of references."
         in throwIOExecException pos (ftext msg) ""
    _ -> return ()

typecheckJavaExpr :: JavaExprTypechecker -> AST.Expr -> MethodSpecTypechecker TC.JavaExpr
typecheckJavaExpr typeChecker astExpr = do
  mstc <- ask
  msts <- get
  liftIO $ typeChecker (typecheckerConfig mstc (mstsActualTypeMap msts) Map.empty) astExpr

-- resolveVarDecl {{{2

-- | Resolve method spec type declaration.
resolveVarDecl :: AST.Expr -> AST.JavaType -> MethodSpecTypechecker ()
resolveVarDecl exprAst typeAst = do
  -- Typecheck Java expression.
  let pos = AST.exprPos exprAst
  expr <- typecheckJavaExpr TC.tcJavaExpr exprAst
  -- Check type is supported by SAWScript.
  liftIO $ checkIsSupportedType pos (TC.jssTypeOfJavaExpr expr)
  -- Check that type of exprAst and the type of tp are compatible.
  cb <- asks (TC.codeBase . mstcGlobalBindings)
  do let exprType = TC.jssTypeOfJavaExpr expr
         tgtType = TC.jssTypeOfASTJavaType typeAst
     b <- liftIO $ JSS.isSubtype cb tgtType exprType
     unless b $ do
      let msg = ftext ("The expression " ++ show expr ++ " is incompatible with")
                 <+> TC.ppASTJavaType typeAst <> char '.'
      throwIOExecException (AST.exprPos exprAst) msg ""
  -- Get Actual type.
  at <- liftIO $ TC.tcActualType cb typeAst
  -- Check expression has not been recorded and record it.
  msts <- get
  when (Map.member expr (mstsActualTypeMap msts)) $
    let msg = "The Java expresssion \'" ++ show expr ++ "\' has been already defined."
     in throwIOExecException pos (ftext msg) ""
  put msts { mstsActualTypeMap = Map.insert expr at (mstsActualTypeMap msts) }

-- resolveMayAlias {{{2

checkRefEquivClassIsUndefined :: Pos -> TC.JavaExpr -> MethodSpecTypechecker ()
checkRefEquivClassIsUndefined pos expr = do
  s <- gets mstsMayAliasMap
  when (Map.member expr s) $ do
    let msg = "\'" ++ show expr ++ "\' is mentioned in multiple mayAlias declarations."
    let res = "MayAlias declarations should be merged so that each reference is mentioned at most once."
    throwIOExecException pos (ftext msg) res

addMayAliasEquivClass :: JavaExprEquivClass -> TC.JavaActualType -> MethodSpecTypechecker ()
addMayAliasEquivClass refs tp =
  modify $ \s ->
     s { mstsMayAliasMap = mapInsertKeys refs refs (mstsMayAliasMap s)
       , mstsRevAliasSets = (refs,tp) : mstsRevAliasSets s }

resolveMayAlias :: [AST.Expr] -> MethodSpecTypechecker ()
resolveMayAlias [] = error "internal: mayAlias set is empty"
resolveMayAlias astRefs = do
  -- Typecheck all expressions.
  ((firstPos,firstRef):restRefs) <- forM astRefs $ \astExpr -> do
    -- Typecheck expression.
    let astPos = AST.exprPos astExpr
    expr <- typecheckJavaExpr TC.tcJavaExpr astExpr
    -- Check expression is defined.
    seenExprs <- gets mstsActualTypeMap
    checkJavaExprIsDefined astPos expr seenExprs
    -- Check expression is a reference.
    unless (JSS.isRefType (TC.jssTypeOfJavaExpr expr)) $ do
      let msg = "\'mayAlias\' provided a non-reference value."
       in throwIOExecException astPos (ftext msg) ""
    -- Check expression has not already appeared in a mayAlias set.
    checkRefEquivClassIsUndefined astPos expr
    -- Return typechecked expression.
    return (astPos,expr)
  -- Check types of references are the same.
  refTypeMap <- gets mstsActualTypeMap
  firstType <- getRefActualType refTypeMap firstPos firstRef
  forM_ restRefs $ \(pos,r) -> do
    rct <- getRefActualType refTypeMap pos r
    when (rct /= firstType) $ do
      let msg = "Different types are assigned to " ++ show firstRef ++ " and " ++ show r ++ "."
          res = "All references that may alias must be assigned the same type."
      throwIOExecException pos (ftext msg) res
  -- Update equiv class. mayAlias to state.
  addMayAliasEquivClass (firstRef : map snd restRefs) firstType

-- BehaviorTypecheckContext {{{1

-- | Context for typechecking the behavior of the method.
data BehaviorTypecheckContext = BTC {
         btcMSTC :: MethodSpecTypecheckContext
         -- | Java expressions seen at method level.
       , btcActualTypeMap :: ExprActualTypeMap
       , btcMayAliasMap  :: Map TC.JavaExpr JavaExprEquivClass
       , btcRefEquivClasses :: [(JavaExprEquivClass, TC.JavaActualType)]
       }

resolveBTC :: MethodSpecTypecheckContext
           -> [AST.MethodSpecDecl]
           -> IO BehaviorTypecheckContext
resolveBTC mstc cmds =
  runMethodSpecTypechecker mstc $ do
    -- 1. Resolve Type expressions.
    sequence_ [ mapM_ (flip resolveVarDecl astTp) astExprs
              | AST.Behavior (AST.VarDecl _ astExprs astTp) <- cmds ]
    -- 2. Resolve may alias expressions.
    sequence_ [ resolveMayAlias astRefs | AST.MayAlias _ astRefs <- cmds ]
    -- 3. Find unaliased references and add implicit unary mayAlias values.
    rtm <- gets mstsActualTypeMap
    forM_ (Map.toList rtm) $ \(r,tp) -> do
      mam <- gets mstsMayAliasMap
      when (TC.isActualRef tp && Map.notMember r mam) $ do
        addMayAliasEquivClass [r] tp
    -- Get state
    msts <- get
    -- Check that each declaration of a field does not have the base
    -- object in the mayAlias class.
     -- TODO: Improve this code to relax this restriction.
    let fieldPairs = [ (lhs,f) | TC.InstanceField lhs f <- Map.keys (mstsActualTypeMap msts) ]
    forM_ fieldPairs $ \(lhs,f) -> do
      when (Map.member lhs (mstsMayAliasMap msts)) $
        let msg = "This specification contains a mayAlias declaration "
                 ++ "containing \'" ++ show lhs ++ "\' and an additional "
                 ++ "declaration that references its field \'"
                 ++ JSS.fieldIdName f ++ "\'.  The current SAWScript "
                 ++ "implementation does not support this."
            res = "Please remove the mayAlias declaration of \'" ++ show lhs
                 ++ "\' and alter the Java code as needed."
         in throwIOExecException (mstcPos mstc) (ftext msg) res
    -- Return BTC
    return BTC { btcMSTC = mstc
               , btcActualTypeMap = mstsActualTypeMap msts
               , btcMayAliasMap = mstsMayAliasMap msts
               , btcRefEquivClasses = mstsRevAliasSets msts
               }

-- BehaviorTypechecker {{{1

-- Definitions {{{2

-- | Statements used for implementing behavior specification.
data Statement
   = AssumePred TC.LogicExpr
   | AssumeValueEq TC.JavaExpr TC.MixedExpr
   | AssumeArrayEq TC.JavaExpr TC.LogicExpr
--   | EnsurePred TC.LogicExpr
   | EnsureInstanceField TC.JavaExpr JSS.FieldId TC.MixedExpr
   | EnsureArray TC.JavaExpr TC.LogicExpr
   | ModifyInstanceField TC.JavaExpr JSS.FieldId
   | ModifyArray TC.JavaExpr DagType
   | Return TC.MixedExpr
  deriving (Show)

data BehaviorTypecheckPath = BTP {
         -- | Maps types to position they are defined.
         btpNewLocalTypes :: ExprActualTypeMap
         -- | Statements for all blocks in reverse order.
       , btpReversedStatements :: [Statement]
       }

data BehaviorTypecheckState = BTS {
         -- | Statements for all blocks in reverse order.
         btsPaths :: [BehaviorTypecheckPath]
         -- | Maps types at method and local level to actual type (this is just used by parser).
       , btsActualTypeMap :: ExprActualTypeMap
         -- | Maps let bindings already seen to position they were defined.
       , btsLetBindings :: Map String (Pos, TC.MixedExpr)
         -- | Maps Java expression to associated value assumption.
       , btsValueAssumptions :: Set TC.JavaExpr
         -- | Maps Java expression to associated array assumption.
       , btsArrayAssumptions :: Set TC.JavaExpr
         -- | Maps Java expression to associated value postcondition.
       , btsValuePostconditions :: Set TC.JavaExpr
         -- | Maps Java expression to associated array postcondition.
       , btsArrayPostconditions :: Set TC.JavaExpr
       , btsReturnSet :: Bool
       }

type BehaviorTypechecker = StateT BehaviorTypecheckState (ReaderT BehaviorTypecheckContext IO)

-- Utilities {{{2

-- | Get codebase used in behavior typechecker.
getBTCCodebase :: BehaviorTypechecker JSS.Codebase
getBTCCodebase = asks (TC.codeBase . mstcGlobalBindings . btcMSTC)

runExprTypechecker :: ExprTypechecker a -> AST.Expr -> BehaviorTypechecker a
runExprTypechecker typeChecker expr = do
  msts <- asks btcMSTC
  actualTypes <- gets btsActualTypeMap
  letBindings <- Map.map snd <$> gets btsLetBindings
  let cfg = typecheckerConfig msts actualTypes letBindings
  liftIO $ typeChecker cfg expr

-- | Check that a type declaration has been provided for this expression.
typecheckRecordedJavaExpr :: JavaExprTypechecker
                          -> AST.Expr
                          -> BehaviorTypechecker TC.JavaExpr
typecheckRecordedJavaExpr typeChecker astExpr = do
  let pos = AST.exprPos astExpr
  expr <- runExprTypechecker typeChecker astExpr
  seenExprs <- asks btcActualTypeMap
  checkJavaExprIsDefined pos expr seenExprs
  return expr

-- | Returns equivalence class of Java expression in translator step.
-- N.B. If this expression denotes a primitive type or does not belong to an
-- equivalence class, this returns a singleton list containing only the
-- expression.
getJavaEquivClass :: TC.JavaExpr -> BehaviorTypechecker JavaExprEquivClass
getJavaEquivClass expr = Map.findWithDefault [expr] expr <$> asks btcMayAliasMap

-- | Throw IO exception indicating name was previously defined.
throwVarIsPreviouslyDefined :: MonadIO m => Pos -> Pos -> String -> m ()
throwVarIsPreviouslyDefined pos absPrevPos name = do
  relPos <- liftIO $ posRelativeToCurrentDirectory absPrevPos
  throwIOExecException pos
                       (ftext "The variable " <+> quotes (text name)
                          <+> ftext "is already bound at "
                          <+> text (show relPos) <> char '.')
                       ("Please ensure all names are distinct.")

throwInvalidAssignment :: MonadIO m => Pos -> String -> String -> m ()
throwInvalidAssignment pos lhs tp =
  let msg = lhs ++ " cannot be assigned a value with type " ++ tp ++ "."
   in throwIOExecException pos (ftext msg) ""

addStatement :: Statement -> BehaviorTypechecker ()
addStatement stmt = do
  let updatePath p = p { btpReversedStatements = stmt : btpReversedStatements p }
  modify $ \bts -> bts { btsPaths = map updatePath (btsPaths bts) }

-- Assignment utilities {{{2

-- | Typecheckes a 'valueOf' expression and returns Java expression inside
-- of it.  The type must be an array, and if a DagType is provided, the expression
-- must be compatible with the type.
typecheckValueOfLhs :: AST.Expr -> Maybe DagType -> BehaviorTypechecker (TC.JavaExpr, DagType)
typecheckValueOfLhs astExpr maybeExpectedType = do
  let pos = AST.exprPos astExpr
  expr <- typecheckRecordedJavaExpr TC.tcValueOfExpr astExpr
  -- Check expression is a compatible array type.
  rtm <- asks btcActualTypeMap
  rct <- getRefActualType rtm pos expr
  case rct of
    TC.ArrayInstance l tp -> do
      let javaValueType = jssArrayDagType l tp
      case maybeExpectedType of
        Nothing -> return ()
        Just expectedType -> do
          when (javaValueType /= expectedType) $ do
            let formattedExpr = "\'valueOf(" ++ show expr ++ ")\'"
            throwInvalidAssignment pos formattedExpr (ppType expectedType)
      return (expr, javaValueType)
    _ ->
      let msg = ftext $ "Type of " ++ show expr ++ " is not an array."
       in throwIOExecException pos msg ""

coercePrimitiveExpr :: MonadIO m
                    => OpCache
                    -> Pos
                    -> TC.LogicExpr
                    -> JSS.Type -- ^ Expected left-hand side type.
                    -> m TC.LogicExpr
coercePrimitiveExpr oc pos expr javaType = do
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

typecheckMixedExpr :: Pos -> String -> JSS.Type -> AST.Expr -> BehaviorTypechecker TC.MixedExpr
typecheckMixedExpr pos lhsName lhsType rhsAst = do
  if JSS.isRefType lhsType then do
    -- Typecheck rhs expression.
    rhsExpr <- typecheckRecordedJavaExpr TC.tcJavaExpr rhsAst
    let rhsType = TC.jssTypeOfJavaExpr rhsExpr
    -- Check lhs can be assigned value on rhs.
    cb <- getBTCCodebase
    typeOk <- liftIO $ JSS.isSubtype cb rhsType lhsType
    unless (typeOk) $ do
      throwInvalidAssignment pos lhsName (show rhsType)
    -- Return result.
    return (TC.JE rhsExpr)
  else do
    -- Typecheck rhs expression.
    rhsExpr <- runExprTypechecker TC.tcLogicExpr rhsAst
    -- Check lhs can be assigned value in rhs (coercing if necessary.
    oc <- asks (TC.opCache . mstcGlobalBindings . btcMSTC)
    TC.LE <$> coercePrimitiveExpr oc pos rhsExpr lhsType

-- Postcondition utilities {{{2

checkFreshAssignment :: Pos -> String -> TC.JavaExpr -> Set TC.JavaExpr -> BehaviorTypechecker ()
checkFreshAssignment pos assignmentType expr prevSet = do
  equivClass <- getJavaEquivClass expr
  when (any (flip Set.member prevSet) equivClass) $
    let msg = "Multiple " ++ assignmentType ++ " defined for " ++ show expr ++ "."
     in throwIOExecException pos (ftext msg) ""

-- | Set imperative assumption.
recordArrayAssumption :: Pos -> TC.JavaExpr -> BehaviorTypechecker ()
recordArrayAssumption pos expr = do
  bts <- get
  let prevSet = btsArrayAssumptions bts
  checkFreshAssignment pos "assumptions" expr prevSet
  put bts { btsArrayAssumptions = Set.insert expr prevSet }

-- | Set imperative assumption.
recordValueAssumption :: Pos -> TC.JavaExpr -> BehaviorTypechecker ()
recordValueAssumption pos expr = do
  bts <- get
  let prevSet = btsValueAssumptions bts
  checkFreshAssignment pos "assumptions" expr prevSet
  put bts { btsValueAssumptions = Set.insert expr prevSet }

-- | Set array postcondition and verify that it has not yet been assigned.
recordArrayPostcondition :: Pos -> TC.JavaExpr -> BehaviorTypechecker ()
recordArrayPostcondition pos expr = do
  bts <- get
  let prevSet = btsArrayPostconditions bts
  checkFreshAssignment pos "postconditions" expr prevSet
  put bts { btsArrayPostconditions = Set.insert expr prevSet }

-- | Set array postcondition and verify that it has not yet been assigned.
recordValuePostcondition :: Pos -> TC.JavaExpr -> BehaviorTypechecker ()
recordValuePostcondition pos expr = do
  -- Make sure expr is assignable.
  case expr of
    TC.This _ ->
      let msg = "Cannot defined post-conditions on \'this\'."
       in throwIOExecException pos (ftext msg) ""
    TC.Arg _ _ ->
      let msg = "Cannot defined post-conditions on argument values."
       in throwIOExecException pos (ftext msg) ""
    TC.Local _ _ -> 
      let msg = "Cannot defined post-conditions on local values."
       in throwIOExecException pos (ftext msg) ""
    TC.InstanceField{} -> return ()
  -- Check postcondition has not been assigned.
  bts <- get
  let prevSet = btsValuePostconditions bts
  checkFreshAssignment pos "postconditions" expr prevSet
  -- Record that expression has postcondition.
  put bts { btsValuePostconditions = Set.insert expr prevSet }

-- resolveDecl {{{2

checkLogicExprIsPred :: MonadIO m => Pos -> TC.LogicExpr -> m ()
checkLogicExprIsPred pos expr =
  case TC.typeOfLogicExpr expr of
    SymBool -> return ()
    _ -> let msg = "Expression does not denote a predicate."
          in throwIOExecException pos (ftext msg) ""

-- | Code for parsing a method spec declaration.
resolveDecl :: [AST.BehaviorDecl] -> BehaviorTypechecker ()
resolveDecl [] = return ()
resolveDecl (AST.VarDecl _ _ _:r) = do
  -- TODO: Fix this.
  error "Not yet implemented: resolveDecl AST.VarDecl"
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.MethodLet pos lhs rhsAst:r) = do
  -- Typecheck rhs.
  rhsExpr <- runExprTypechecker TC.tcMixedExpr rhsAst
  -- Record variable binding.
  do bts <- get
     let locals = btsLetBindings bts
     case Map.lookup lhs locals of
       Just (prevPos,_) -> throwVarIsPreviouslyDefined pos prevPos lhs
       Nothing -> return ()
     put bts { btsLetBindings = Map.insert lhs (pos,rhsExpr) locals }
  -- Resolve remaining declarations.
  resolveDecl r

resolveDecl (AST.AssumePred _ ast:r) = do
  -- Typecheck expression.
  expr <- runExprTypechecker TC.tcLogicExpr ast
  -- Check expr is a Boolean
  checkLogicExprIsPred (AST.exprPos ast) expr
  -- Add assumption.
  addStatement $ AssumePred expr
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.AssumeImp _ lhsAst@(AST.ApplyExpr _ "valueOf" _) rhsAst:r) = do
  -- Typecheck rhs
  rhsExpr <- runExprTypechecker TC.tcLogicExpr rhsAst
  let rhsType = TC.typeOfLogicExpr rhsExpr
  -- Typecheck lhs
  let lhsPos = AST.exprPos lhsAst
  (lhsExpr, _) <- typecheckValueOfLhs lhsAst (Just rhsType)
  -- Update assumption.
  recordArrayAssumption lhsPos lhsExpr
  addStatement $ AssumeArrayEq lhsExpr rhsExpr
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.AssumeImp _ lhsAst rhsAst:r) = do
  -- Typecheck lhs expression.
  let lhsPos = AST.exprPos lhsAst
  lhsExpr <- typecheckRecordedJavaExpr TC.tcJavaExpr lhsAst
  let lhsName = "\'" ++ show lhsExpr ++ "\'"
  let lhsType = TC.jssTypeOfJavaExpr lhsExpr
  -- Typecheck mixed expression.
  rhsExpr <- typecheckMixedExpr lhsPos lhsName lhsType rhsAst
  -- Add assumption.
  recordValueAssumption lhsPos lhsExpr
  addStatement $ AssumeValueEq lhsExpr rhsExpr
  -- Resolve remaining declarations.
  resolveDecl r
{-
resolveDecl (AST.EnsuresPred _ astExpr:r) = do
  -- Typecheck expression.
  expr <- runExprTypechecker TC.tcLogicExpr astExpr
  -- Check expr is a Boolean
  checkLogicExprIsPred (AST.exprPos astExpr) expr
  -- Update current expressions
  addStatement $ EnsurePred expr
  -- Resolve remaining declarations.
  resolveDecl r
  -}
resolveDecl (AST.EnsuresImp _ lhsAst@(AST.ApplyExpr _ "valueOf" _) rhsAst:r) = do
  -- Typecheck rhs
  rhsExpr <- runExprTypechecker TC.tcLogicExpr rhsAst
  let rhsType = TC.typeOfLogicExpr rhsExpr
  -- Typecheck lhs
  (lhsExpr, _) <- typecheckValueOfLhs lhsAst (Just rhsType)
  -- Update postcondition.
  recordArrayPostcondition (AST.exprPos lhsAst) lhsExpr
  addStatement $ EnsureArray lhsExpr rhsExpr
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.EnsuresImp _ astLhsExpr astRhsExpr:r) = do
  -- Typecheck lhs expression.
  let lhsPos = AST.exprPos astLhsExpr
  lhsExpr <- typecheckRecordedJavaExpr TC.tcJavaExpr astLhsExpr
  let lhsName = "\'" ++ show lhsExpr ++ "\'"
  let lhsType = TC.jssTypeOfJavaExpr lhsExpr
  -- Typecheck right hand side.
  rhsExpr <- typecheckMixedExpr lhsPos lhsName lhsType astRhsExpr
  -- Add postcondition based on left-hand side expression.
  recordValuePostcondition lhsPos lhsExpr
  case lhsExpr of
    TC.InstanceField ref f -> addStatement $ EnsureInstanceField ref f rhsExpr
    _ -> error "internal: Unexpected expression"
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.Modifies _ astExprs:r) = mapM_ resolveExpr astExprs >> resolveDecl r
  where resolveExpr ast@(AST.ApplyExpr _ "valueOf" _) = do
          (expr, tp) <- typecheckValueOfLhs ast Nothing
          -- Update postcondition
          recordArrayPostcondition (AST.exprPos ast) expr
          addStatement $ ModifyArray expr tp
        resolveExpr ast = do
          -- Typecheck ast
          let pos = AST.exprPos ast
          expr <- typecheckRecordedJavaExpr TC.tcJavaExpr ast
          -- Update postcondition.
          recordValuePostcondition pos expr
          -- Check this type is valid for a modifies clause.
          let exprType = TC.jssTypeOfJavaExpr expr
          when (JSS.isRefType exprType)  $ do
            let msg = "Modifies postconditions may not be applied to reference types."
            throwIOExecException pos (ftext msg) ""
          -- Add statement
          case expr of
            TC.InstanceField ref field -> 
              addStatement $ ModifyInstanceField ref field
            _ -> error "internal: Unexpected expression"
resolveDecl (AST.Return pos ast:r) = do
  method <- asks (mstcMethod . btcMSTC)
  case JSS.methodReturnType method of
    Nothing ->
      let msg = "Return value specified for \'" ++ JSS.methodName method 
                   ++ "\', but method returns \'void\'."
       in throwIOExecException pos (ftext msg) ""
    Just returnType -> do
      -- Typecheck expression.
      expr <- typecheckMixedExpr (AST.exprPos ast) "The return value" returnType ast
      -- Record return is set.
      bts <- get
      when (btsReturnSet bts) $ do
        let msg = "Multiple return values specified in a single method spec."
        throwIOExecException pos (ftext msg) ""
      put bts { btsReturnSet = True }
      -- Add return statement.
      addStatement $ Return expr
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.MethodIf p c t:r) =
  resolveDecl (AST.MethodIfElse p c t (AST.Block []):r)
resolveDecl (AST.MethodIfElse pos condAst t f:r) = do
  -- Typecheck condition.
  cond <- runExprTypechecker TC.tcLogicExpr condAst
  checkLogicExprIsPred (AST.exprPos condAst) cond
  -- Branch execution.
  bts <- get
  lift $ do
    tBts <- flip execStateT bts $ do
      addStatement $ AssumePred cond
      resolveDecl [t]
    fBts <- flip execStateT bts $ do
      let negCond = TC.Apply bNotOp [cond]
      addStatement $ AssumePred negCond
      resolveDecl [f]
    when (btsReturnSet tBts /= btsReturnSet fBts) $ do
      let msg = "Return value set in one branch, but not the other."
          res = "Please ensure that both branches set the return value."
       in throwIOExecException pos (ftext msg) res
    let rBts = BTS { btsPaths = btsPaths tBts ++ btsPaths fBts
                     -- Use originl local type map and let bindings.
                   , btsActualTypeMap = btsActualTypeMap bts
                   , btsLetBindings = btsLetBindings bts
                     -- Use other values from tBts
                   , btsValueAssumptions = btsValueAssumptions tBts
                               `Set.union` btsValueAssumptions fBts
                   , btsArrayAssumptions = btsArrayAssumptions tBts
                               `Set.union` btsArrayAssumptions fBts
                   , btsValuePostconditions = btsValuePostconditions tBts
                                  `Set.union` btsValuePostconditions fBts
                   , btsArrayPostconditions = btsArrayPostconditions tBts
                                  `Set.union` btsArrayPostconditions fBts
                   , btsReturnSet = btsReturnSet tBts
                   }
    evalStateT (resolveDecl r) rBts
resolveDecl (AST.Block ast:r) = do
  oldBindings <- gets btsLetBindings
  -- Resolve declarations in block.
  resolveDecl ast
  -- Forget let bindings in block for typechecking purposes.
  modify $ \bts -> bts { btsLetBindings = oldBindings }
  -- Resolve remaining declarations.
  resolveDecl r

-- resolveReturns {{{2
{- TODO: Fix this
resolveReturns :: Pos
               -> JSS.Method
               -> [AST.BehaviorDecl]
               -> BehaviorTypechecker (Maybe TC.MixedExpr)
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
       [astExpr] -> Just <$>
         typecheckMixedExpr (AST.exprPos astExpr) "The return value" returnType astExpr
  | otherwise = -- No return expected
     case returns of
       [] -> return Nothing
       astExpr:_ ->
         let msg = "Return value specified for \'" ++ JSS.methodName method 
                      ++ "\', but method returns \'void\'."
          in throwIOExecException (AST.exprPos astExpr) (ftext msg) ""
 where returns = [ e | AST.Return _ e <- cmds ]
       -}

-- BehaviorSpec {{{1

data BehaviorSpec = BehaviorSpec {
         -- | Program counter for spec.
         bsPC :: JSS.PC
             {-
         -- All expressions in specification, used to validate all expected
         -- expressions are defined in simulator.
         bsJavaExprs :: [TC.JavaExpr]
       -}
         -- | References in specification, grouped by alias equivalence class and
         -- concrete type expected.
       , bsRefEquivClasses :: [(JavaExprEquivClass, TC.JavaActualType)]
         -- | Let bindings (stored in order they should be evaluated).
       , bsStatements :: [Statement]
       } deriving (Show)

resolveBehaviorSpecs :: Pos -- ^ Position of method
                     -> BehaviorTypecheckContext 
                     -> JSS.PC
                     -> AST.BehaviorDecl
                     -> IO (ExprActualTypeMap, [BehaviorSpec])
resolveBehaviorSpecs mpos btc pc block = do
  let initBtp = BTP { btpNewLocalTypes = Map.empty, btpReversedStatements = [] }
      initBts = BTS { btsPaths = [initBtp]
                    , btsActualTypeMap = btcActualTypeMap btc
                    , btsLetBindings = Map.empty
                    , btsValueAssumptions = Set.empty
                    , btsArrayAssumptions = Set.empty
                    , btsValuePostconditions = Set.empty
                    , btsArrayPostconditions = Set.empty
                    , btsReturnSet = False
                    }
  -- Flatten commands so top level type information is available to expression parser.
  let flatDecls = case block of
                    AST.Block cmds -> cmds
                    _ -> [block]
  bts <- flip runReaderT btc $
           flip execStateT initBts $
             resolveDecl flatDecls
  -- Ensure returns is set if method has return value.
  let method = mstcMethod (btcMSTC btc)
  when (isJust (JSS.methodReturnType method) && not (btsReturnSet bts)) $
    let msg = "The Java method \'" ++ JSS.methodName method
                     ++ "\' has a return value, but the spec does not define it."
     in throwIOExecException mpos (ftext msg) ""
  -- Return paths parsed from this spec.
  let mkSpec p = BehaviorSpec { bsPC = pc
                              , bsRefEquivClasses = btcRefEquivClasses btc
                              , bsStatements = reverse (btpReversedStatements p) }
  return ( btsActualTypeMap bts
         , map mkSpec (btsPaths bts))

-- resolveValidationPlan {{{1

-- TODO: Replace this with a more meaningful command.
data VerifyCommand
   = Rewrite
   | ABC
   | SmtLib (Maybe Int) (Maybe String) -- version, file
   | Yices (Maybe Int)
   | Expand Pos TC.LogicExpr
    -- | Enable use of a rule or extern definition.
   | VerifyEnable String
     -- | Disable use of a rule or extern definition.
   | VerifyDisable String
   | VerifyAt JSS.PC [VerifyCommand]
 deriving (Show)

data ValidationPlan
  = Skip
  | QuickCheck Integer (Maybe Integer)
  | Verify [VerifyCommand]
  deriving (Show)

isValidationPlanDecl :: AST.MethodSpecDecl -> Bool
isValidationPlanDecl AST.Verify{} = True
isValidationPlanDecl AST.QuickCheck{} = True
isValidationPlanDecl _ = False

checkRuleIsDefined :: MonadIO m => Pos -> String -> Set String -> m ()
checkRuleIsDefined pos nm ruleNames = do
  when (Set.notMember nm ruleNames) $ do
    let msg = "Unknown rule or definition " ++ show nm ++ "."
    throwIOExecException pos (ftext msg) ""

data VerifyTypecheckerState = VTS {
         vtsMSTC :: MethodSpecTypecheckContext
       , vtsExprTypes :: Map JSS.PC ExprActualTypeMap
         -- | Current PC (if inside at command).
       , vtsPC :: Maybe JSS.PC
       }

type VerifyTypechecker = StateT VerifyTypecheckerState IO

resolveVerifyCommand :: AST.VerifyCommand -> VerifyTypechecker [VerifyCommand]
resolveVerifyCommand cmd =
  case cmd of
    AST.ABC -> return [ABC]
    AST.Rewrite -> return [Rewrite]
    AST.SmtLib v f -> return [SmtLib v f]
    AST.Yices v -> return [Yices v]
    AST.Expand ast -> do
      expr <- undefined -- TODO: Fix this
      return [Expand (AST.exprPos ast) expr]
    AST.VerifyEnable pos nm -> do
      ruleNames <- gets (mstcRuleNames . vtsMSTC)
      checkRuleIsDefined pos nm ruleNames
      return [VerifyEnable nm]
    AST.VerifyDisable pos nm -> do
      ruleNames <- gets (mstcRuleNames . vtsMSTC)
      checkRuleIsDefined pos nm ruleNames
      return [VerifyDisable nm]
    AST.VerifyAt pos astPC astCmd -> do
      pc <- typecheckPC pos astPC
      cmds <- resolveVerifyCommand astCmd
      return [VerifyAt pc cmds]
    AST.VerifyBlock cmds ->
      concat <$> mapM resolveVerifyCommand cmds

resolveValidationPlan :: MethodSpecTypecheckContext
                      -> Map JSS.PC ExprActualTypeMap
                      -> [AST.MethodSpecDecl] -> IO ValidationPlan
resolveValidationPlan mstc exprTypes decls = 
  case filter isValidationPlanDecl decls of
    _:_:_ -> 
      let msg = "Multiple validation approaches set in method specification."
       in throwIOExecException (mstcPos mstc) (ftext msg) ""
    [AST.QuickCheck _ n mlimit] -> return $ QuickCheck n mlimit
    [AST.Verify _ cmds] ->
       let initVTS = VTS { vtsMSTC = mstc
                         , vtsExprTypes = exprTypes
                         , vtsPC = Nothing
                         }
        in Verify <$> evalStateT (resolveVerifyCommand cmds) initVTS
    _ -> error "internal: resolveValidationPlan reached illegal state."

{- TODO: Fix this
resolveValidationPlan mpos method cmds =
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
         -}

-- MethodSpecIR {{{1

data MethodSpecIR = MSIR {
    methodSpecPos :: Pos
    -- | Class used for this instance.
  , methodSpecIRThisClass :: JSS.Class
    -- | Class where method is defined.
  , methodSpecIRMethodClass :: JSS.Class
    -- | Method to verify.
  , methodSpecIRMethod :: JSS.Method
    -- | Class names expected to be initialized using JVM "/" separators.
    -- (as opposed to Java "." path separators).
  , initializedClasses :: [String]
    -- | Behavior specifications for method at different PC values.
    -- A list is used because the behavior may depend on the inputs.
  , methodSpecBehaviors :: Map JSS.PC [BehaviorSpec]
    -- | Describes how the method is expected to be validatioed.
  , methodSpecValidationPlan :: ValidationPlan
  } deriving (Show)

-- | Return user printable name of method spec (currently the class + method name).
methodSpecName :: MethodSpecIR -> String
methodSpecName ir =
 let clName = JSS.className (methodSpecIRThisClass ir)
     mName = JSS.methodName (methodSpecIRMethod ir)
  in slashesToDots clName ++ ('.' : mName)

-- | Interprets AST method spec commands to construct an intermediate
-- representation that
resolveMethodSpecIR :: TC.GlobalBindings
                    -> Set String -- ^ Names of rules in spec.
                    -> Pos
                    -> JSS.Class
                    -> String
                    -> [AST.MethodSpecDecl]
                    -> IO MethodSpecIR
resolveMethodSpecIR gb ruleNames pos thisClass mName cmds = do
  let cb = TC.codeBase gb
  (methodClass,method) <- findMethod cb pos mName thisClass
  let mstc = MSTC { mstcPos = pos
                  , mstcGlobalBindings = gb
                  , mstcClass = thisClass
                  , mstcMethod = method
                  , mstcRuleNames = ruleNames
                  }
  btc <- resolveBTC mstc cmds
  -- Get list of initial superclasses.
  superClasses <- JSS.supers cb thisClass
  -- Resolve other declarations tactic.
  let isVarDecl AST.VarDecl{} = True
      isVarDecl _ = False
  methodBehavior <- resolveBehaviorSpecs pos btc 0 $
    AST.Block [ cmd | AST.Behavior cmd <- cmds, not (isVarDecl cmd) ]
  --  Resolve behavior specs at other PCs.
  let specAtCmds = [ (specPos, pc, bcmds) | AST.SpecAt specPos pc bcmds <- cmds ]
  localBehaviors <- forM specAtCmds $ \(specPos,astPC,bcmds) -> do
      pc <- typecheckPC specPos astPC
      bs <- resolveBehaviorSpecs pos btc pc bcmds
      return (pc, bs)
  -- TODO: Check that no duplicates appear in local behavior specifications.
  let allBehaviors = Map.fromList $ (0, methodBehavior) : localBehaviors
  plan <- resolveValidationPlan mstc (Map.map fst allBehaviors) cmds
  -- Return IR.
  return MSIR { methodSpecPos = pos
              , methodSpecIRThisClass = thisClass
              , methodSpecIRMethodClass = methodClass
              , methodSpecIRMethod = method
              , initializedClasses = map JSS.className superClasses
              , methodSpecBehaviors = Map.map snd allBehaviors
              , methodSpecValidationPlan = plan
              }
