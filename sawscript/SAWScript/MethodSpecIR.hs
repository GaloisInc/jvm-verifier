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
  ( -- * MethodSpec record
    MethodSpecIR
  , specName
  , specPos
  , specThisClass
  , specMethod
  , specMethodClass
  , specInitializedClasses
  , specBehaviors
  , specValidationPlan
  , resolveMethodSpecIR
    -- * Method behavior.
  , BehaviorSpec
  , bsPC
  , bsRefExprs
  , bsMayAliasSet
  , RefEquivConfiguration
  , bsRefEquivClasses
  , BehaviorCommand(..)
  , bsCommands
    -- * Equivalence classes for references.
  , JavaExprEquivClass
  , ppJavaExprEquivClass
    -- * Validation plan
  , VerifyCommand(..)
  , ValidationPlan(..)
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
import qualified SAWScript.CongruenceClosure as CC
import qualified SAWScript.MethodAST as AST
import qualified SAWScript.TypeChecker as TC
import SAWScript.CongruenceClosure (CCSet)
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

-- Alias definitions {{{1

type JavaExprEquivClass = [TC.JavaExpr]

-- | Returns a name for the equivalence class.
ppJavaExprEquivClass :: JavaExprEquivClass -> String
ppJavaExprEquivClass [] = error "internal: ppJavaExprEquivClass"
ppJavaExprEquivClass [expr] = show expr
ppJavaExprEquivClass cl = "{ " ++ intercalate ", " (map show (sort cl)) ++ " }"

-- MethodTypecheckContext {{{1

-- | Global context for method spec typechecker.
data MethodTypecheckContext = MTC {
         -- | Position of method spec declaration.
         mtcPos :: Pos
         -- Bindings at global level.
       , mtcGlobalBindings :: TC.GlobalBindings
         -- | Class we are currently parsing.
       , mtcClass :: JSS.Class
         -- | Method that spec is for.
       , mtcMethod :: JSS.Method
         -- | Names of rules (used for resolving verify commands).
       , mtcRuleNames :: Set String
       }

typecheckerConfig :: MethodTypecheckContext -- ^ Context for typechecker
                  -> JSS.PC -- ^ PC to parse from.
                  -> ExprActualTypeMap -- ^ Maps Java expressions for references to actual type.
                  -> Map String TC.MixedExpr -- ^ Local bindings
                  -> TC.TCConfig
typecheckerConfig mtc pc actualTypeMap localBindings =
  TC.TCC { TC.globalBindings = mtcGlobalBindings mtc
         , TC.localBindings = localBindings
         , TC.methodInfo = Just
             TC.MethodInfo { TC.miClass  = mtcClass mtc
                           , TC.miMethod = mtcMethod mtc
                           , TC.miPC = pc
                           , TC.miJavaExprType = flip Map.lookup actualTypeMap
                           }
         }

-- BehaviorSpec {{{1

-- | Postconditions used for implementing behavior specification.
data BehaviorCommand
   = InputValue TC.JavaExpr DagType
   | AssertValue Pos TC.JavaExpr TC.LogicExpr
     -- | An assertion that is assumed to be true in the specificaiton.
   | AssertPred Pos TC.LogicExpr
     -- | An assumption made in a conditional behavior specification.
   | AssumePred TC.LogicExpr
     -- | Assign Java expression the value given by the mixed expression.
   | EnsureInstanceField Pos TC.JavaExpr JSS.FieldId TC.MixedExpr
     -- | Assign array value of Java expression the value given by the rhs.
   | EnsureArray Pos TC.JavaExpr TC.LogicExpr
     -- | Modify the Java expression to an arbitrary value.
     -- May point to integral type or array.
   | ModifyInstanceField TC.JavaExpr JSS.FieldId
     -- | Modify the Java array to an arbitrary value.
     -- May point to integral type or array.
   | ModifyArray TC.JavaExpr DagType
     -- | Specifies value method returns.
   | Return TC.MixedExpr
  deriving (Show)

data BehaviorSpec = BS {
         -- | Program counter for spec.
         bsPC :: JSS.PC
         -- | Maps all expressions seen along path to actual type.
       , bsActualTypeMap :: ExprActualTypeMap
         -- | Stores which Java expressions must alias each other.
       , bsMustAliasSet :: CCSet TC.JavaExprF
         -- | May alias relation between Java expressions.
       , bsMayAliasClasses :: [[TC.JavaExpr]]
         -- | Commands to execute in reverse order.
       , bsReversedCommands :: [BehaviorCommand]
       } deriving (Show)

-- | Returns list of all Java expressions that are references.
bsExprs :: BehaviorSpec -> [TC.JavaExpr]
bsExprs bs = Map.keys (bsActualTypeMap bs)

-- | Returns list of all Java expressions that are references.
bsRefExprs :: BehaviorSpec -> [TC.JavaExpr]
bsRefExprs bs = filter TC.isRefJavaExpr (bsExprs bs)

bsMayAliasSet :: BehaviorSpec -> CCSet TC.JavaExprF
bsMayAliasSet bs =
  foldr CC.insertEquivalenceClass
        (bsMustAliasSet bs)
        (bsMayAliasClasses bs)

-- | Check that all expressions that may alias have equal types.
bsCheckAliasTypes :: Pos -> BehaviorSpec -> IO ()
bsCheckAliasTypes pos bs = mapM_ checkClass (CC.toList (bsMayAliasSet bs))
  where atm = bsActualTypeMap bs
        checkClass [] = error "internal: Equivalence class empty"
        checkClass (x:l) = do
          let Just xType = Map.lookup x atm
          forM l $ \y -> do
            let Just yType = Map.lookup x atm
            when (xType /= yType) $ do
              let msg = "Different types are assigned to " ++ show x ++ " and " ++ show y ++ "."
                  res = "All references that may alias must be assigned the same type."
              throwIOExecException pos (ftext msg) res

type RefEquivConfiguration = [(JavaExprEquivClass, TC.JavaActualType)]

-- | Returns all possible potential equivalence classes for spec.
bsRefEquivClasses :: BehaviorSpec -> [RefEquivConfiguration]
bsRefEquivClasses bs = 
  map (map parseSet . CC.toList) $ Set.toList $
    mayAliases (bsMayAliasClasses bs) (bsMustAliasSet bs)
 where parseSet l@(e:_) =
         case Map.lookup e (bsActualTypeMap bs) of
           Just tp -> (l,tp)
           Nothing -> error $ "internal: bsRefEquivClass given bad expression: " ++ show e
       parseSet [] = error "internal: bsRefEquivClasses given empty list."

-- BehaviorTypechecker {{{1

data BehaviorTypecheckState = BTS {
         btsPC :: JSS.PC
         -- | Maps expressions to actual type (forgets expressions within conditionals and blocks).
       , btsActualTypeMap :: ExprActualTypeMap
         -- | Maps let bindings already seen to position they were defined.
       , btsLetBindings :: Map String (Pos, TC.MixedExpr)
         -- | Flag indicating if return has been set.
       , btsReturnSet :: Bool
         -- | Paths along execution.
       , btsPaths :: [BehaviorSpec]
       }

type BehaviorTypechecker = StateT BehaviorTypecheckState (ReaderT MethodTypecheckContext IO)

-- BehaviorTypechecker Utilities {{{1

-- | Get codebase used in behavior typechecker.
getBTCCodebase :: BehaviorTypechecker JSS.Codebase
getBTCCodebase = asks (TC.codeBase . mtcGlobalBindings)

forEachPath_ :: (BehaviorSpec -> BehaviorTypechecker a) -> BehaviorTypechecker ()
forEachPath_ fn = mapM_ fn =<< gets btsPaths

modifyPaths :: (BehaviorSpec -> BehaviorSpec) -> BehaviorTypechecker ()
modifyPaths fn = do 
  bts <- get
  put bts { btsPaths = map fn (btsPaths bts) }

-- Actual Type utilities {{{2

-- | Checks actual type is undefined.
checkActualTypeUndefined :: Pos -> TC.JavaExpr -> BehaviorTypechecker ()
checkActualTypeUndefined pos expr = do
  --Check type is not defined
  forEachPath_ $ \bs -> do
    when (Map.member expr (bsActualTypeMap bs)) $
      let msg = "The Java expression \'" ++ show expr ++ "\' has been already defined."
       in throwIOExecException pos (ftext msg) ""

-- | Records that the given expression is bound to the actual type.
recordActualType :: Pos -> TC.JavaExpr -> TC.JavaActualType -> BehaviorTypechecker ()
recordActualType pos expr at = do
  -- Record actual type undefined or unchanged.
  bts <- get
  newPaths <- forM (btsPaths bts) $ \bs ->
    case Map.lookup expr (bsActualTypeMap bs) of
      Nothing ->
        return bs { bsActualTypeMap = Map.insert expr at (bsActualTypeMap bs)
                  , bsMustAliasSet = 
                      if JSS.isRefType (TC.jssTypeOfJavaExpr expr) then
                        CC.insertTerm expr (bsMustAliasSet bs)
                      else
                        bsMustAliasSet bs
                  }
      Just prevAt -> do
        when (at /= prevAt) $
          let msg = "\"" ++ show expr ++ "\" is already defined."
           in throwIOExecException pos (ftext msg) ""
        return bs
  -- Update main state.
  put bts { btsActualTypeMap = Map.insert expr at (btsActualTypeMap bts)
          , btsPaths = newPaths }

-- | Returns actual type of Java expression.
getActualType :: Pos -> TC.JavaExpr -> BehaviorTypechecker TC.JavaActualType
getActualType pos expr = do
  typeMap <- gets btsActualTypeMap
  case Map.lookup expr typeMap of
    Just tp -> return tp
    Nothing ->
      let msg = "The type of " ++ show expr ++ " has not been defined."
          res = "Please add a declaration to indicate the concrete type of this Java expression."
       in throwIOExecException pos (ftext msg) res

{-
-- | Returns equivalence class of Java expression in translator step.
-- N.B. If this expression denotes a primitive type or does not belong to an
-- equivalence class, this returns a singleton list containing only the
-- expression.
getJavaEquivClass :: TC.JavaExpr -> BehaviorTypechecker JavaExprEquivClass
getJavaEquivClass expr = Map.findWithDefault [expr] expr <$> gets btsMayAliasMap
-}

-- Exception utilities {{{2

-- | Throw IO exception indicating name was previously defined.
throwVarIsPreviouslyDefined :: MonadIO m => Pos -> Pos -> String -> m ()
throwVarIsPreviouslyDefined pos absPrevPos name = do
  relPos <- liftIO $ posRelativeToCurrentDirectory absPrevPos
  throwIOExecException pos
                       (ftext "The variable " <+> quotes (text name)
                          <+> ftext "is already bound at "
                          <+> text (show relPos) <> char '.')
                       ("Please ensure all names are distinct.")

throwInvalidAssignment :: MonadIO m => Pos -> String -> String -> m a
throwInvalidAssignment pos lhs tp =
  let msg = lhs ++ " cannot be assigned a value with type " ++ tp ++ "."
   in throwIOExecException pos (ftext msg) ""

checkLogicExprIsPred :: MonadIO m => Pos -> TC.LogicExpr -> m ()
checkLogicExprIsPred pos expr =
  case TC.typeOfLogicExpr expr of
    SymBool -> return ()
    _ -> let msg = "Expression does not denote a predicate."
          in throwIOExecException pos (ftext msg) ""


-- Typecheck utilities {{{2

type Typechecker ast res = TC.TCConfig -> ast -> IO res

runTypechecker :: Typechecker ast res -> ast -> BehaviorTypechecker res
runTypechecker typeChecker expr = do
  msts <- ask
  pc <- gets btsPC
  actualTypes <- gets btsActualTypeMap
  letBindings <- Map.map snd <$> gets btsLetBindings
  let cfg = typecheckerConfig msts pc actualTypes letBindings
  liftIO $ typeChecker cfg expr

-- | Check that a type declaration has been provided for this expression.
typecheckRecordedJavaExpr :: Typechecker AST.Expr TC.JavaExpr
                          -> AST.Expr
                          -> BehaviorTypechecker (TC.JavaExpr, TC.JavaActualType)
typecheckRecordedJavaExpr typeChecker astExpr = do
  let pos = AST.exprPos astExpr
  expr <- runTypechecker typeChecker astExpr
  at <- getActualType pos expr
  return (expr, at)

-- | Typecheckes a 'valueOf' expression and returns Java expression inside
-- of it.  The type must be an array, and if a DagType is provided, the expression
-- must be compatible with the type.
typecheckValueOfLhs :: AST.Expr -> Maybe DagType -> BehaviorTypechecker (TC.JavaExpr, DagType)
typecheckValueOfLhs astExpr maybeExpectedType = do
  let pos = AST.exprPos astExpr
  (expr, at) <- typecheckRecordedJavaExpr TC.tcValueOfExpr astExpr
  -- Check expression is a compatible array type.
  unless (JSS.isRefType (TC.jssTypeOfJavaExpr expr)) $ do
    let msg = "Found primitive value " ++ show expr ++ " where reference is expected."
    throwIOExecException pos (ftext msg) ""
  case at of
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

coercePrimitiveExpr :: Pos
                    -> String
                    -> JSS.Type -- ^ Expected left-hand side type.
                    -> TC.LogicExpr
                    -> BehaviorTypechecker TC.LogicExpr
coercePrimitiveExpr pos lhsName javaType expr = do
  oc <- asks (TC.opCache . mtcGlobalBindings)
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
    _ -> throwInvalidAssignment pos lhsName (ppType valueType)

typecheckLogicExpr :: Pos -> String -> JSS.Type -> AST.Expr
                   -> BehaviorTypechecker TC.LogicExpr
typecheckLogicExpr lhsPos lhsName lhsType rhsAst =
  -- Check lhs can be assigned value in rhs (coercing if necessary).
  coercePrimitiveExpr lhsPos lhsName lhsType
    =<< runTypechecker TC.tcLogicExpr rhsAst

typecheckMixedExpr :: Pos -> String -> JSS.Type -> AST.Expr
                   -> BehaviorTypechecker (TC.MixedExpr, TC.JavaActualType)
typecheckMixedExpr lhsPos lhsName lhsType rhsAst =
  if JSS.isRefType lhsType then do
    rhsExpr <- runTypechecker TC.tcJavaExpr rhsAst
    -- Check lhs can be assigned value on rhs.
    let rhsType = TC.jssTypeOfJavaExpr rhsExpr
    cb <- getBTCCodebase
    typeOk <- liftIO $ JSS.isSubtype cb rhsType lhsType
    unless (typeOk) $ do
      throwInvalidAssignment lhsPos lhsName (show rhsType)
    -- Get actual type.
    at <- getActualType (AST.exprPos rhsAst) rhsExpr
    -- Return result.
    return (TC.JE rhsExpr, at)
  else do
    -- Check lhs can be assigned value in rhs (coercing if necessary).
    rhsExpr <- typecheckLogicExpr lhsPos lhsName lhsType rhsAst
    return (TC.LE rhsExpr, TC.PrimitiveType lhsType)

-- Command utilities {{{2

-- | Return commands in behavior in order they appeared in spec.
bsCommands :: BehaviorSpec -> [BehaviorCommand]
bsCommands = reverse . bsReversedCommands

-- | Add command to typechecker.
addCommand :: BehaviorCommand -> BehaviorTypechecker ()
addCommand bc = modifyPaths $ \bs ->
  bs { bsReversedCommands = bc : bsReversedCommands bs }

-- | Make sure expr can be assigned a postcondition.
checkValuePostconditionTarget :: Pos -> TC.JavaExpr -> BehaviorTypechecker ()
checkValuePostconditionTarget pos (CC.Term expr) = do
  case expr of
    TC.This _ ->
      let msg = "Cannot defined post-conditions on \'this\'."
       in throwIOExecException pos (ftext msg) ""
    TC.Arg _ _ ->
      let msg = "Cannot defined post-conditions on argument values."
       in throwIOExecException pos (ftext msg) ""
    TC.Local _ _ _ -> 
      let msg = "Cannot defined post-conditions on local values."
       in throwIOExecException pos (ftext msg) ""
    TC.InstanceField{} -> return ()

-- resolveDecl {{{1

-- | Code for parsing a method spec declaration.
resolveDecl :: [AST.BehaviorDecl] -> BehaviorTypechecker ()
resolveDecl [] = return ()
resolveDecl (AST.VarDecl _ exprAstList typeAst:r) = do
  -- Get actual type.
  at <- runTypechecker TC.tcActualType typeAst
  -- Parse expressions.
  cb <- getBTCCodebase
  forM_ exprAstList $ \exprAst -> do
    -- Typecheck Java expression.
    let pos = AST.exprPos exprAst
    expr <- runTypechecker TC.tcJavaExpr exprAst
    -- Check that type of exprAst and the type of tp are compatible.
    do let exprType = TC.jssTypeOfJavaExpr expr
           tgtType = TC.jssTypeOfActual at
       typeOk <- liftIO $ JSS.isSubtype cb tgtType exprType
       unless typeOk $
         let msg = ftext ("The expression " ++ show expr ++ " is incompatible with")
                     <+> TC.ppASTJavaType typeAst <> char '.'
          in throwIOExecException pos msg ""
    -- Record actual type.
    checkActualTypeUndefined pos expr
    recordActualType pos expr at
    -- Record input command if needed.
    case TC.logicTypeOfActual at of
      Nothing -> return ()
      Just tp -> addCommand (InputValue expr tp)
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.MethodLet pos lhs rhsAst:r) = do
  -- Typecheck rhs.
  rhsExpr <- runTypechecker TC.tcMixedExpr rhsAst
  -- Record variable binding.
  do bts <- get
     let locals = btsLetBindings bts
     case Map.lookup lhs locals of
       Just (prevPos,_) -> throwVarIsPreviouslyDefined pos prevPos lhs
       Nothing -> return ()
     put bts { btsLetBindings = Map.insert lhs (pos,rhsExpr) locals }
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.MayAlias _ exprAstList:r) = do
  -- Record may alias relation in each path.
  exprList <- forM exprAstList $ \exprAst -> do
     (expr,_) <- typecheckRecordedJavaExpr TC.tcJavaExpr exprAst
     -- Check expression is a reference.
     unless (JSS.isRefType (TC.jssTypeOfJavaExpr expr)) $ do
       let msg = "\'mayAlias\' provided a non-reference value."
        in throwIOExecException (AST.exprPos exprAst) (ftext msg) ""
     return expr
  -- Update each path with new alias.
  modifyPaths $ \bs -> bs { bsMayAliasClasses = exprList:bsMayAliasClasses bs }
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.AssertPred pos ast:r) = do
  -- Typecheck expression.
  expr <- runTypechecker TC.tcLogicExpr ast
  -- Check expr is a Boolean
  checkLogicExprIsPred (AST.exprPos ast) expr
  -- Add assertion.
  addCommand (AssertPred pos expr)
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.AssertImp pos lhsAst@(AST.ApplyExpr _ "valueOf" _) rhsAst:r) = do
  -- Typecheck rhs
  rhsExpr <- runTypechecker TC.tcLogicExpr rhsAst
  let rhsType = TC.typeOfLogicExpr rhsExpr
  -- Typecheck lhs
  (lhsExpr, _) <- typecheckValueOfLhs lhsAst (Just rhsType)
  -- Record assertion.
  addCommand (AssertValue pos lhsExpr rhsExpr)
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.AssertImp pos lhsAst rhsAst:r) = do
  -- Typecheck lhs expression.
  let lhsPos = AST.exprPos lhsAst
  lhsExpr <- runTypechecker TC.tcJavaExpr lhsAst
  let lhsName = "\'" ++ show lhsExpr ++ "\'"
  let lhsType = TC.jssTypeOfJavaExpr lhsExpr
  -- Typecheck right-hand side.
  if JSS.isRefType lhsType then do
    (rhsExpr,rhsActualType) <- typecheckRecordedJavaExpr TC.tcJavaExpr rhsAst
    -- Check lhs can be assigned value on rhs.
    let rhsJSSType = TC.jssTypeOfJavaExpr rhsExpr
    cb <- getBTCCodebase
    typeOk <- liftIO $ JSS.isSubtype cb rhsJSSType lhsType
    unless (typeOk) $ do
      throwInvalidAssignment lhsPos lhsName (show rhsJSSType)
    -- Record actual type.
    recordActualType lhsPos lhsExpr rhsActualType
    -- Record must alias assertion.
    modifyPaths $ \bs ->
      bs { bsMustAliasSet = CC.insertEquation lhsExpr rhsExpr (bsMustAliasSet bs) }
  else do
    -- Check lhs can be assigned value in rhs (coercing if necessary).
    rhsExpr <- typecheckLogicExpr lhsPos lhsName lhsType rhsAst
    -- Record actual type.
    recordActualType lhsPos lhsExpr (TC.PrimitiveType lhsType)
    -- Record value assertion.
    addCommand $ AssertValue pos lhsExpr rhsExpr
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.EnsureImp pos lhsAst@(AST.ApplyExpr _ "valueOf" _) rhsAst:r) = do
  -- Typecheck rhs
  rhsExpr <- runTypechecker TC.tcLogicExpr rhsAst
  let rhsType = TC.typeOfLogicExpr rhsExpr
  -- Typecheck lhs
  (lhsExpr, _) <- typecheckValueOfLhs lhsAst (Just rhsType)
  -- Update postcondition.
  addCommand (EnsureArray pos lhsExpr rhsExpr)
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.EnsureImp _ astLhsExpr astRhsExpr:r) = do
  -- Typecheck lhs expression.
  let lhsPos = AST.exprPos astLhsExpr
  (lhsExpr,_) <- typecheckRecordedJavaExpr TC.tcJavaExpr astLhsExpr
  let lhsName = "\'" ++ show lhsExpr ++ "\'"
  let lhsType = TC.jssTypeOfJavaExpr lhsExpr
  -- Typecheck right hand side.
  (rhsExpr,_) <- typecheckMixedExpr lhsPos lhsName lhsType astRhsExpr
  -- Add postcondition based on left-hand side expression.
  checkValuePostconditionTarget lhsPos lhsExpr
  case lhsExpr of
    CC.Term (TC.InstanceField ref f) ->
      addCommand (EnsureInstanceField lhsPos ref f rhsExpr)
    _ -> error "internal: Unexpected expression"
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.Modify _ astExprs:r) = mapM_ resolveExpr astExprs >> resolveDecl r
  where resolveExpr ast@(AST.ApplyExpr _ "valueOf" _) = do
          (expr, tp) <- typecheckValueOfLhs ast Nothing
          -- Add modify statement.
          addCommand $ ModifyArray expr tp
        resolveExpr ast = do
          ---- Typecheck ast
          let pos = AST.exprPos ast
          (expr, _) <- typecheckRecordedJavaExpr TC.tcJavaExpr ast
          ---- Check this type is valid for a modifies clause.
          let exprType = TC.jssTypeOfJavaExpr expr
          when (JSS.isRefType exprType)  $ do
            let msg = "Modify may not refer to reference types."
            throwIOExecException pos (ftext msg) ""
          -- Add modify statement.
          checkValuePostconditionTarget pos expr
          case expr of
            CC.Term (TC.InstanceField ref f) -> 
              addCommand (ModifyInstanceField ref f)
            _ -> error "internal: Unexpected expression"
resolveDecl (AST.Return pos ast:r) = do
  method <- asks mtcMethod
  case JSS.methodReturnType method of
    Nothing ->
      let msg = "Return value specified for \'" ++ JSS.methodName method 
                   ++ "\', but method returns \'void\'."
       in throwIOExecException pos (ftext msg) ""
    Just returnType -> do
      -- Typecheck expression.
      (expr,_) <- typecheckMixedExpr (AST.exprPos ast) "The return value" returnType ast
      -- Record return is set.
      do bts <- get
         when (btsReturnSet bts) $ do
           let msg = "Multiple return values specified in a single method spec."
           throwIOExecException pos (ftext msg) ""
         put bts { btsReturnSet = True }
      -- Add return statement.
      addCommand (Return expr)
  -- Resolve remaining declarations.
  resolveDecl r
resolveDecl (AST.MethodIf p c t:r) =
  resolveDecl (AST.MethodIfElse p c t (AST.Block []):r)
resolveDecl (AST.MethodIfElse pos condAst t f:r) = do
  -- Typecheck condition.
  cond <- runTypechecker TC.tcLogicExpr condAst
  checkLogicExprIsPred (AST.exprPos condAst) cond
  -- Branch execution.
  bts <- get
  lift $ do
    tBts <- flip execStateT bts $ do
      -- Add assumption.
      addCommand (AssumePred cond)
      -- Resolve commands.
      resolveDecl [t]
    fBts <- flip execStateT bts $ do
      let negCond = TC.Apply bNotOp [cond]
      addCommand $ AssumePred negCond
      resolveDecl [f]
    when (btsReturnSet tBts /= btsReturnSet fBts) $ do
      let msg = "Return value set in one branch, but not the other."
          res = "Please ensure that both branches set the return value."
       in throwIOExecException pos (ftext msg) res
    let rBts = BTS { -- Use originl local type map and let bindings.
                     btsPC = btsPC bts
                   , btsActualTypeMap = btsActualTypeMap bts
                   , btsLetBindings = btsLetBindings bts
                     -- Use return set from tBts.
                   , btsReturnSet = btsReturnSet tBts
                     -- Union paths.
                   , btsPaths = btsPaths tBts ++ btsPaths fBts
                   }
    evalStateT (resolveDecl r) rBts
resolveDecl (AST.Block ast:r) = do
  oldBts <- get
  -- Resolve declarations in block.
  resolveDecl ast
  -- Forget concrete types and let bindings.
  modify $ \bts -> bts { btsActualTypeMap = btsActualTypeMap oldBts
                       , btsLetBindings = btsLetBindings oldBts }
  -- Resolve remaining declarations.
  resolveDecl r

-- CCSet utilities {{1

-- | 
splitClass :: (CC.OrdFoldable f, CC.Traversable f)
           => [CC.Term f] -> Set (CCSet f) -> Set (CCSet f)
splitClass [] sets = sets
splitClass (h:l) sets = splitClass l (sets `Set.union` newSets)
 where insertEqualToH t = Set.map (CC.insertEquation h t) sets
       newSets = Set.unions (map insertEqualToH l)

-- | Returns all congruence-closed sets generated by the may alias
-- configurations.
mayAliases :: (CC.OrdFoldable f, CC.Traversable f)
           => [[CC.Term f]] -> CCSet f -> Set (CCSet f)
mayAliases l s = foldr splitClass (Set.singleton s) l


-- resolveBehaviorSpecs {{{1

resolveBehaviorSpecs :: MethodTypecheckContext 
                     -> JSS.PC
                     -> AST.BehaviorDecl
                     -> IO (ExprActualTypeMap -- ^ Actual types that agree between specs.
                           , [BehaviorSpec])
resolveBehaviorSpecs mtc pc block = do
  let method = mtcMethod mtc
  let initTypeMap | JSS.methodIsStatic method = Map.empty
                  | otherwise = 
                     let t = CC.Term (TC.This (JSS.className (mtcClass mtc)))
                         at = TC.ClassInstance (mtcClass mtc)
                      in Map.singleton t at
      initPath = BS { bsPC = pc
                    , bsActualTypeMap = initTypeMap
                    , bsMustAliasSet = CC.empty
                    , bsMayAliasClasses = []
                    , bsReversedCommands = []
                    }
      initBts = BTS { btsPC = pc
                    , btsActualTypeMap = initTypeMap
                    , btsLetBindings = Map.empty
                    , btsPaths = [initPath]
                    , btsReturnSet = False
                    }
  -- Flatten commands so top level type information is available to expression parser.
  let flatDecls = case block of
                    AST.Block cmds -> cmds
                    _ -> [block]
  bts <- flip runReaderT mtc $
           flip execStateT initBts $
             resolveDecl flatDecls
  -- Check expressions that may alias to verify they have equivalent types.
  mapM_ (bsCheckAliasTypes (mtcPos mtc)) (btsPaths bts)
  if pc == 0 then
    -- TODO: Check all arguments are defined.
    return ()
  else
    -- TODO: Check all expected locals are defined.
    return ()
  -- Ensure returns is set if method has return value.
  when (isJust (JSS.methodReturnType method) && not (btsReturnSet bts)) $
    let msg = "The Java method \'" ++ JSS.methodName method
                     ++ "\' has a return value, but the spec does not define it."
     in throwIOExecException (mtcPos mtc) (ftext msg) ""
  -- Return paths parsed from this spec.
  return (btsActualTypeMap bts, btsPaths bts)

-- resolveValidationPlan {{{1

-- | Commands issued to verify method.
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

-- | Returns true if this is a validation plan declaration.
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
         vtsRuleNames :: Set String
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
      expr <- error "Not Yet implemented: resolveVerifyCommand Expand" -- TODO: Fix this
      return [Expand (AST.exprPos ast) expr]
    AST.VerifyEnable pos nm -> do
      ruleNames <- gets vtsRuleNames
      checkRuleIsDefined pos nm ruleNames
      return [VerifyEnable nm]
    AST.VerifyDisable pos nm -> do
      ruleNames <- gets vtsRuleNames
      checkRuleIsDefined pos nm ruleNames
      return [VerifyDisable nm]
    AST.VerifyAt pos astPC astCmd -> do
      pc <- typecheckPC pos astPC
      cmds <- resolveVerifyCommand astCmd
      return [VerifyAt pc cmds]
    AST.VerifyBlock cmds ->
      concat <$> mapM resolveVerifyCommand cmds

throwMultipleValidationPlans :: Pos -> IO a
throwMultipleValidationPlans pos =
  let msg = "Multiple validation approaches set in method specification."
   in throwIOExecException pos (ftext msg) ""

resolveValidationPlan :: Set String -- ^ Names of rules in spec.
                      -> Map JSS.PC ExprActualTypeMap
                      -> [AST.MethodSpecDecl] -> IO ValidationPlan
resolveValidationPlan ruleNames exprTypes decls = 
  case filter isValidationPlanDecl decls of
    [] -> return $ Skip
    [AST.QuickCheck _ n mlimit] -> return $ QuickCheck n mlimit
    [AST.Verify _ cmds] ->
       let initVTS = VTS { vtsRuleNames = ruleNames
                         , vtsExprTypes = exprTypes
                         , vtsPC = Nothing
                         }
        in Verify <$> evalStateT (resolveVerifyCommand cmds) initVTS
    _:AST.QuickCheck p _ _:_ -> throwMultipleValidationPlans p
    _:AST.Verify p _:_       -> throwMultipleValidationPlans p
    _ -> error "internal: resolveValidationPlan reached illegal state."

-- MethodSpecIR {{{1

data MethodSpecIR = MSIR {
    specPos :: Pos
    -- | Class used for this instance.
  , specThisClass :: JSS.Class
    -- | Class where method is defined.
  , specMethodClass :: JSS.Class
    -- | Method to verify.
  , specMethod :: JSS.Method
    -- | Class names expected to be initialized using JVM "/" separators.
    -- (as opposed to Java "." path separators).
  , specInitializedClasses :: [String]
    -- | Behavior specifications for method at different PC values.
    -- A list is used because the behavior may depend on the inputs.
  , specBehaviors :: Map JSS.PC [BehaviorSpec]
    -- | Describes how the method is expected to be validatioed.
  , specValidationPlan :: ValidationPlan
  } deriving (Show)

-- | Return user printable name of method spec (currently the class + method name).
specName :: MethodSpecIR -> String
specName ir =
 let clName = JSS.className (specThisClass ir)
     mName = JSS.methodName (specMethod ir)
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
  let mtc = MTC { mtcPos = pos
                , mtcGlobalBindings = gb
                , mtcClass = thisClass
                , mtcMethod = method
                , mtcRuleNames = ruleNames
                }
  -- Get list of initial superclasses.
  superClasses <- JSS.supers cb thisClass
  -- Resolve behavior spec for PC 0.
  methodBehavior <- resolveBehaviorSpecs mtc 0 $
    AST.Block [ cmd | AST.Behavior cmd <- cmds ]
  --  Resolve behavior specs at other PCs.
  let specAtCmds = [ (specPos, pc, bcmds) | AST.SpecAt specPos pc bcmds <- cmds ]
  localBehaviors <- forM specAtCmds $ \(specPos,astPC,bcmds) -> do
      pc <- typecheckPC specPos astPC
      bs <- resolveBehaviorSpecs mtc pc bcmds
      return (pc, bs)
  -- TODO: Check that no duplicates appear in local behavior specifications.
  let allBehaviors = Map.fromList $ (0, methodBehavior) : localBehaviors
  -- Resolve verification plan.
  plan <- resolveValidationPlan ruleNames (Map.map fst allBehaviors) cmds
  -- Return IR.
  return MSIR { specPos = pos
              , specThisClass = thisClass
              , specMethodClass = methodClass
              , specMethod = method
              , specInitializedClasses = map JSS.className superClasses
              , specBehaviors = Map.map snd allBehaviors
              , specValidationPlan = plan
              }
