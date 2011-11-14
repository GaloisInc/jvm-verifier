{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards  #-}
{-# LANGUAGE ViewPatterns   #-}
module SAWScript.TypeChecker
  ( -- * Typechecking configuration.
    GlobalBindings(..)
  , TCConfig(..)
  , mkGlobalTCConfig
    -- * Typechecking type expressions.
  , tcType
    -- * Java Expressions
  , JavaExpr(..)
  , jssTypeOfJavaExpr
  , tcJavaExpr
  , tcValueOfExpr
    -- * Logic expressions
  , LogicExpr(..)
  , typeOfLogicExpr
  , logicExprVarNames
  , globalEval
  , tcLogicExpr
    -- * Mixed expressions
  , MixedExpr(..)
  , tcMixedExpr
  -- * Java types
  , ppASTJavaType
  , jssTypeOfASTJavaType
  , JavaRefActualType(..)
  , jssTypeOfActual
  , isActualSubtype
  , ppActualType
  , tcActualType
  ) where

-- Imports {{{2

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans
import Data.Int
import Data.List (intercalate)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set
import Text.PrettyPrint.HughesPJ

import Verinf.Symbolic

import qualified JavaParser as JSS
import qualified Execution.Codebase as JSS
import qualified SAWScript.MethodAST as AST
import SAWScript.TIMonad
import SAWScript.Utils
import Utils.Common

-- Typecheck DagType {{{1

-- | Convert expression type from AST into WidthExpr
tcheckExprWidth :: AST.ExprWidth -> WidthExpr
tcheckExprWidth (AST.WidthConst _ i  ) = constantWidth (Wx i)
tcheckExprWidth (AST.WidthVar   _ nm ) = varWidth nm
tcheckExprWidth (AST.WidthAdd   _ u v) = addWidth (tcheckExprWidth u) (tcheckExprWidth v)

-- | Convert expression type from AST into DagType.
-- Uses Executor monad for parsing record types.
tcType :: OpCache -> AST.ExprType -> DagType
tcType _ (AST.BitType       _)   = SymBool
tcType _ (AST.BitvectorType _ w) = SymInt (tcheckExprWidth w)
tcType oc (AST.Array _ w tp)     = SymArray (tcheckExprWidth w) (tcType oc tp)
tcType oc (AST.Record _ fields)  = SymRec def sub
  where names = [ nm | (_,nm,_) <- fields ]
        typeMap = Map.fromList [ (nm, tcType oc tp) | (_,nm,tp) <- fields ]
        def = getStructuralRecord oc (Set.fromList names)
        sub = emptySubst { shapeSubst = typeMap }
tcType _ (AST.ShapeVar _ v)        = SymShapeVar v

tcT :: AST.ExprType -> SawTI DagType
tcT tp = (\oc -> tcType oc tp) `fmap` gets opCache

-- Typechecking configuration {{{1

-- | Context for resolving top level expressions.
data GlobalBindings = GlobalBindings {
         codeBase      :: JSS.Codebase
       , ssOpts        :: SSOpts
       , opBindings    :: Map String OpDef
       , constBindings :: Map String (CValue,DagType)
       }

-- | Context for resolving expressions at the top level or within a method.
data TCConfig = TCC {
         opCache        :: OpCache
       , globalBindings :: GlobalBindings
       , methodInfo     :: Maybe (JSS.Method, JSS.Class)
       , localBindings  :: Map String MixedExpr
       , toJavaExprType :: Maybe (JavaExpr -> Maybe JavaRefActualType)
       }

mkGlobalTCConfig :: OpCache -> GlobalBindings -> Map String LogicExpr -> TCConfig
mkGlobalTCConfig opCache globalBindings lb = do
  TCC { opCache
      , globalBindings
      , methodInfo = Nothing
      , localBindings = Map.map LE lb
      , toJavaExprType = Nothing }


-- JavaExpr {{{1

-- | Identifies a reference to a Java value.
data JavaExpr
  = This String -- | Name of classname for this object.
  | Arg Int JSS.Type
  | Local String JSS.LocalVariableIndex JSS.Type
  | InstanceField JavaExpr JSS.FieldId

instance Eq JavaExpr where
  This _      == This _      = True
  Arg i _     == Arg j _     = i == j
  Local _ i _ == Local _ i' _ = i == i'
  InstanceField r1 f1 == InstanceField r2 f2 = r1 == r2 && f1 == f2
  _               == _               = False

instance Ord JavaExpr where
  This _      `compare` This _      = EQ
  This _      `compare` _           = LT
  _           `compare` This _      = GT
  Arg i _     `compare` Arg j _     = i `compare` j
  Arg _ _     `compare` _           = LT
  _           `compare` Arg _ _     = GT
  Local _ i _ `compare` Local _ i' _ = i `compare` i'
  Local _ _ _ `compare` _           = LT
  _           `compare` Local _ _ _ = GT
  InstanceField r1 f1 `compare` InstanceField r2 f2 =
        case r1 `compare` r2 of
          EQ -> f1 `compare` f2
          r  -> r

instance Show JavaExpr where
  show (This _)    = "this"
  show (Arg i _)   = "args[" ++ show i ++ "]"
  show (Local nm _ _) = "locals[" ++ nm ++ "]"
  show (InstanceField r f) = show r ++ "." ++ JSS.fieldIdName f

-- | Returns JSS Type of JavaExpr
jssTypeOfJavaExpr :: JavaExpr -> JSS.Type
jssTypeOfJavaExpr (This cl)   = JSS.ClassType cl
jssTypeOfJavaExpr (Arg _ tp)  = tp
jssTypeOfJavaExpr (Local _ _ tp)  = tp
jssTypeOfJavaExpr (InstanceField _ f) = JSS.fieldIdType f

tcJavaExpr :: TCConfig -> AST.Expr -> IO JavaExpr
tcJavaExpr cfg e = runTI cfg (tcJE e)

-- | Typecheck expression with form valueOf(args), returning java expression
-- inside args.
tcValueOfExpr ::  TCConfig -> AST.Expr -> IO JavaExpr
tcValueOfExpr cfg (AST.ApplyExpr _po "valueOf" [astExpr]) = do
  tcJavaExpr cfg astExpr
tcValueOfExpr _ (AST.ApplyExpr pos "valueOf" _) =
  let msg = ftext "Unexpected number of arguments to \"valueOf\"."
   in throwIOExecException pos msg ""
tcValueOfExpr _ _ = error "internal: tcValueOfExpr given illegal expression"


-- LogicExpr {{{1

-- | A type-checked expression which appears insider a global let binding,
-- method declaration, or rule term.
data LogicExpr
   = Apply Op [LogicExpr]
   | Cns CValue DagType
   | Var String DagType
     -- | Refers to the logical value of a Java expression.  For scalars,
     -- this is the value of the scalar with the number of bits equal to
     -- the stack width.  For arrays, this is the value of the array.
     -- Other reference types are unsupported.
   | JavaValue JavaExpr DagType
   deriving (Show)

-- | Return type of a typed expression.
typeOfLogicExpr :: LogicExpr -> DagType
typeOfLogicExpr (Apply     op _) = opResultType op
typeOfLogicExpr (Cns       _ tp) = tp
typeOfLogicExpr (JavaValue _ tp) = tp
typeOfLogicExpr (Var       _ tp) = tp

-- | Returns names of variables appearing in typedExpr.
logicExprVarNames :: LogicExpr -> Set String
logicExprVarNames (Apply _ exprs) = Set.unions (map logicExprVarNames exprs)
logicExprVarNames (Cns _ _)       = Set.empty
logicExprVarNames (JavaValue _ _) = Set.empty
logicExprVarNames (Var nm _)      = Set.singleton nm

-- | Evaluate a ground typed expression to a constant value.
globalEval :: LogicExpr -> CValue
globalEval expr = eval expr
  where ts = evalTermSemantics
        eval (Var _nm _tp) =
          error "internal: globalEval called with non-ground expression"
        eval (JavaValue _nm _tp) =
          error "internal: globalEval called with expression containing Java expressions."
        eval (Cns c tp) = runIdentity (tsConstant ts c tp)
        eval (Apply op args) = runIdentity (tsApplyOp ts op (V.map eval (V.fromList args)))

-- | Internal utility for flipping arguments to binary logic expressions.
flipBinOpArgs :: LogicExpr -> LogicExpr
flipBinOpArgs (Apply o [a, b]) = Apply o [b, a]
flipBinOpArgs e = error $ "internal: flipBinOpArgs: received: " ++ show e

-- | Typecheck a logic expression.
tcLogicExpr :: TCConfig -> AST.Expr -> IO LogicExpr
tcLogicExpr cfg e = runTI cfg (tcLE e)

-- MixedExpr {{{1

-- | A logic or Java expression.
data MixedExpr
  = LE LogicExpr
  | JE JavaExpr
  deriving (Show)

tcMixedExpr :: TCConfig -> AST.Expr -> IO MixedExpr
tcMixedExpr cfg e = runTI cfg (tcE e)

-- Typechecking Java types {{{1

jssTypeOfASTJavaType :: AST.JavaType -> JSS.Type
jssTypeOfASTJavaType tp =
  case tp of
    AST.BoolType _   -> JSS.BooleanType
    AST.ByteType _   -> JSS.ByteType
    AST.CharType _   -> JSS.CharType
    AST.DoubleType _ -> JSS.DoubleType
    AST.FloatType _  -> JSS.FloatType
    AST.IntType _    -> JSS.IntType
    AST.LongType _   -> JSS.LongType
    AST.ShortType _  -> JSS.ShortType
    AST.ArrayType eltTp _ -> JSS.ArrayType (jssTypeOfASTJavaType eltTp)
    AST.RefType _ names   -> JSS.ClassType (intercalate "/" names)

ppASTJavaType :: AST.JavaType -> Doc
ppASTJavaType tp =
  case tp of
    AST.BoolType _      -> text "boolean"
    AST.ByteType _      -> text "byte"
    AST.CharType _      -> text "char"
    AST.DoubleType _    -> text "double"
    AST.FloatType _     -> text "float"
    AST.IntType _       -> text "int"
    AST.LongType _      -> text "long"
    AST.ShortType _     -> text "short"
    AST.RefType _ nm    -> text (intercalate "." nm)
    AST.ArrayType etp l -> ppASTJavaType etp <> brackets (int l)



-- | Identifies concrete type of a Java expression, and possible initial value.
data JavaRefActualType
  = ClassInstance JSS.Class
  | ArrayInstance Int JSS.Type (Maybe CValue)
  deriving (Show)

instance Eq JavaRefActualType where
  ClassInstance c1 == ClassInstance c2 = JSS.className c1 == JSS.className c2
  ArrayInstance l1 tp1 v1 == ArrayInstance l2 tp2 v2 = l1 == l2 && tp1 == tp2 && v1 == v2
  _ == _ = False

jssTypeOfActual :: JavaRefActualType -> JSS.Type
jssTypeOfActual (ClassInstance x) = JSS.ClassType (JSS.className x)
jssTypeOfActual (ArrayInstance _ tp _) = JSS.ArrayType tp

-- @isActualSubtype cb x y@ returns True if @x@ is a subtype of @y@.
isActualSubtype :: JSS.Codebase -> JavaRefActualType -> JavaRefActualType -> IO Bool
isActualSubtype cb (ArrayInstance lx ex _) (ArrayInstance ly ey _)
  | lx == ly = JSS.isSubtype cb ex ey
  | otherwise = return False
isActualSubtype _ _ ArrayInstance{} = return False
isActualSubtype cb x y 
  = JSS.isSubtype cb (jssTypeOfActual x) (jssTypeOfActual y)

ppActualType :: JavaRefActualType -> String
ppActualType (ClassInstance x) = slashesToDots (JSS.className x)
ppActualType (ArrayInstance l tp _) = show tp ++ "[" ++ show l ++ "]"

-- | Convert AST.JavaType into JavaRefActualType if it is a reference.
tcActualType :: JSS.Codebase -> AST.JavaType -> IO (Maybe JavaRefActualType)
tcActualType _ (AST.ArrayType eltTp l) = do
  unless (0 <= l && toInteger l < toInteger (maxBound :: Int32)) $ do
    let msg  = "Array length " ++ show l ++ " is invalid."
    throwIOExecException (AST.javaTypePos eltTp) (ftext msg) ""
  let jssType = jssTypeOfASTJavaType eltTp
  return $ Just (ArrayInstance (fromIntegral l) jssType Nothing)
tcActualType cb (AST.RefType pos names) = do
  cl <- lookupClass cb pos (intercalate "/" names)
  return $ Just (ClassInstance cl)
tcActualType _ _ = return Nothing

-- SawTI {{{1

type SawTI = TI IO TCConfig

debugTI :: String -> SawTI ()
debugTI msg = do os <- gets (ssOpts . globalBindings)
                 liftIO $ debugVerbose os $ putStrLn msg

getMethodInfo :: SawTI (JSS.Method, JSS.Class)
getMethodInfo = do
  maybeMI <- gets methodInfo
  case maybeMI of
    Nothing -> error $ "internal: getMethodInfo called when parsing outside a method declaration"
    Just p -> return p

-- | Check argument count matches expected length
checkArgCount :: Pos -> String -> [a] -> Int -> SawTI ()
checkArgCount pos nm (length -> foundOpCnt) expectedCnt = do
  unless (expectedCnt == foundOpCnt) $
    typeErr pos $ ftext $ "Incorrect number of arguments to \'" ++ nm ++ "\'.  "
                        ++ show expectedCnt ++ " arguments were expected, but "
                        ++ show foundOpCnt ++ " arguments were found."

-- Core expression typechecking {{{1

-- | Typecheck expression as a Java expression.
tcJE :: AST.Expr -> SawTI JavaExpr
tcJE astExpr = do
  r <- tcE astExpr
  case r of
    JE e -> return e
    LE _ -> 
     let msg = ftext $ "\'" ++ show astExpr ++ "\' is not a valid Java expression."
      in typeErr (AST.exprPos astExpr) msg

-- | Typecheck expression as a logic expression.
tcLE :: AST.Expr -> SawTI LogicExpr
tcLE astExpr = do
  r <- tcE astExpr
  case r of
    LE e -> return e
    JE e -> do
      let javaType = jssTypeOfJavaExpr e
      when (JSS.isRefType javaType) $ do
        let msg = "Encountered a Java expression denoting a reference where a logical expression is expected."
        typeErr (AST.exprPos astExpr) (ftext msg)
      when (JSS.isFloatType javaType) $ do
        let msg = "Encountered a Java expression denoting a floating point value where a logical expression is expected."
        typeErr (AST.exprPos astExpr) (ftext msg)
      let dagType = SymInt (constantWidth (Wx (JSS.stackWidth javaType)))
      return $ JavaValue e dagType

-- | Convert AST expression into expression.
tcE :: AST.Expr -> SawTI MixedExpr
tcE (AST.ConstantInt p _) = typeErrWithR p msg rec
  where msg = ftext ("The use of constant literal requires a type-annotation")
        rec = "Please provide the bit-size of the constant with a type-annotation"
tcE (AST.ApplyExpr p nm _)
  | nm `elem` ["split", "trunc", "signedExt"] = typeErrWithR p msg rec
  where msg = ftext ("Use of operator '" ++ nm ++ "' requires a type-annotation.")
        rec = "Please provide an annotation for the surrounding expression."
tcE (AST.Var pos name) = do
  locals  <- gets localBindings
  globals <- gets (constBindings . globalBindings)
  case name `Map.lookup` locals of
    Just res -> return res
    Nothing -> do
      case name `Map.lookup` globals of
        Just (c,tp) -> return $ LE (Cns c tp)
        Nothing -> typeErr pos $ ftext $ "Unknown variable \'" ++ name ++ "\'."
tcE (AST.ConstantBool _ b) = return $ LE (Cns (mkCBool b) SymBool)
tcE (AST.MkArray p [])
  = typeErrWithR p (ftext ("Use of empty array-comprehensions requires a type-annotation")) "Please provide the type of the empty-array value"
tcE (AST.MkArray p (es@(_:_))) = do
  es' <- mapM tcLE es
  let go []                 = error "internal: impossible happened in tcE-non-empty-mkArray"
      go [(_, x)]           = return x
      go ((i, x):rs@((j, y):_))
        | x == y = go rs 
        | otherwise = mismatch p ("array elements " ++ show i ++ " and " ++ show j) x y
  t   <- go $ zip [(1::Int)..] $ map typeOfLogicExpr es'
  oc <- gets opCache
  return $ LE $ Apply (mkArrayOp oc (length es') t) es'
tcE (AST.TypeExpr pos (AST.ConstantInt posCnst i) astTp) = do
  tp <- tcT astTp
  let nonGround = typeErr pos $   text "The type" <+> text (ppType tp)
                              <+> ftext "bound to literals must be a ground type."
  case tp of
    SymInt (widthConstant -> Just (Wx w)) -> do
      warnRanges posCnst tp i w
      return $ LE $ Cns (mkCInt (Wx w) i) tp
    SymInt      _ -> nonGround
    SymShapeVar _ -> nonGround
    _             -> typeErr pos $   text "Incompatible type" <+> text (ppType tp)
                                 <+> ftext "assigned to integer literal."
tcE (AST.TypeExpr _ (AST.ApplyExpr appPos "split" astArgs) astResType) = do
  args <- mapM tcLE astArgs
  checkArgCount appPos "split" args 1
  resType <- tcT astResType
  let argType = typeOfLogicExpr (head args)
  case (argType, resType) of
    (  SymInt (widthConstant -> Just wl)
     , SymArray (widthConstant -> Just l) (SymInt (widthConstant -> Just w)))
      | wl == l * w -> do
        oc <- gets opCache
        return $ LE $ Apply (splitOp oc l w) args
    _ -> typeErr appPos $ ftext $ "Illegal arguments and result type given to \'split\'."
                                ++ " SAWScript currently requires that the argument is ground type, "
                                ++ " and an explicit result type is given."
tcE (AST.TypeExpr p (AST.MkArray _ []) astResType) = do
  resType <- tcT astResType
  case resType of
    SymArray we _
      | Just (Wx 0) <- widthConstant we -> do
         oc <- gets opCache
         return $ LE $ Apply (mkArrayOp oc 0 resType) []
    _  -> unexpected p "Empty-array comprehension" "empty-array type" resType
tcE (AST.MkRecord _ flds) = do
   flds' <- mapM tcLE [e | (_, _, e) <- flds]
   let names = [nm | (_, nm, _) <- flds]
   oc <- gets opCache
   let def = getStructuralRecord oc (Set.fromList names)
   let fldTps = map typeOfLogicExpr flds'
   let sub = emptySubst { shapeSubst = Map.fromList $ names `zip` fldTps }
   return $ LE $ Apply (mkOp (recDefCtor def) sub) flds'
tcE (AST.TypeExpr p e astResType) = do
   te <- tcLE e
   let tet = typeOfLogicExpr te
   resType <- tcT astResType
   if tet /= resType
      then mismatch p "type-annotation" tet resType
      else return (LE te)
tcE (AST.ApplyExpr p "valueOf" [jr]) = do
  sje <- tcJE jr
  unless (JSS.isRefType (jssTypeOfJavaExpr sje)) $ do
    let msg = "The Java value \'" ++ show sje ++ "\' does not denote a reference."
        res = "Only expressions refering to Java reference value may appear inside 'valueOf'."
     in typeErrWithR p (ftext msg) res
  mbToJavaT <- gets toJavaExprType
  case mbToJavaT of
    Nothing ->
      let msg = "The Java value \'" ++ show sje ++ "\' appears in a global context."
          res = "Java values may not be references outside method declarations."
       in typeErrWithR p (ftext msg) res
    Just toJavaT -> do
      case toJavaT sje of
        Nothing ->
          let msg = "The Java value \'" ++ show sje ++ "\' is missing a \'type\' annotation."
              res = "Please add a type declaration to Java values before "
                     ++ "referring to them in SAWScript expressions."
           in typeErrWithR p (ftext msg) res
        Just (ClassInstance _) ->
          let msg = "The expression " ++ show sje ++ " does not refer to an array,"
           in typeErrWithR p (ftext msg) ""
        Just (ArrayInstance l tp _) -> do
          let arrayTp = jssArrayDagType l tp
          return $ LE $ JavaValue sje arrayTp
tcE (AST.ApplyExpr appPos "join" astArgs) = do
  args <- mapM tcLE astArgs
  checkArgCount appPos "join" args 1
  let argType = typeOfLogicExpr (head args)
  case argType of
    SymArray (widthConstant -> Just l) (SymInt (widthConstant -> Just w)) -> do
         oc <- gets opCache
         return $ LE $ Apply (joinOp oc l w) args
    _ -> typeErr appPos $ ftext $ "Illegal arguments and result type given to \'join\'."
                                ++ " SAWScript currently requires that the argument is ground"
                                ++ " array of integers. "
tcE (AST.ApplyExpr pos nm astArgs) = do
  opBindings <- gets (opBindings . globalBindings)
  case Map.lookup nm opBindings of
    Nothing -> typeErrWithR pos (ftext ("Unknown operator '" ++ nm ++ "'.")) "Please check that the operator is correct."
    Just opDef -> do
      args <- mapM tcLE astArgs
      let defArgTypes = opDefArgTypes opDef
      checkArgCount pos nm args (V.length defArgTypes)
      let defTypes = V.toList defArgTypes
      let argTypes = map typeOfLogicExpr args
      case matchSubst (defTypes `zip` argTypes) of
        Nothing  -> do
          debugTI $ show defTypes
          debugTI $ show argTypes
          mismatchArgs pos ("in call to '" ++ nm ++ "'") argTypes defTypes
        Just sub -> do
          debugTI $ "Making expression with operator " ++ opDefName opDef ++ " and substitution " ++  show sub
          return $ LE $ Apply (mkOp opDef sub) args
tcE (AST.NotExpr      p l)   = lift1Bool     p "not" bNotOp        l
tcE (AST.BitComplExpr p l)   = lift1Word     p "~"   iNotOp        l
tcE (AST.NegExpr      p l)   = lift1Word     p "-"   negOp         l
tcE (AST.MulExpr      p l r) = lift2WordEq   p "*"   (const mulOp)         l r
tcE (AST.SDivExpr     p l r) = lift2WordEq   p "/s"  (const signedDivOp)   l r
tcE (AST.SRemExpr     p l r) = lift2WordEq   p "%s"  (const signedRemOp)   l r
tcE (AST.PlusExpr     p l r) = lift2WordEq   p "+"   (const addOp)         l r
tcE (AST.SubExpr      p l r) = lift2WordEq   p "-"   (const subOp)         l r
tcE (AST.ShlExpr      p l r) = lift2Word     p "<<"  shlOp         l r
tcE (AST.SShrExpr     p l r) = lift2Word     p ">>s" shrOp         l r
tcE (AST.UShrExpr     p l r) = lift2Word     p ">>u" ushrOp        l r
tcE (AST.BitAndExpr   p l r) = lift2WordEq   p "&"   (const iAndOp)        l r
tcE (AST.BitOrExpr    p l r) = lift2WordEq   p "|"   (const iOrOp)         l r
tcE (AST.BitXorExpr   p l r) = lift2WordEq   p "^"   (const iXorOp)        l r
tcE (AST.AppendExpr   p l r) = lift2Word     p "#"   appendIntOp   l r
tcE (AST.EqExpr       p l r) = LE <$> lift2ShapeCmp p "=="  eqOp          l r
tcE (AST.IneqExpr     p l r) = (\e -> LE (Apply bNotOp [e])) <$> lift2ShapeCmp p "!="  eqOp l r
tcE (AST.SGeqExpr     p l r) = (LE . flipBinOpArgs) <$> lift2WordCmp  p ">=s" signedLeqOp   l r
tcE (AST.SLeqExpr     p l r) = LE                   <$> lift2WordCmp  p "<=s" signedLeqOp   l r
tcE (AST.SGtExpr      p l r) = (LE . flipBinOpArgs) <$> lift2WordCmp  p ">s"  signedLtOp    l r
tcE (AST.SLtExpr      p l r) = LE                   <$> lift2WordCmp  p "<s"  signedLtOp    l r
tcE (AST.UGeqExpr     p l r) = (LE . flipBinOpArgs) <$> lift2WordCmp  p ">=u" unsignedLeqOp l r
tcE (AST.ULeqExpr     p l r) = LE                   <$> lift2WordCmp  p "<=u" unsignedLeqOp l r
tcE (AST.UGtExpr      p l r) = (LE . flipBinOpArgs) <$> lift2WordCmp  p ">u"  unsignedLtOp  l r
tcE (AST.ULtExpr      p l r) = LE                   <$> lift2WordCmp  p "<u"  unsignedLtOp  l r
tcE (AST.AndExpr      p l r) = lift2Bool     p "&&"  bAndOp        l r
tcE (AST.OrExpr       p l r) = lift2Bool     p "||"  bOrOp         l r
tcE (AST.IteExpr      p t l r) = do
        --TODO: See if this can be fixed to support reference expressions.
        [t', l', r'] <- mapM tcLE [t, l, r]
        let [tt, lt, rt] = map typeOfLogicExpr [t', l', r']
        unless (tt == SymBool) $
          mismatch p "test expression of if-then-else" tt SymBool
        unless (lt == rt) $
          mismatch p "branches of if-then-else expression" lt rt
        return $ LE $ Apply (iteOp lt) [t', l', r']
tcE (AST.DerefField p e fName) = do
   me <- tcE e
   case me of
     JE lhs -> do
       cb <- gets (codeBase . globalBindings)
       f <- liftIO $ findField cb p (jssTypeOfJavaExpr lhs) fName
       return $ JE $ InstanceField lhs f
     LE e' ->
       --TODO: Simplify this method and move code to VerInf.
       case typeOfLogicExpr e' of
         rt@(SymRec recDef recSubst) -> do
           let fops = recDefFieldOps recDef
           case V.find (\op -> opDefName op == fName) fops of
             Nothing -> unexpected p "record field selection" ("record containing field " ++ show fName) rt
             Just fop -> return $ LE $ Apply (mkOp fop recSubst) [e']
         rt  -> unexpected p "record field selection" ("record containing field " ++ show fName) rt
tcE (AST.ThisExpr pos) = do
  (method, cl) <- getMethodInfo
  when (JSS.methodIsStatic method) $
    typeErr pos (ftext "\'this\' is not defined on static methods.")
  return $ JE (This (JSS.className cl))
tcE (AST.ArgExpr pos i) = do
  (method, _) <- getMethodInfo
  let params = V.fromList (JSS.methodParameterTypes method)
  -- Check that arg index is valid.
  unless (0 <= i && i < V.length params) $
    typeErr pos (ftext "Invalid argument index for method.")
  return $ JE (Arg i (params V.! i))
tcE (AST.LocalExpr pos name) = do
  (method, _) <- getMethodInfo
  case JSS.lookupLocalVariableByName method name of
    Nothing -> typeErr pos (ftext $ "Local variable " ++ name ++ " not found")
    -- TODO: check that the type exists
    Just l -> return $ JE (Local name (JSS.localIdx l) (JSS.localType l))

lift1Bool :: Pos -> String -> Op -> AST.Expr -> SawTI MixedExpr
lift1Bool p nm o l = do
  l' <- tcLE l
  let lt = typeOfLogicExpr l'
  case lt of
    SymBool -> return $ LE (Apply o [l'])
    _       -> mismatch p ("argument to operator '" ++ nm ++ "'")  lt SymBool

lift1Word :: Pos -> String -> (WidthExpr -> Op) -> AST.Expr -> SawTI MixedExpr
lift1Word p nm opMaker l = do
  l' <- tcLE l
  let lt = typeOfLogicExpr l'
  case lt of
    SymInt wl -> return $ LE $ Apply (opMaker wl) [l']
    _         -> unexpected p ("Argument to operator '" ++ nm ++ "'") "word" lt

lift2Bool :: Pos -> String -> Op -> AST.Expr -> AST.Expr -> SawTI MixedExpr
lift2Bool p nm o l r = do
  l' <- tcLE l
  r' <- tcLE r
  let lt = typeOfLogicExpr l'
      rt = typeOfLogicExpr r'
  case (lt, rt) of
    (SymBool, SymBool) -> return $ LE $ Apply o [l', r']
    (SymBool, _      ) -> mismatch p ("second argument to operator '" ++ nm ++ "'") rt SymBool
    (_      , _      ) -> mismatch p ("first argument to operator '"  ++ nm ++ "'") lt SymBool

lift2Word :: Pos -> String -> (WidthExpr -> WidthExpr -> Op)
          -> AST.Expr -> AST.Expr -> SawTI MixedExpr
lift2Word = lift2WordGen False

lift2WordEq :: Pos -> String -> (WidthExpr -> WidthExpr -> Op)
            -> AST.Expr -> AST.Expr -> SawTI MixedExpr
lift2WordEq = lift2WordGen True

-- The bool argument says if the args should be of the same type
lift2WordGen :: Bool
             -> Pos
             -> String
             -> (WidthExpr -> WidthExpr -> Op)
             -> AST.Expr -> AST.Expr -> SawTI MixedExpr
lift2WordGen checkEq p nm opMaker l r = do
  l' <- tcLE l
  r' <- tcLE r
  let lt = typeOfLogicExpr l'
      rt = typeOfLogicExpr r'
  case (lt, rt) of
    (SymInt wl, SymInt wr) ->
      if not checkEq || wl == wr
        then return $ LE $ Apply (opMaker wl wr) [l', r']
        else mismatch p ("arguments to operator '" ++ nm ++ "'") lt rt
    (SymInt _,  _)         -> unexpected p ("Second argument to operator '" ++ nm ++ "'") "word" rt
    (_       ,  _)         -> unexpected p ("First argument to operator '"  ++ nm ++ "'") "word" lt

lift2ShapeCmp :: Pos -> String -> (DagType -> Op)
              -> AST.Expr -> AST.Expr -> SawTI LogicExpr
lift2ShapeCmp p nm opMaker l r = do
  l' <- tcLE l
  r' <- tcLE r
  let lt = typeOfLogicExpr l'
      rt = typeOfLogicExpr r'
  unless (lt == rt) $ do
    mismatch p ("arguments to operator '" ++ nm ++ "'") lt rt
  return $ Apply (opMaker lt) [l', r']

lift2WordCmp :: Pos -> String -> (WidthExpr -> Op) 
             -> AST.Expr -> AST.Expr -> SawTI LogicExpr
lift2WordCmp p nm opMaker l r = do
  l' <- tcLE l
  r' <- tcLE r
  let lt = typeOfLogicExpr l'
      rt = typeOfLogicExpr r'
  case (lt, rt) of
    (SymInt wl, SymInt wr) -> do
      unless (wl == wr) $
        mismatch p ("arguments to operator '" ++ nm ++ "'") lt rt
      return $ Apply (opMaker wl) [l', r']
    (SymInt _,  _)         -> unexpected p ("Second argument to operator '" ++ nm ++ "'") "word" rt
    (_       ,  _)         -> unexpected p ("First argument to operator '"  ++ nm ++ "'") "word" lt

-- Only warn if the constant is beyond range for both signed/unsigned versions
-- This is less precise than it can be, but less annoying too..
warnRanges :: Pos -> DagType -> Integer -> Int -> SawTI ()
warnRanges pos tp i w'
  | violatesBoth = typeWarn pos $  ftext ("Constant \"" ++ show i ++ " : " ++ ppType tp ++ "\" will be subject to modular reduction.")
                                $$ complain srange "a signed"    (if j >= 2^(w-1) then j - (2^w) else j)
                                $$ complain urange "an unsigned" j
  | True         = return ()
  where violatesBoth = not (inRange srange || inRange urange)
        w :: Integer
        w = fromIntegral w'
        j :: Integer
        j = i `mod` (2^w)
        srange, urange :: (Integer, Integer)
        srange = (-(2^(w-1)), (2^(w-1))-1)
        urange = (0, 2^w-1)
        inRange (a, b) = i >= a && i <= b
        complain (a, b) ctx i' =    ftext ("In " ++ ctx ++ " context, range will be: [" ++ show a ++ ", " ++ show b ++ "]")
                                 $$ ftext ("And the constant will assume the value " ++ show i')
