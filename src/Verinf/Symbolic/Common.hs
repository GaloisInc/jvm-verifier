{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -O0 #-}
-- Exports {{{1
module Verinf.Symbolic.Common (
  -- * BitWidth {{{2
    BitWidth(..)
  , maxBitIdx
  , numBits
  , bitWidthSize
  -- * Type functions {{{2
  -- ** Width expressions
  , WidthVar
  , WidthExpr, ppWidthExpr
  , constantWidth
  , varWidth
  , addWidth
  , widthConstant
  , widthVar
  -- ** DagType expressions
  , ShapeVar
  , SymTypeVar(..)
  , RecordId
  , SymRecDef(..)
  , recDefFieldNames
  , recDefFieldTypes
  , DagType(..)
  , ppType
  , defaultValue
  , TypeSubst(..)
  , emptySubst
  , idSubst
  , applyTypeSubst
  , composeTypeSubst
  , ppTypeSubst
  , matchSubst
  , recFieldTypes
  , recDefParams
  -- * OpDef {{{2
  , OpDef(..)
  , OpIndex(..)
  , OpPrec
  , defaultPrec
  , DagOpEval(..)
  , OpDefKind(..)
  , opDefArity
  , opDefParams
  , opDefFnType
  , opDefDefinition
  -- * Op {{{2
  , Op
  , opDef
  , opSubst
  , mkOp
  , opIndex
  , opName
  , opPrec
  , opArgTypes
  , opResultType
  , opFnType
  , opEval
  , opIsBottomUpInferable
  , opLitFn
  , applyTypeSubstToOp
  -- * DagTerm {{{2
  , DagTerm(..)
  , AppIndex
  , termAppIndex
  , evalDagTermFn
  , evalDagTerm
  , nodeCounts
  -- * DagApp {{{2
  , DagApp(..)
  , appOp
  , appArgs
  -- * Term Information and construction {{{2
  , TypedTerm(..)
  , InputIndex
  , TermClass(..)
  , TermSemantics(..)
  , tsBoolConstant
  , tsIntConstant
  , mkTermSemantics
  , applyTypeSubstToSemantics
  , PPConfig(..)
  , defaultPPConfig
  , PrettyTerm(..)
  -- * CValue {{{2
  , ConstantInjection(..)
  , ConstantProjection(..)
  , CValue(CBool, CArray, CRec)
  , LazyCValue
  , concreteEvalFn
  , PrettyNotation(..)
  , getCArray
  , ppCValueD
  , ppCValue
  , evalTermSemantics
  -- * CValue operations {{{2
  , cEq
  , cNot, cAnd, cOr, cXor, cImplies
  , cNegate, cAdd, cSub, cMul
  , cSignedQuot, cSignedRem, cUnsignedQuot, cUnsignedRem
  , cSignedLeq, cSignedLt, cUnsignedLeq, cUnsignedLt
  , cGetArrayValue, cSetArrayValue
  -- * Bitblasting {{{2
  , LitResult(..)
  , mapLitResult
  , flattenLitResult
  , litToLitResult
  , toLsbfV
  , toMsbfV
  , fromLsbfV
  , fromMsbfV
  , mkCIntFromLsbfV
  , boolLitResultFromCValue
  , lFromCValue
  , lEqLitResult
  , lIteLitResult
  , lMkLitResultForType
  , lMkInputLitResult
  , mkBitBlastTermSemantics
  -- }}}2
  ) where

-- Imports {{{1
import Control.Applicative ((<$>))
import Control.Exception (assert)
import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Monoid as Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Word
import Text.PrettyPrint.HughesPJ

import Verinf.Symbolic.Lit
import Verinf.Symbolic.Lit.Functional
import Verinf.Utils.CacheM

-- Bitwidth {{{1
newtype BitWidth = Wx Int deriving (Eq, Ord)

instance Show BitWidth where
  show (Wx i) = show i

instance Num BitWidth where
  (Wx u) + (Wx v) = Wx (u + v)
  (Wx u) * (Wx v) = Wx (u * v)
  (Wx u) - (Wx v) = assert (u >= v) $ Wx (u - v)
  negate (Wx 0) = Wx 0
  negate (Wx _) = error "internal: Bitwidth cannot be negated"
  abs = id
  signum _ = (Wx 1)
  fromInteger i | i >= 0 = Wx (fromInteger i)
                | otherwise = error "internal: Bitwidth cannot be negative"

maxBitIdx :: Num t => BitWidth -> t
maxBitIdx (Wx n) = fromIntegral $ n - 1

numBits :: Num t => BitWidth -> t
numBits (Wx n) = fromIntegral n

-- | Returns number of elements in bit vector with given size.
bitWidthSize :: BitWidth -> Integer
bitWidthSize (Wx w) = 2 ^ w

-- DagType {{{1

-- WidthExpr {{{2

type WidthVar = String

data WidthExpr = WExpr (Map WidthVar Int) BitWidth
  deriving (Eq, Ord, Show)

-- | Make constant width expression.
constantWidth :: BitWidth -> WidthExpr
constantWidth w = WExpr Map.empty w

-- | Make variable width expression.
varWidth :: WidthVar -> WidthExpr
varWidth v = WExpr (Map.singleton v 1) 0

-- | Add two width expressions.
addWidth :: WidthExpr -> WidthExpr -> WidthExpr
addWidth (WExpr mx ox) (WExpr my oy) = WExpr (Map.unionWith (+) mx my) (ox + oy)

-- | Returns constant for width expression if it is a constant.
widthConstant :: WidthExpr -> Maybe BitWidth
widthConstant (WExpr mx ox)
  | Map.null mx = Just ox
  | otherwise = Nothing

-- | Extracts variable out of width expression if it is a single variable
-- expression.
widthVar :: WidthExpr -> Maybe WidthVar
widthVar (WExpr (Map.toList -> [(v,1)]) 0) = Just v
widthVar _ = Nothing

-- | Returns width variables in expression.
widthVars :: WidthExpr -> Set WidthVar
widthVars (WExpr m _) = Map.keysSet m

-- | Pretty print DagType
ppWidthExpr :: WidthExpr -> ShowS
ppWidthExpr (WExpr m o) = showString $
  intercalate " + " (if o == 0 then mapVars else mapVars ++ [show o])
 where mapVars :: [String]
       mapVars = concat $ map (\(v,c) -> replicate c v) $ Map.toList m

-- | Apply a width substitution to a width expression.
applyWidthSubst :: WidthExpr -> Map WidthVar WidthExpr -> WidthExpr
applyWidthSubst (WExpr m o) s =
  let mapFn (v,n) =
        case Map.lookup v s of
          Nothing -> error $
            "internal: Could not find var " ++ show v ++ " in width substitution."
          Just (WExpr rm ro) -> WExpr (Map.map (n*) rm) ((Wx n) * ro)
   in foldl' addWidth
             (WExpr Map.empty o)
             (map mapFn (Map.toList m))

-- DagType {{{2

type ShapeVar = String

-- | A symbol type parameter.
data SymTypeVar
  = ShapeVar ShapeVar
  | WidthVar WidthVar
  deriving (Eq, Ord, Show)

type RecordId = Word32

-- | A record definition.
data SymRecDef
   = SymRecDef { recDefCtor :: OpDef
               , recDefFieldOps :: V.Vector OpDef }
  deriving (Eq, Ord, Show)

-- | Returns type of fields in a rec def.
recDefFieldNames :: SymRecDef -> V.Vector String
recDefFieldNames d = V.map opDefName (recDefFieldOps d)

-- | Returns type of fields in a rec def.
recDefFieldTypes :: SymRecDef -> V.Vector DagType
recDefFieldTypes d = opDefArgTypes (recDefCtor d)

-- | A type parsed from the SBV format.
data DagType
  = SymBool
  | SymInt !WidthExpr
  | SymArray !WidthExpr !DagType
  -- | @SymRec r typeParams@ denotes the record @r@ after it's type
  -- parameters have been instantiated with values in vector.
  | SymRec !SymRecDef !TypeSubst
  | SymShapeVar !ShapeVar
  deriving (Eq, Ord, Show)

-- | An assignment to shape and width variables.
data TypeSubst = TypeSubst {
    shapeSubst :: Map ShapeVar DagType
  , widthSubst :: Map WidthVar WidthExpr
  } deriving (Eq, Ord, Show)

-- | Returns vars in type.
typeVars :: DagType -> Set SymTypeVar
typeVars SymBool = Set.empty
typeVars (SymInt wexpr) = Set.map WidthVar (widthVars wexpr)
typeVars (SymArray _ tp) = typeVars tp
typeVars (SymRec _ sub) = Set.union svars (Set.map WidthVar wvars)
  where wvars = Set.unions (map widthVars (Map.elems (widthSubst sub)))
        svars = Set.unions (map typeVars (Map.elems (shapeSubst sub)))
typeVars (SymShapeVar v) = Set.singleton (ShapeVar v)

-- | Returns vars in vector.
vectorTypeVars :: V.Vector DagType -> Set SymTypeVar
vectorTypeVars = V.foldl' Set.union Set.empty . V.map typeVars

-- | Returns field parameters for rec definition.
recDefParams :: SymRecDef -> Set SymTypeVar
recDefParams = vectorTypeVars . recDefFieldTypes

-- | Returns field types in record definition after instantation.
recFieldTypes :: SymRecDef -> TypeSubst -> V.Vector DagType
recFieldTypes recDef recSubst =
  V.map (flip applyTypeSubst recSubst) (recDefFieldTypes recDef)

-- | Pretty print DagType
ppType ::  DagType -> String
ppType SymBool = "Bit"
ppType (SymInt w) = '[' : ppWidthExpr w "]"
ppType (SymArray l tp) = '[' : ppWidthExpr l (']' : ppType tp)
ppType (SymRec def sub) =
  let ppFields op = opDefName op ++ " = "
                      ++ ppType (applyTypeSubst (opDefResultType op) sub)
   in '{' : (intercalate "; " (V.toList (V.map ppFields (recDefFieldOps def)))
               ++ "}")
ppType (SymShapeVar v) = v

-- | Returns value associated with type.
defaultValue :: DagType -> CValue
defaultValue SymBool = CBool False
defaultValue (SymInt (widthConstant -> Just w)) = CInt w 0
defaultValue (SymInt _) =
  error $ "internal: Cannot create default value for non-constant widths"
defaultValue (SymArray (widthConstant -> Just (Wx len)) eltTp) =
  CArray $ V.replicate len $ defaultValue eltTp
defaultValue (SymArray _ _) =
  error $ "internal: Cannot create default value for arrays with variable widths."
--TODO: Fix this
defaultValue SymRec{} = error "internal: Cannot create array of records"
defaultValue (SymShapeVar _) =
  error $ "internal: Cannot create default value for non-ground types"

-- | Returns empty substitution.
emptySubst :: TypeSubst
emptySubst = TypeSubst Map.empty Map.empty

-- | Returns identity substitution for this.
idSubst :: Set SymTypeVar -> TypeSubst
idSubst vars =
  TypeSubst
  { shapeSubst =
      Map.fromList [ (v, SymShapeVar v) | ShapeVar v <- Set.toList vars]
  , widthSubst =
      Map.fromList [ (v, varWidth v) | WidthVar v <- Set.toList vars]
  }

-- | Apply a type substitution to a given type.
applyTypeSubst :: DagType -> TypeSubst -> DagType
applyTypeSubst SymBool _ = SymBool
applyTypeSubst (SymInt wexpr) subst =
  SymInt (applyWidthSubst wexpr (widthSubst subst))
applyTypeSubst (SymArray len tp) subst =
  SymArray (applyWidthSubst len (widthSubst subst))
           (applyTypeSubst tp subst)
applyTypeSubst (SymRec def params) subst =
  SymRec def (composeTypeSubst params subst)
applyTypeSubst (SymShapeVar v) subst =
  case Map.lookup v (shapeSubst subst) of
    Just t -> t
    Nothing -> error $
      "internal: Could not find variable " ++ show v
         ++ " in shape substitution."

-- | Compose two type substitutions together.
composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeTypeSubst s t =
  TypeSubst
    { shapeSubst = Map.map (flip applyTypeSubst t) (shapeSubst s)
    , widthSubst = Map.map (flip applyWidthSubst (widthSubst t)) (widthSubst s)
    }

-- | Given pairs of equations @(tp_i, gtp_i)@ with @i \in [1,k)@ returns a
-- substitution @s@ such that @tp_i s = gtp_i@.
matchSubst :: [(DagType, DagType)] -> Maybe TypeSubst
matchSubst tps = impl tps emptySubst
  where wsub :: WidthExpr ->  WidthExpr -> TypeSubst -> Maybe TypeSubst
        wsub me@(widthConstant -> Just _) we s
          | me == we = Just s
          | otherwise = Nothing
        wsub (widthVar -> Just mv) we s =
          case Map.lookup mv (widthSubst s) of
            Nothing ->
              Just s { widthSubst = Map.insert mv we (widthSubst s) }
            Just me | me == we -> Just s
                    | otherwise -> Nothing
        wsub _ _ _ = error "internal: matching against complex width expression"
        impl :: [(DagType, DagType)] -> TypeSubst -> Maybe TypeSubst
        impl [] s = Just s
        impl ((SymBool, SymBool):eqs) s = impl eqs s
        impl ((SymInt me, SymInt we):eqs) s = impl eqs =<< wsub me we s
        impl ((SymArray ml me, SymArray tl te):eqs) s =
          impl ((me,te):eqs) =<< wsub ml tl s
        impl ((SymRec md ms, SymRec td ts):eqs) s | md == td = do
          let eqFn :: Ord a => Map a b -> Map a c -> [(b,c)]
              eqFn mm tm = Map.elems $ Map.intersectionWith (,) mm tm
          ws <- foldM (flip $ uncurry wsub) s $ eqFn (widthSubst ms) (widthSubst ts)
          impl (eqFn (shapeSubst ms) (shapeSubst ts) ++ eqs) ws
        impl ((SymShapeVar mv, tp):eqs) s =
          case Map.lookup mv (shapeSubst s) of
            Nothing ->
              impl eqs s { shapeSubst = Map.insert mv tp (shapeSubst s) }
            Just mt | mt == tp -> impl eqs s
                    | otherwise -> Nothing
        impl _ _ = Nothing

ppTypeSubst :: TypeSubst -> String
ppTypeSubst (TypeSubst shapeMap widthMap) = '{' : (res ++ "}")
  where res = intercalate ", " [ v ++ '/' : p | (v,p) <- wl ++ sl ]
        wl = Map.toList $ Map.map ppType shapeMap
        sl = Map.toList $ Map.map (\t -> ppWidthExpr t "") widthMap

-- OpDef {{{1

type OpPrec = Int

-- | Default ooperator precdence
defaultPrec :: OpPrec
defaultPrec = 50

-- We use this so that we can support lazy operators (e.g., if-then-else, etc.)
type LazyCValue = IO CValue

data DagOpEval
  = UnaryOpEval   !(TypeSubst -> LazyCValue -> LazyCValue)
  | BinaryOpEval  !(TypeSubst -> LazyCValue -> LazyCValue -> LazyCValue)
  | TernaryOpEval !(TypeSubst -> LazyCValue -> LazyCValue -> LazyCValue ->
                                                                  LazyCValue)
  | VectorOpEval  !(TypeSubst -> V.Vector LazyCValue -> LazyCValue)
  | NoOpEval

instance Show DagOpEval where
  show (UnaryOpEval _)   = "(UnaryOpEval <fn>)"
  show (BinaryOpEval _)  = "(BinaryOpEval <fn>)"
  show (TernaryOpEval _) = "(TernaryOpEval <fn>)"
  show (VectorOpEval _)  = "(VectorOpEval <fn>)"
  show NoOpEval          = "NoOpEval"

-- | Type of an operator definition
data OpDefKind
   = PrimOp
   | UninterpOp
   | RecCtor [String]
   | DefinedOp DagTerm

data OpIndex
  -- Common operations
  = Eq
  | ITE
  | Trunc BitWidth
  | SignedExt BitWidth
  | UnsignedExt BitWidth

  -- Boolean operations
  | Not
  | And
  | Or
  | Xor
  | Implies

  -- Integer bitwise operations
  | INot
  | IAnd
  | IOr
  | IXor

  -- Integer shift operations
  | Shl
  | Shr
  | Ushr
  | AppendInt

  -- Integer arithmetic operations
  | Add
  | Mul
  | Neg
  | Sub
  | SignedDiv
  | SignedRem
  | UnsignedDiv
  | UnsignedRem

  -- Arithmetic comparison operations
  | SignedLeq
  | SignedLt
  | UnsignedLeq
  | UnsignedLt

  -- Array operations
  | GetArrayValue
  | SetArrayValue
  | Split BitWidth BitWidth
  | Join BitWidth BitWidth
  | MkArray Int

  -- Other
  | Dynamic !Int
    deriving (Show, Eq, Ord)

-- | Operator definition and associated types.
data OpDef
  = OpDef {
       opDefIndex        :: !OpIndex
     , opDefName         :: !String
     , opDefPrec         :: !OpPrec
     , opDefArgTypes     :: !(V.Vector DagType)
     , opDefResultType   :: !DagType
     , opDefKind         :: !OpDefKind
       -- | Evaluate operator on ground term.
     , opDefEval         :: !DagOpEval
       -- | Bitblast arguments
     , opDefLitFn  :: forall l.(?be :: BitEngine l, Eq l, SV.Storable l) =>
          TypeSubst -> V.Vector (LitResult l) -> IO (LitResult l)
     }

instance Eq OpDef where
  x == y = opDefIndex x == opDefIndex y

instance Ord OpDef where
  compare x y = compare (opDefIndex x) (opDefIndex y)

instance Show OpDef where
  show op = opDefName op

-- | Returns arity of operator definition.
opDefArity :: OpDef -> Int
opDefArity op = V.length (opDefArgTypes op)

-- | Returns parameters in op definition.
opDefParams :: OpDef -> Set SymTypeVar
opDefParams op =
  Set.union (vectorTypeVars (opDefArgTypes op))
            (typeVars (opDefResultType op))

-- | Returns type of function.
opDefFnType :: OpDef -> (V.Vector DagType, DagType)
opDefFnType op = (opDefArgTypes op, opDefResultType op)

-- | Returns definition of operator if this is a defined op.
opDefDefinition :: OpDef -> Maybe DagTerm 
opDefDefinition OpDef { opDefKind = DefinedOp t } = Just t
opDefDefinition _ = Nothing

-- Op {{{1

data Op = Op { opDef :: !OpDef , opSubst :: !TypeSubst }

instance Show Op where
  showsPrec _p op = showChar '"' . showString (opName op) . showChar '"'

instance Eq Op where
  x == y =    opDefIndex (opDef x) == opDefIndex (opDef y)
           && opSubst x == opSubst y

instance Ord Op where
  compare x y =
    Monoid.mappend (opDefIndex (opDef x) `compare` opDefIndex (opDef y))
                   (opSubst x `compare` opSubst y)

-- | Instantiate an operator definition to a specific instance.
mkOp :: OpDef -> TypeSubst -> Op
mkOp = Op

opIndex :: Op -> OpIndex
opIndex = opDefIndex . opDef

-- | Returns the name of the operator.
opName :: Op -> String
opName = opDefName . opDef

-- | Returns precedence of operator.
opPrec :: Op -> OpPrec
opPrec = opDefPrec . opDef

-- | Returns argument types for operator.
opArgTypes :: Op -> V.Vector DagType
opArgTypes op = V.map (flip applyTypeSubst (opSubst op))
                      (opDefArgTypes (opDef op))

-- | Returns result type for operator.
opResultType :: Op -> DagType
opResultType op = applyTypeSubst (opDefResultType (opDef op)) (opSubst op)

-- | Returns type of function.
opFnType :: Op -> (V.Vector DagType, DagType)
opFnType op = (opArgTypes op, opResultType op)

-- | Return evaluation function of operator.
opEval :: Op -> DagOpEval
opEval op = opDefEval (opDef op)

-- | Returns true if result type is a subset of argument types.
opIsBottomUpInferable :: Op -> Bool
opIsBottomUpInferable op =
  typeVars (opResultType op) `Set.isSubsetOf` vectorTypeVars (opArgTypes op)

applyTypeSubstToOp :: TypeSubst -> Op -> Op
applyTypeSubstToOp sub (Op o s) = Op o (composeTypeSubst s sub)

opLitFn  :: (?be :: BitEngine l, Eq l, SV.Storable l)
         => Op -> V.Vector (LitResult l) -> IO (LitResult l)
opLitFn op = opDefLitFn (opDef op) (opSubst op)

-- DagApp {{{1

-- | A function application
data DagApp
   = UnaryApp   !Op !DagTerm
   | BinaryApp  !Op !DagTerm !DagTerm
   | TernaryApp !Op !DagTerm !DagTerm !DagTerm
   | App !Op !(V.Vector DagTerm)
   deriving (Eq, Ord, Show)

-- | Returns operator used in application
appOp :: DagApp -> Op
appOp (UnaryApp op _) = op
appOp (BinaryApp op _ _) = op
appOp (TernaryApp op _ _ _) = op
appOp (App op _) = op

-- | Returns a vector of the application's arguments
appArgs :: DagApp -> V.Vector DagTerm
appArgs (UnaryApp _ t)          = V.singleton t
appArgs (BinaryApp _ t1 t2)     = V.fromListN 2 [t1, t2]
appArgs (TernaryApp _ t1 t2 t3) = V.fromListN 3 [t1, t2, t3]
appArgs (App _ v)               = v

-- DagTerm {{{1

type AppIndex = Word64

-- | A possible symbolic term which may be a fresh input variable, a function
-- application, a ground value, or an array of symbolic terms.
data DagTerm
  = ConstTerm CValue DagType
  | InputTerm !InputIndex !DagType
  | AppVar !AppIndex !DagApp
  deriving (Typeable)

instance Eq DagTerm where
  InputTerm i _ == InputTerm j _ = i == j
  AppVar i _ == AppVar j _ = i == j
  ConstTerm x _ == ConstTerm y _ = x == y
  _ == _ = False

instance Ord DagTerm where
  compare (InputTerm i _) (InputTerm j _) = i `compare` j
  compare (InputTerm _ _) _ = LT
  compare _ (InputTerm _ _) = GT
  compare (AppVar i _) (AppVar j _) = i `compare` j
  compare (AppVar _ _) _ = LT
  compare _ (AppVar _ _) = GT
  compare (ConstTerm x _) (ConstTerm y _) = x `compare` y

instance Show DagTerm where
  showsPrec d t = showParen (d > 10) $ showTerm t
    where showTerm (InputTerm i _) =
            showString "InputTerm " . showsPrec 11 i . showString " XXXX"
          showTerm (AppVar ti app)
            = showString "AppVar "
                . showsPrec 11 ti . showChar ' '
                . showsPrec 11 app
          showTerm (ConstTerm val _) =
            showString "ConstTerm " . showsPrec 11 val


instance TypedTerm DagTerm where
  termType (InputTerm _ tp) = tp
  termType (AppVar _ app) = opResultType (appOp app)
  termType (ConstTerm _ tp) = tp

instance TermClass DagTerm where
  termApp (AppVar _ app) = Just (appOp app, appArgs app)
  termApp _ = Nothing

  termInputId (InputTerm i _) = Just i
  termInputId _ = Nothing

-- | Returns node app index if any.
termAppIndex :: DagTerm -> Maybe AppIndex
termAppIndex (AppVar i _) = Just i
termAppIndex _ = Nothing

type InputEvaluation m r = InputIndex -> DagType -> m r

evalDagTermFn :: CacheM m 
              => InputEvaluation m r -> TermSemantics m r -> m (DagTerm -> m r)
evalDagTermFn inputFn ts = do
  cacheRef <- newCache
  let fn (InputTerm i tp) = inputFn i tp
      fn (ConstTerm c tp) = tsConstant ts c tp
      fn (AppVar _ app) = do
        mb <- lookupCache cacheRef app
        case mb of
          Just res -> return res
          Nothing -> do
            res <- case app of
                     UnaryApp   op vx    -> tsApplyUnary ts op (fn vx)
                     BinaryApp  op vx vy -> tsApplyBinary ts op (fn vx) (fn vy)
                     TernaryApp op vx vy vz ->
                       tsApplyTernary ts op (fn vx) (fn vy) (fn vz)
                     App op args -> tsApplyOp ts op (V.map fn args)
            updateCache cacheRef app res
            return res
  return fn

evalDagTerm :: CacheM m 
            => InputEvaluation m r -> TermSemantics m r -> DagTerm -> m r
evalDagTerm i ts t = (\fn -> fn t) =<< evalDagTermFn i ts

instance ConstantInjection DagTerm where
  mkCBool b = ConstTerm (CBool b) SymBool
  mkCInt w i = ConstTerm (mkCInt w i) (SymInt (constantWidth w))

instance ConstantProjection DagTerm where
  termConst (ConstTerm v _) = Just v
  termConst _ = Nothing
  getBool = getBool <=< termConst
  getSValW = getSValW <=< termConst
  getUValW = getUValW <=< termConst
  getSVal = getSVal <=< termConst
  getUVal = getUVal <=< termConst

-- | Given a node returns a map that maps each subnode to its size.
nodeCounts :: DagTerm -> Map DagTerm Int
nodeCounts node = execState (visit node) Map.empty
  where visit :: DagTerm -> State (Map DagTerm Int) ()
        visit n@(termApp -> Just (_op, args)) = do
          m <- get
          case Map.lookup n m of
            Nothing -> do
              put $ Map.insert n 1 m
              V.mapM_ visit args
            Just cnt -> do
              let newCnt = cnt + 1
              put $ newCnt `seq` Map.insert n newCnt m
        visit _ = return ()

-- Term pretty printing {{{1

instance PrettyTerm DagTerm where
  prettyTermWithD = ppSymTermSExpWith

type Visited  = Set Word64
type NodeInfo = Map Word64 (Int, Int)  -- (depth, #of occs)

ppSymTermSExpWith :: PPConfig -> DagTerm -> Doc
ppSymTermSExpWith cfg dag = doc
  where (_, doc) = ppN Set.empty dag
        cutoffDepth    = ppMinSharingDepth cfg
        nodeInfos      = collectNodeInfo dag Map.empty

        ppN :: Visited -> DagTerm -> (Visited, Doc)
        ppN sofar (AppVar i app)
         | maybe True (depth <=) cutoffDepth = (sofar,  line empty)
         | i `Set.member` sofar              = (sofar,  nid)
         | cnt > 1                           = (sofar', line nref)
         | True                              = (sofar', line empty)
         where op = appOp app
               si        = show i
               nid       = text $ 'n' : si
               nref      = braces $ text $ si
               line r
                 | RecCtor fieldNames <- opDefKind (opDef op) =
                    let ppField nm d = parens (text nm <+> d)
                     in call (text "mkRec") r (zipWith ppField fieldNames argDs)
                 | otherwise =
                    call (text (opName op)) r argDs
               call o r as = parens $ fsep (o <> r : as)
               Just (depth, cnt) = Map.lookup i nodeInfos
               (sofar', reverse -> argDs) =
                 V.foldl nextArg (i `Set.insert` sofar, []) (appArgs app)
               nextArg (s, ds) a = let (s', d) = ppN s a in (s', d : ds)
        ppN sofar (termInputId -> Just i) = (sofar, text $ '?' : show i)
        ppN sofar (termConst -> Just c)   = (sofar, ppCValueD Prefix c)
        ppN _     _                       = error "illegal term in Symbolic.PrettySExp.ppSymTermSExp.ppN"

-- | Collect the occurrence count and depth of nodes
collectNodeInfo :: DagTerm -> NodeInfo -> NodeInfo
collectNodeInfo (AppVar i (appArgs -> args)) ni
   = case Map.lookup i ni of
       Just _  -> Map.adjust bumpCnt i ni
       Nothing -> d `seq` Map.insert i (d, 1) ni'
   where 
         bumpCnt (depth, count) = let count' = count+1 in count' `seq` (depth, count')
         ni' = V.foldr collectNodeInfo ni args
         d   = 1 + V.maximum (V.cons 0 (V.map (depthOf ni') args))
collectNodeInfo _ ni = ni

-- | Compute the depth of a given node; note that this is a mere look-up from
-- the map, no computation is actually done
depthOf :: NodeInfo -> DagTerm -> Int
depthOf ni (AppVar i _)
  | Just (d, _) <- i `Map.lookup` ni = d
  | True                           = error $ "Cannot locate depth info for node: " ++ show i
depthOf _ _ = 0

-- TermSemantics {{{1

data TermSemantics m t = TermSemantics {
         tsConstant :: CValue -> DagType -> m t
       , tsApplyUnary :: Op -> m t -> m t
       , tsApplyBinary :: Op -> m t -> m t -> m t
       , tsApplyTernary :: Op -> m t -> m t -> m t -> m t
       , tsApplyOp :: Op -> V.Vector (m t) -> m t
       }

tsBoolConstant :: TermSemantics m t -> Bool -> m t
tsBoolConstant ts b = tsConstant ts (CBool b) SymBool

tsIntConstant :: TermSemantics m t -> BitWidth -> Integer -> m t
tsIntConstant ts w v = tsConstant ts (mkCInt w v) (SymInt (constantWidth w))

-- | Utility function for creating a term semantics instance from constant
-- evaluation and vector evaluation functions.
mkTermSemantics :: (CValue -> DagType -> m t)
                -> (Op -> V.Vector (m t) -> m t)
                -> TermSemantics m t
mkTermSemantics cfn opfn =
  TermSemantics {
      tsConstant     = cfn
    , tsApplyUnary   = \op x     -> opfn op (V.singleton x)
    , tsApplyBinary  = \op x y   -> opfn op (V.fromListN 2 [x, y])
    , tsApplyTernary = \op x y z -> opfn op (V.fromListN 3 [x, y, z])
    , tsApplyOp      = opfn
    }

applyTypeSubstToSemantics :: TypeSubst
                          -> TermSemantics m t -> TermSemantics m t
applyTypeSubstToSemantics sub ts =
  TermSemantics {
      tsConstant     = tsConstant ts
    , tsApplyUnary   = tsApplyUnary   ts . applyTypeSubstToOp sub
    , tsApplyBinary  = tsApplyBinary  ts . applyTypeSubstToOp sub
    , tsApplyTernary = tsApplyTernary ts . applyTypeSubstToOp sub
    , tsApplyOp      = tsApplyOp      ts . applyTypeSubstToOp sub
    }

-- Concrete Value representation {{{1

class ConstantInjection c where
  mkCBool :: Bool -> c
  -- | Returns value for integer by normalizing integer into twos-complement
  -- representation for given bitwidth.
  mkCInt :: BitWidth -> Integer -> c


-- | Class for types capable of storing array, boolean, and int values.  TODO:
-- Improve this class so that all uses of CValue can be eventually replaced with
-- Symbolic terms.
class ConstantProjection c where
  getSValW :: c -> Maybe (BitWidth,Integer)
  -- | Returns signed representation of constant int if this is a concrete
  -- integer.
  getSVal :: c -> Maybe Integer
  getSVal c = snd <$> getSValW c

  getUValW :: c -> Maybe (BitWidth,Integer)
  -- | Returns unsigned representation of constant int if this is a concrete
  -- integer.
  getUVal :: c -> Maybe Integer
  getUVal c = snd <$> getUValW c

  -- | Returns boolean from constant class if it is a concrete boolean.
  getBool :: c -> Maybe Bool
  -- | Returns constant from term if any.
  termConst :: c -> Maybe CValue
  -- | Returns true if a constant can be extracted.
  isConst :: c -> Bool
  isConst = isJust . termConst

-- | Concrete terms in symbolic evaluation.
data CValue
  = CBool Bool
  | CInt BitWidth Integer
  -- | Array where everything has the same type.
  | CArray (V.Vector CValue)
  | CRec SymRecDef TypeSubst (V.Vector CValue)
 deriving (Eq, Ord, Show)

instance ConstantInjection CValue where
  mkCBool b = CBool b
  mkCInt (Wx w) x =
   CInt (Wx w) $
     let l = bitWidthSize (Wx w) -- Get size of bitvector.
         x' = x .&. (l - 1) -- Get unsigned representation of x.
      in if x' `testBit` (w-1)
           then x' - l -- Subtract size of bitvector if msb of x' is negative.
           else x'

instance ConstantProjection CValue where
  getSValW (CInt w v) = Just (w,v)
  getSValW _ = Nothing
  getUValW (CInt w v) = Just (w, v .&. mask)
    where mask = bitWidthSize w - 1
  getUValW _ = Nothing
  getBool (CBool b) = Just b
  getBool _ = Nothing
  termConst c = Just c

-- | Get array representation.
getCArray :: CValue -> Maybe (V.Vector CValue)
getCArray (CArray x) = Just x
getCArray _ = Nothing

cEq :: CValue -> CValue -> CValue
cEq x y = CBool (x == y)

-- | Returns function for evaluating terms with a concrete semantics.
concreteEvalFn :: V.Vector CValue -> IO (DagTerm -> IO CValue)
concreteEvalFn values = evalDagTermFn inputFn evalTermSemantics
  where inputFn i _ = return (values V.! i)

-- Bitwise operations {{{2

-- | Perform negation on Booleans or bitwise on integers.
cNot :: CValue -> CValue
cNot (CBool b) = CBool (not b)
cNot (CInt w v) = mkCInt w (complement v)
cNot _ = error "internal: Illegal arguments to cNot"

cAnd :: CValue -> CValue -> CValue
cAnd (CBool x)   (CBool y)   = CBool (x && y)
cAnd (CInt wx x) (CInt wy y) | wx == wy = mkCInt wx (x .&. y)
cAnd _ _ = error "internal: Illegal arguments to cAnd"

cOr :: CValue -> CValue -> CValue
cOr (CBool x) (CBool y) = CBool (x || y)
cOr (CInt wx x) (CInt wy y) | wx == wy = mkCInt wx (x .|. y)
cOr _ _ = error "internal: Illegal arguments to cOr"

cXor :: CValue -> CValue -> CValue
cXor (CBool x) (CBool y) = CBool (x /= y)
cXor (CInt wx x) (CInt wy y) | wx == wy = mkCInt wx (x `xor` y)
cXor _ _ = error "internal: Illegal arguments to cXor"

cImplies :: CValue -> CValue -> CValue
cImplies (CBool x) (CBool y) = CBool (not x || y)
cImplies _ _ = error "internal: Illegal arguments to cXor"

-- Arithmetic operations {{{2

cNegate :: CValue -> CValue
cNegate (CInt w v) = mkCInt w (-v)
cNegate _ = error "internal: Illegal arguments to cNegate"

cAdd :: CValue -> CValue -> CValue
cAdd (CInt wx x) (CInt wy y) | wx == wy = mkCInt wx (x + y)
cAdd _ _ = error "internal: Illegal arguments to cAdd"

cSub :: CValue -> CValue -> CValue
cSub (CInt wx x) (CInt wy y) | wx == wy = mkCInt wx (x - y)
cSub _ _ = error "internal: Illegal arguments to cSub"

cMul :: CValue -> CValue -> CValue
cMul (CInt wx x) (CInt wy y) | wx == wy = mkCInt wx (x * y)
cMul _ _ = error "internal: Illegal arguments to cMul"

cSignedQuot :: CValue -> CValue -> CValue
cSignedQuot (CInt wx x) (CInt wy y) | wx == wy = mkCInt wx (x `quot` y)
cSignedQuot _ _ = error "internal: Illegal arguments to cSignedQuot"

cSignedRem :: CValue -> CValue -> CValue
cSignedRem (CInt wx x) (CInt wy y) | wx == wy = mkCInt wx (x `rem` y)
cSignedRem _ _ = error "internal: Illegal arguments to cSignedRem"

cUnsignedQuot :: CValue -> CValue -> CValue
cUnsignedQuot (CInt wx@(Wx w) x) (CInt wy y) | wx == wy = mkCInt wx (tr x `quot` tr y)
  where mask = (1 `shiftL` w) - 1
        tr v = mask .&. v
cUnsignedQuot _ _ = error "internal: Illegal arguments to cUnsignedQuot"

cUnsignedRem :: CValue -> CValue -> CValue
cUnsignedRem (CInt wx@(Wx w) x) (CInt wy y) | wx == wy = mkCInt wx (tr x `rem` tr y)
  where mask = (1 `shiftL` w) - 1
        tr v = mask .&. v
cUnsignedRem _ _ = error "internal: Illegal arguments to cUnsignedRem"

-- Relation operations {{{2

cSignedLeq :: CValue -> CValue -> CValue
cSignedLeq (CInt wx x) (CInt wy y) | wx == wy = CBool (x <= y)
cSignedLeq _ _ = error "internal: Illegal arguments to cSignedLeq"

cSignedLt :: CValue -> CValue -> CValue
cSignedLt (CInt wx x) (CInt wy y) | wx == wy = CBool (x < y)
cSignedLt _ _ = error "internal: Illegal arguments to cSignedLt"

cUnsignedLeq :: CValue -> CValue -> CValue
cUnsignedLeq (CInt wx@(Wx w) x) (CInt wy y) | wx == wy = CBool (tr x <= tr y)
  where mask = (1 `shiftL` w) - 1
        tr v = mask .&. v
cUnsignedLeq _ _ = error "internal: Illegal arguments to cUnsignedLeq"

cUnsignedLt :: CValue -> CValue -> CValue
cUnsignedLt (CInt wx@(Wx w) x) (CInt wy y) | wx == wy = CBool (tr x < tr y)
  where mask = (1 `shiftL` w) - 1
        tr v = mask .&. v
cUnsignedLt _ _ = error "internal: Illegal arguments to cUnsignedLt"

-- Array operations {{{2

cGetArrayValue :: CValue -> CValue -> CValue
cGetArrayValue (CArray a) (CInt (Wx w) i) | ui < toInteger (V.length a) = a V.! fromInteger ui
  where mask = (1 `shiftL` w) - 1
        ui = i .&. mask
cGetArrayValue _ _ = error "internal: Illegal arguments to cGetArrayValue"

cSetArrayValue :: CValue -> CValue -> CValue -> CValue
cSetArrayValue (CArray a) (CInt (Wx w) i) v
    | ui < toInteger (V.length a) = CArray (a V.// [(fromInteger ui,v)])
  where mask = (1 `shiftL` w) - 1
        ui = i .&. mask
cSetArrayValue a i v = error $ "internal: Illegal arguments to cSetArrayValue"
                         ++ "\n Array: " ++ show a
                         ++ "\n Index: " ++ show i
                         ++ "\n Value: " ++ show v

data PrettyNotation = Mixfix | Prefix

-- | Pretty print a constant value.
-- Examples:
--   True, False (for Booleans)
--   1:[32]  (for a 32-bit integer)
--   [1:[32], 2:[32], 3:[32], 4:[32]] (4 element array in mixfix notation).
--   (mkArray 1:[32] 2:[32] 3:[32] 4:[32]) (4 element array in prefix notation).
--   { x = 1:[32] ; y = 2:[32] ; z = 3:[32] } (3 element record in mixfix notation).
--   (mkRec (x 1:[32]) (y 2:[32]) (z 3:[32])) (3 element record in prefix notation).
ppCValueD :: PrettyNotation -> CValue -> Doc
ppCValueD _ (CBool b) = text (if b then "True" else "False")
ppCValueD _ (CInt (Wx w) i) = integer i <> text ":[" <> int w <> char ']'
ppCValueD f (CArray vs) =
    case f of
      Mixfix -> brackets $ fsep $ punctuate comma (V.toList args)
      Prefix -> parens $ text "mkArray" <+> fsep (V.toList args)
  where args = V.map (ppCValueD f) vs
ppCValueD f (CRec def _ vs) =
  case f of
    Mixfix ->
     let ppField fieldOp d = text (opDefName fieldOp) <+> equals <+> d
      in braces $
           fsep $ punctuate semi
                $ V.toList
                $ V.zipWith ppField (recDefFieldOps def) args
    Prefix ->
     let ppField fieldOp d = parens (text (opDefName fieldOp) <+> d)
      in parens $ text "mkRec" <+>
                  (fsep $ V.toList
                        $ V.zipWith ppField (recDefFieldOps def) args)
  where args = V.map (ppCValueD f) vs

ppCValue :: PrettyNotation -> CValue -> ShowS
ppCValue n c = (render (ppCValueD n c) ++)

-- Evaluation {{{2

evalTermSemantics :: TermSemantics IO CValue
evalTermSemantics =
  TermSemantics {
      tsConstant  = \c _ -> return c
    , tsApplyUnary
    , tsApplyBinary
    , tsApplyTernary
    , tsApplyOp
    }
 where tsApplyUnary op@(opSubst -> sub) x =
         case opDefEval (opDef op) of
           UnaryOpEval fn -> fn sub x
           VectorOpEval fn -> fn sub (V.singleton x)
           NoOpEval -> error "Cannot evaluate uninterpreted op"
           _ -> error "internal: Bad op given to tsApplyUnary"
       tsApplyBinary op@(opSubst -> sub) x y =
         case opDefEval (opDef op) of
           BinaryOpEval fn -> fn sub x y
           VectorOpEval fn -> fn sub (V.fromListN 2 [x, y])
           NoOpEval -> error "Cannot evaluate uninterpreted op"
           _ -> error "internal: Bad op given to tsApplyBinary"
       tsApplyTernary op@(opSubst -> sub) x y z =
         case opDefEval (opDef op) of
           TernaryOpEval fn -> fn sub x y z
           VectorOpEval fn -> fn sub (V.fromListN 3 [x, y, z])
           NoOpEval -> error "Cannot evaluate uninterpreted op"
           _ -> error "internal: Bad op given to tsApplyTernary"
       tsApplyOp op@(opSubst -> sub) args =
         case opDefEval (opDef op) of
           UnaryOpEval fn -> fn sub (args V.! 0)
           BinaryOpEval fn -> fn sub (args V.! 0) (args V.! 1) 
           TernaryOpEval fn -> fn sub (args V.! 0) (args V.! 1) (args V.! 2)
           VectorOpEval fn -> fn sub args
           NoOpEval -> error "Cannot evaluate uninterpreted op"

-- BitBlast Representation {{{1

-- | An literal vector.
data LitResult l
   = LV !(SV.Vector l)
   | LVN !(V.Vector (LitResult l))
  deriving (Eq)

-- Conversion {{{2

-- | Map between lit results.
mapLitResult :: (SV.Storable x, SV.Storable y) 
             => (x -> y) -> LitResult x -> LitResult y
mapLitResult fn (LV v) = LV (SV.map fn v) 
mapLitResult fn (LVN v) = LVN (V.map (mapLitResult fn) v)

-- | Flatten lit result into single vector.
flattenLitResult :: (SV.Storable l) => LitResult l -> SV.Vector l
flattenLitResult (LV v) = v
flattenLitResult (LVN v) = SV.concatMap (flattenLitResult . (v V.!))
                                        (SV.enumFromN 0 (V.length v)) 

-- | Returns lit vector from lit.
litToLitResult :: SV.Storable l => l -> LitResult l
litToLitResult = LV . SV.singleton

-- | Literals in an int with least-significant bit first
toLsbfV :: LitResult l -> SV.Vector l
toLsbfV (LV x) = x
toLsbfV (LVN _) = error "toLsbfV applied to nested litvector"

-- | Literals in an int with least-significant bit first
toMsbfV :: SV.Storable l => LitResult l -> SV.Vector l
toMsbfV (LV x) = SV.reverse x
toMsbfV (LVN _) = error "toMsbfV applied to nested litvector"

-- | LitResult with literals ordered with least-significant bit first
fromLsbfV :: SV.Vector l -> LitResult l
fromLsbfV = LV

-- | LitResult with literals ordered with least-significant bit first
fromMsbfV :: SV.Storable l => SV.Vector l -> LitResult l
fromMsbfV = LV . SV.reverse

-- | Create a CInt from a list of bits with least-significant bit from.
mkCIntFromLsbfV :: SV.Vector Bool -> CValue
mkCIntFromLsbfV ilits =
  let n = SV.length ilits
   in mkCInt (Wx n) $
        V.foldl' (\i k -> if ilits SV.! k then setBit i k else i)
                 0
                 (V.enumFromN 0 n)

-- PrettyTerm {{{2

instance (SV.Storable a, PrettyTerm a) => PrettyTerm (LitResult a) where
  prettyTerm (LV v) = concatMap prettyTerm (SV.toList v)
  prettyTerm (LVN ls) = intercalate "; " $ map prettyTerm (V.toList ls)
  prettyTermWithD _ = text . prettyTerm

-- Bitblasting {{{2

-- | Returns a bitvector representation of c.
boolLitResultFromCValue :: CValue -> LitResult Bool
boolLitResultFromCValue (CArray a)      = LVN (V.map boolLitResultFromCValue a)
boolLitResultFromCValue (CBool b)       = LV (SV.singleton b)
boolLitResultFromCValue (CInt (Wx w) x) = LV (SV.generate w (testBit x))
boolLitResultFromCValue (CRec _ _ x)    = LVN (V.map boolLitResultFromCValue x)

-- | @lFromCValue c@ returns a bitvector representation of c.
lFromCValue :: (?be :: BitEngine l, SV.Storable l) => CValue -> LitResult l
lFromCValue cv = mapLitResult lFromBool (boolLitResultFromCValue cv)

-- | @lIteLitResult c lTrue lFalse @ returns the lit vector equal to lTrue if c
-- is true and lFalse otherwise.
lIteLitResult :: (?be :: BitEngine l, SV.Storable l)
              => l -> LitResult l -> LitResult l -> LitResult l
lIteLitResult = lLazyMux mergeFn
  where mergeFn c (LV vx) (LV vy) =
          assert (SV.length vx == SV.length vy) $ LV (SV.zipWith (lIte c) vx vy)
        mergeFn c (LVN vx) (LVN vy) =
          assert (V.length vx == V.length vy) $ LVN (V.zipWith (mergeFn c) vx vy)
        mergeFn _ _ _ = error "internal: illegal arguments to lIteLitResult"

lEqLitResult :: (?be :: BitEngine l, SV.Storable l)
             => LitResult l -> LitResult l -> l
lEqLitResult (LV vx) (LV vy) = vx `lEqVector` vy
lEqLitResult (LVN vx) (LVN vy) = 
  assert (V.length vx == V.length vy) $
    V.foldl' lAnd lTrue (V.zipWith lEqLitResult vx vy)
lEqLitResult _ _ = error "internal: bad arguments to lEqLitResult"

-- | Create a lit result with bits from the generator with the given ground dag type.
lMkLitResultForType :: (SV.Storable l, Monad m)
                    => m l -> DagType -> m (LitResult l)
lMkLitResultForType gen SymBool = return . LV . SV.singleton =<< gen
lMkLitResultForType gen (SymInt (widthConstant -> Just (Wx w))) =
  return . LV =<< SV.replicateM w gen
lMkLitResultForType gen (SymArray (widthConstant -> Just (Wx l)) eltTp) =
  return . LVN =<< V.replicateM l (lMkLitResultForType gen eltTp)
lMkLitResultForType gen (SymRec d sub) =
  return . LVN =<< V.mapM (lMkLitResultForType gen) (recFieldTypes d sub)
lMkLitResultForType _ _ =
  error "internal: lMkLitResultForType called with non-ground type."

-- | Create a lit result with fresh input bits for the given ground dag type.
lMkInputLitResult :: (?be :: BitEngine l, SV.Storable l)
                  => DagType -> IO (LitResult l)
lMkInputLitResult = lMkLitResultForType lMkInput

mkBitBlastTermSemantics :: (Eq l, SV.Storable l)
                        => BitEngine l
                        -> TermSemantics IO (LitResult l)
mkBitBlastTermSemantics be =
  let cfn c _ = return (lFromCValue c)
      opfn op args = opDefLitFn (opDef op) (opSubst op) =<< V.sequence args
   in mkTermSemantics cfn opfn
 where ?be = be

-- TypedTerm {{{1

class TypedTerm t where
  -- | Returns type of term.
  termType :: t -> DagType

-- TermClass {{{1

type InputIndex = Int

class (Eq t, Ord t, ConstantProjection t, TypedTerm t) => TermClass t where
  -- | Returns term input index if this is an input node.
  termInputId :: t -> Maybe InputIndex
  -- | Returns true if this is a term input node.
  isInputTerm :: t-> Bool
  isInputTerm = isJust . termInputId
  -- | Returns App from term if any.
  termApp :: t -> Maybe (Op, V.Vector t)
  -- | Returns true if term is an application.
  isApp :: t -> Bool
  isApp = isJust . termApp
  -- | Returns operator definition, type substitution, and subterms for term.
  termTypedApp  :: t -> Maybe (OpDef, TypeSubst, V.Vector t)
  termTypedApp t = (\(op,args) -> (opDef op, opSubst op, args)) <$> termApp t

-- Pretty printing {{{1

data PPConfig = PPConfig {
          ppShowConstTypes  :: !Bool         -- whether we should display the types of constant literals
        , ppMinSharingDepth :: !(Maybe Int)  -- anything less than this depth will always be shown, 0 will give maximum sharing. Nothing: show the full tree.
        , ppLineLength      :: !Int          -- line length for the printed page
        }

defaultPPConfig :: PPConfig
defaultPPConfig = PPConfig {
          ppShowConstTypes  = False
        , ppMinSharingDepth = Just 0
        , ppLineLength      = 80
        }

class PrettyTerm a where
  prettyTermD      :: a -> Doc
  prettyTermWithD  :: PPConfig -> a -> Doc
  prettyTerm       :: a -> String
  prettyTermWith   :: PPConfig -> a -> String

  -- minimal completion: prettyTermWithD
  prettyTermD      = prettyTermWithD defaultPPConfig
  prettyTerm       = renderStyle style { lineLength = ppLineLength defaultPPConfig }
                   . prettyTermD
  prettyTermWith c = renderStyle style { lineLength = ppLineLength c }
                   . prettyTermWithD c

