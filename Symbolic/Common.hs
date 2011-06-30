{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix
-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module Symbolic.Common (
  -- * BitWidth
    BitWidth(..)
  , maxBitIdx
  , numBits
  , bitWidthSize
  -- * Type functions
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
  , recDefFieldTypes
  , DagType(..)
  , ppType
  , defaultValue
  , TypeSubst(..)
  , emptySubst
  , idSubst
  , applyTypeSubst
  , composeTypeSubst
  , matchSubst
  , recFieldTypes
  , recDefParams
  -- * CValue
  , ConstantInjection(..)
  , ConstantProjection(..)
  , CValue(..)
  , fromCArray
  , fromCBool
  , PrettyNotation(..)
  , ppCValueD
  , ppCValue
  -- * LitResult
  , LitResult(..)
  , litToLitResult
  , toLsbfV
  , toMsbfV
  , fromLsbfV
  , fromMsbfV
  , mkCIntFromLsbfV
  , concreteValueLit
  -- * TermClass
  , InputIndex
  , TypedTerm(..)
  , TermClass(..)
  -- * ApplyOpMonad
  , MonadTerm
  , ApplyOpMonad(..)
  -- * OpDef
  , OpPrec
  , defaultPrec
  , DagOpEval(..)
  , OpDefKind(..)
  , OpDef(..)
  , opArity
  , opDefParams
  -- * Op
  , Op
  , opDef
  , opSubst
  , mkOp
  , groundOp
  , opName
  , opPrec
  , opArgTypes
  , opResultType
  , opEval
  , opIsBottomUpInferable
  -- * OpSession
  , OpId(..)
  , OpSession
  , runOpSession
  , cachedOpDef
  , definedOp
  , uninterpretedOp
  , getStructuralRecord
  , listStructuralRecords
  -- * BitBlastMonad
  , BitBlastMonad(..)
  -- * WordMonad
  , WordMonad(..)
  -- * AigOps
  , AigOps(..)
  , SupportsSession(..)
  -- * PrettyPrinting
  , PPConfig(..), defaultPPConfig
  , PrettyTerm(..)
  -- * Misc
  , OpIndex
  , boolFromConst
  , boolSeqToHex
  , boolSeqToInt32
  , boolSeqToInt64
  , byteSeqToHex
  , constBool
  , constInt
  , constLong
  , evalAigArgs8
  , evalAigArgs32
  , evalAigArgs64
  , hexToByteSeq
  , hexToIntSeq
  , hexToLongSeq
  , intFromConst
  , intSeqToBoolSeq
  , intSeqToHex
  , intToBoolSeq
  , longFromConst
  , outsToInts8
  , outsToInts32
  , outsToInts64
  ) where

-- Imports {{{1
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Control.Monad.State.Lazy as MTLL
import qualified Control.Monad.State.Strict as MTLS
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Word
import Numeric
import Symbolic.Lit
import Text.PrettyPrint.HughesPJ

import IO.Session
import Utils.IOStateT
import Utils.LogMonad

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

type TypeVector = V.Vector DagType

type RecordId = Word32

-- | A record definition.
data SymRecDef
   = SymRecDef { recDefCtor :: OpDef
               , recDefFieldOps :: V.Vector OpDef }
  deriving (Eq, Ord, Show)

-- | Returns type of fields in a rec def.
recDefFieldTypes :: SymRecDef -> V.Vector DagType
recDefFieldTypes = opDefArgTypes . recDefCtor

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
recFieldTypes :: SymRecDef -> TypeSubst -> TypeVector
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
   in '{' : (intercalate "; " (V.toList (V.map ppFields (recDefFieldOps def))) ++ "}")
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
      "internal: Could not find variable " ++ show v ++ " in shape substitution."

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


-- Concrete Value representation {{{1

class ConstantInjection c where
  -- | Returns value for Boolean
  mkCBool :: Bool -> c
  -- | Returns value for integer by normalizing integer into twos-complement
  -- representation for given bitwidth.
  mkCInt :: BitWidth -> Integer -> c

-- | Class for types capable of storing array, boolean, and int values.  TODO:
-- Improve this class so that all uses of CValue can be eventually replaced with
-- Symbolic terms.
class ConstantProjection c where
  -- | Returns signed representation of constant int if this is a concrete
  -- integer.
  getSVal :: c -> Maybe Integer
  -- | Returns unsigned representation of constant int if this is a concrete
  -- integer.
  getUVal :: c -> Maybe Integer
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

fromCArray :: CValue -> V.Vector CValue
fromCArray (CArray x) = x
fromCArray _ = error "internal: CValue not constructed via CArray"

{-# DEPRECATED fromCBool "Use getBool instead" #-}
fromCBool :: CValue -> Bool
fromCBool (CBool x) = x
fromCBool _  = error "internal: CValue not constructed via CBool"

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
  getSVal (CInt _ v) = Just v
  getSVal _ = Nothing
  getUVal (CInt w x) = Just $ (x + w') .&. (w' - 1)
    where w' = bitWidthSize w
  getUVal _ = Nothing
  getBool (CBool b) = Just b
  getBool _ = Nothing
  termConst c = Just c

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

-- LitResult {{{1

-- | An literal vector.
data LitResult l
   = LV !(SV.Vector l) -- Invariant: This vector should be of length one;
                       -- otherwise, it's an error.
   | LVN !(V.Vector (LitResult l))
  deriving (Eq)

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

-- | @concreteValueLit c@ returns a bitvector representation of c.
concreteValueLit :: CValue -> AigComputation OpSession (LitResult Lit)
concreteValueLit c = case c of
  CArray a -> fmap LVN $ V.mapM concreteValueLit a
  CBool b -> fmap litToLitResult $ litFromBool b
  CInt (Wx n) x  -> do
    fmap LV $ SV.mapM (litFromBool . testBit x)
            $ SV.enumFromN 0 n
  CRec _ _ x -> fmap LVN $ V.mapM concreteValueLit x

-- TermClass {{{1

type InputIndex = Int

class TypedTerm t where
  -- | Returns type of term.
  termType :: t -> DagType

class (Eq t, Ord t, ConstantProjection t, TypedTerm t) => TermClass t where
  -- | Returns term input index if this is an input node.
  termInputId :: t -> Maybe InputIndex
  -- | Returns true if this is a term input node.
  isInputTerm :: t-> Bool
  isInputTerm = isJust . termInputId
  -- | Returns App from term if any.
  getAppVector :: t -> Maybe (Op, V.Vector t)
  -- | Returns operator definition, type substitution, and subterms for term.
  getTypedApp  :: t -> Maybe (OpDef, TypeSubst, V.Vector t)
  -- | Returns true if term is an application.
  isApp :: t -> Bool
  isApp = isJust . getAppVector

-- ApplyOpMonad {{{1

-- | Returns term type associated with monad.
type family MonadTerm (m :: * -> *)

-- | Operations on a term dag representation.
class (Functor m, Monad m) => ApplyOpMonad m where
  -- | Make a term from a constant.
  makeConstant :: CValue -> DagType -> m (MonadTerm m)
  -- | Apply a unary operator to a term.
  applyUnaryOp :: Op -> MonadTerm m -> m (MonadTerm m)
  -- | Apply a binary operator to a term.
  applyBinaryOp :: Op -> MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Apply ternary operator to terms.
  applyTernaryOp :: Op -> MonadTerm m -> MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Apply operator to vector of terms.
  applyOpVector :: Op -> V.Vector (MonadTerm m) -> m (MonadTerm m)

  -- | Apply operator to list of terms.
  applyOp :: Op -> [MonadTerm m] -> m (MonadTerm m)
  applyOp op [x] = applyUnaryOp op x
  applyOp op [x, y] = applyBinaryOp op x y
  applyOp op [x, y, z] = applyTernaryOp op x y z
  applyOp op args = applyOpVector op (V.fromListN (length args) args)

-- State Transformers {{{2

type instance MonadTerm (StateT s m) = MonadTerm m
instance ApplyOpMonad m => ApplyOpMonad (StateT s m) where
  makeConstant c tp       = lift $ makeConstant c tp
  applyUnaryOp op x       = lift $ applyUnaryOp op x
  applyBinaryOp op x y    = lift $ applyBinaryOp op x y
  applyTernaryOp op x y z = lift $ applyTernaryOp op x y z
  applyOpVector op args   = lift $ applyOpVector op args

type instance MonadTerm (MTLL.StateT s m) = MonadTerm m
instance ApplyOpMonad m => ApplyOpMonad (MTLL.StateT s m) where
  makeConstant c tp       = lift $ makeConstant c tp
  applyUnaryOp op x       = lift $ applyUnaryOp op x
  applyBinaryOp op x y    = lift $ applyBinaryOp op x y
  applyTernaryOp op x y z = lift $ applyTernaryOp op x y z
  applyOpVector op args   = lift $ applyOpVector op args

type instance MonadTerm (MTLS.StateT s m) = MonadTerm m
instance ApplyOpMonad m => ApplyOpMonad (MTLS.StateT s m) where
  makeConstant c tp       = lift $ makeConstant c tp
  applyUnaryOp op x       = lift $ applyUnaryOp op x
  applyBinaryOp op x y    = lift $ applyBinaryOp op x y
  applyTernaryOp op x y z = lift $ applyTernaryOp op x y z
  applyOpVector op args   = lift $ applyOpVector op args

-- OpDef {{{1

type OpPrec = Int

-- | Default ooperator precdence
defaultPrec :: OpPrec
defaultPrec = 50

data DagOpEval
  = UnaryOpEval !(TypeSubst -> CValue -> CValue)
  | BinaryOpEval !(TypeSubst -> CValue -> CValue -> CValue)
  | TernaryOpEval !(TypeSubst -> CValue -> CValue -> CValue -> CValue)
  | VectorOpEval !(TypeSubst -> V.Vector CValue -> CValue)
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
  | DefinedOp (TypeSubst
                 -> forall sym . WordMonad sym
                      => V.Vector (MonadTerm sym)
                      -> sym (MonadTerm sym))

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
       -- | Generate an AIGER from a literal vector.
     , opDefDescriptor   :: TypeSubst
                              -> forall sym . AigOps sym
                                   => V.Vector (MonadTerm sym)
                                   -> sym (LitResult Lit)
     }

instance Eq OpDef where
  x == y = opDefIndex x == opDefIndex y

instance Ord OpDef where
  compare x y = compare (opDefIndex x) (opDefIndex y)

instance Show OpDef where
  show op = opDefName op

-- | Returns arity of operator definition.
opArity :: OpDef -> Int
opArity op = V.length (opDefArgTypes op)

-- | Returns parameters in op definition.
opDefParams :: OpDef -> Set SymTypeVar
opDefParams op =
  Set.union (vectorTypeVars (opDefArgTypes op))
            (typeVars (opDefResultType op))

-- Op {{{

data Op = Op { opDef :: !OpDef , opSubst :: !TypeSubst }

instance Show Op where
  showsPrec _p op = showChar '"' . showString (opName op) . showChar '"'

instance Eq Op where
  x == y =    opDefIndex (opDef x) == opDefIndex (opDef y)
           && opSubst x == opSubst y

instance Ord Op where
  compare x y =
    mappend (opDefIndex (opDef x) `compare` opDefIndex (opDef y))
            (opSubst x `compare` opSubst y)

-- | Instantiate an operator definition to a specific instance.
mkOp :: OpDef -> TypeSubst -> Op
mkOp = Op

-- | Makes operator from ground op definition.
groundOp :: OpDef -> Op
groundOp opDef = mkOp opDef emptySubst

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

-- | Return evaluation function of operator.
opEval :: Op -> DagOpEval
opEval op = opDefEval (opDef op)

-- | Returns true if result type is a subset of argument types.
opIsBottomUpInferable :: Op -> Bool
opIsBottomUpInferable op =
  typeVars (opResultType op) `Set.isSubsetOf` vectorTypeVars (opArgTypes op)

-- OpSession {{{1

-- | Identifier for op.
data OpId
  -- | @TruncOpId outputWidth@
  = TruncOpId BitWidth
  -- | @SignedExtOpId outputWidth@
  | SignedExtOpId BitWidth
  -- | @SplitOpId arrayLength elementWidth@
  | SplitOpId BitWidth BitWidth
  -- | @JoinOpId arrayLength elementWidth@
  | JoinOpId BitWidth BitWidth
  -- | @MkArrayOpId arrayLength@
  | MkArrayOpId Int
  deriving (Eq, Ord)

data OpState = OpState {
    nextOpDefIndex :: !OpIndex
  , opIdMap :: !(Map OpId OpDef)
  , structuralRecordMap :: !(Map (Set String) SymRecDef)
  }

newtype OpSession a = OS (StateT OpState IO a)
  deriving ( Functor
           , CatchMIO
           , Monad
           , MonadIO
           , SessionMonad)

instance SessionState OpState where
  initialState = OpState
    { nextOpDefIndex = 70
    , opIdMap  = Map.empty
    , structuralRecordMap = Map.empty
    }

-- | Runs operator session.
runOpSession :: OpSession a -> IO a
runOpSession (OS m) = evalStateT m initialState

-- | Function used for creating a cached operator.
cachedOpDef :: OpId -> (OpIndex -> OpDef) -> OpSession OpDef
cachedOpDef opId opFn = do
  s <- OS get
  let idx = nextOpDefIndex s
  case Map.lookup opId (opIdMap s) of
    Just op -> return op
    Nothing -> do
      let opDef = opFn idx
      seq opDef $ do
        OS $ put s { nextOpDefIndex = idx + 1
                   , opIdMap = Map.insert opId opDef (opIdMap s)
                   }
        return opDef

-- | Create a defined operator with given name.
definedOp :: String
          -> [DagType]
          -> DagType
          -> (TypeSubst ->
                (forall sym . WordMonad sym
                   => V.Vector (MonadTerm sym)
                   -> sym (MonadTerm sym)))
          -> OpSession OpDef
definedOp opDefName argTypes opDefResultType fn = do
  s <- OS get
  let opDefIndex = nextOpDefIndex s
  OS $ put s { nextOpDefIndex = opDefIndex + 1 }
  return OpDef { opDefIndex
               , opDefName
               , opDefPrec = defaultPrec
               , opDefArgTypes = V.fromList argTypes
               , opDefResultType
               , opDefKind = DefinedOp fn
               , opDefEval = NoOpEval
               , opDefDescriptor = \subst -> fn subst >=> getVarLit
               }

-- | Create uninterpreted operator with given name and index.
uninterpretedOp :: String
                -> V.Vector DagType
                -> DagType
                -> OpSession OpDef
uninterpretedOp opDefName opDefArgTypes opDefResultType = do
  s <- OS get
  let idx = nextOpDefIndex s
  OS $ put s { nextOpDefIndex = idx + 1 }
  return OpDef { opDefIndex = idx
               , opDefName
               , opDefPrec = defaultPrec
               , opDefArgTypes
               , opDefResultType
               , opDefKind = UninterpOp
               , opDefEval = NoOpEval
               , opDefDescriptor = \_ _ -> do
                   fail $ "Cannot bitblast uninterpreted op " ++ opDefName
               }

-- | Returns a record definition that is unique for a given set of
-- field names.  The type of each field is shape variable with the same
-- name as the field name.
getStructuralRecord :: Set String -> OpSession SymRecDef
getStructuralRecord names = do
  rm <- OS $ gets structuralRecordMap
  case Map.lookup names rm of
    Just def -> return def
    Nothing -> do -- Create new record
      let namesList = Set.toList names
      let namesVector = V.fromList namesList
      let ctorName = "{ " ++ intercalate ", " namesList ++ " }"
      idx <- OS $ gets nextOpDefIndex
      let fieldCount = V.length namesVector
      let fieldTypes = V.map SymShapeVar namesVector
      let ctorOp = OpDef { opDefIndex = idx
                         , opDefName = ctorName
                         , opDefPrec = defaultPrec
                         , opDefArgTypes = fieldTypes
                         , opDefResultType = recType
                         , opDefKind = RecCtor namesList
                         , opDefEval = VectorOpEval (CRec recDef)
                         , opDefDescriptor = \_ args -> do
                            fmap LVN $ V.mapM getVarLit args
                         }
          fieldOpFn i =
            let opDefName = namesVector V.! i
             in OpDef { opDefIndex = idx + fromIntegral i + 1
                      , opDefName
                      , opDefPrec = defaultPrec
                      , opDefArgTypes = V.singleton recType
                      , opDefResultType = SymShapeVar opDefName
                      , opDefKind = PrimOp
                      , opDefEval = UnaryOpEval $ \_ (CRec _ _ rf) -> rf V.! i
                      , opDefDescriptor = \_ args -> do
                         LVN lv <- getVarLit (args V.! 0)
                         return (lv V.! i)
                      }
          fieldOps = V.map fieldOpFn (V.enumFromN 0 fieldCount)
          recDef   = SymRecDef { recDefCtor = ctorOp
                               , recDefFieldOps = fieldOps }
          sub = emptySubst { shapeSubst = Map.fromList 
                                        $ map (\nm -> (nm, SymShapeVar nm))
                                        $ namesList }
          recType  = SymRec recDef sub
      OS $ modify $ \s ->
             s { nextOpDefIndex = idx + fromIntegral fieldCount + 1
               , structuralRecordMap = Map.insert names recDef (structuralRecordMap s) }
      return recDef

-- | Returns defined structural records parsed so far.
listStructuralRecords :: OpSession (Map (Set String) SymRecDef)
listStructuralRecords = OS $ gets structuralRecordMap

-- BitBlastMonad {{{1

class Monad m => BitBlastMonad m where
  -- | Maps each term index to the associated lit vector if any.
  getAppLitResult :: MonadTerm m -> m (Maybe (LitResult Lit))
  -- | Maps each term index to the associated lit vector if any.
  setAppLitResult :: MonadTerm m -> LitResult Lit -> m ()
  -- | Returns lit vector for non-application nodes.
  constLitResult :: MonadTerm m -> m (LitResult Lit)
  -- | Runs aig computation inside this monad.
  liftAigMonad :: AigComputation OpSession a -> m a
  -- | Runs op session inside this monad.
  liftOpSession :: OpSession a -> m a
  liftOpSession = liftAigMonad . lift

instance (BitBlastMonad m, MonadIO m) => BitBlastMonad (StateT s m) where
  getAppLitResult      = lift . getAppLitResult
  setAppLitResult n lv = lift $ setAppLitResult n lv
  constLitResult       = lift . constLitResult
  liftAigMonad         = lift . liftAigMonad

-- WordMonad {{{1
class ( ApplyOpMonad m
      , BitBlastMonad m
      , ConstantInjection (MonadTerm m)
      , ConstantProjection (MonadTerm m)
      , Ord (MonadTerm m)
      , Functor m
      , CatchMIO m
      , PrettyTerm (MonadTerm m)
      , Show (MonadTerm m)
      , TypedTerm (MonadTerm m)
      , Typeable (MonadTerm m)
      , Typeable1 m
      ) => WordMonad m where

  termInt  :: Int32 -> m (MonadTerm m)
  termLong :: Int64 -> m (MonadTerm m)
  termBool :: Bool  -> m (MonadTerm m)

  -- | Allocates a fresh variable where the 8 low-order bits are fresh lits and the
  -- upper bits are zero.
  freshByte :: m (MonadTerm m)
  freshInt  :: m (MonadTerm m)
  freshLong :: m (MonadTerm m)

  freshUninterpretedVar :: DagType -> m (MonadTerm m)

  freshVar :: DagType -> LitResult Lit -> m (MonadTerm m)

  -- | compare equality of two arguments.
  applyEq :: (MonadTerm m) -> (MonadTerm m) -> m (MonadTerm m)
  -- | Apply iteOp to condition, true branch, and false branch.
  applyIte :: MonadTerm m -> MonadTerm m -> MonadTerm m -> m (MonadTerm m)

  -- | Complement argument.
  applyBNot :: MonadTerm m -> m (MonadTerm m)
  -- | and of arguments.
  applyBAnd :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | or of arguments.
  applyBOr  :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | exclusive or of arguments.
  applyBXor :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)

  -- | Complement bits of arguments.
  applyINot :: MonadTerm m -> m (MonadTerm m)
  -- | Bitwise and of arguments.
  applyIAnd :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Bitwise or of arguments.
  applyIOr  :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Bitwise exclusive or of arguments.
  applyIXor :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)

  -- | Append two integer terms together.
  applyAppendInt :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Signed extension of input to given width.
  applySignedExt :: BitWidth -> MonadTerm m -> m (MonadTerm m)
  -- | Apply truncOp to truncate term (MonadTerm m)o given bitwidth.
  applyTrunc :: BitWidth -> MonadTerm m -> m (MonadTerm m)

  -- | @applyUShr x y@ treats @x@ and @y@ as unsigned and shifts @x@
  -- to the left by @y@ bits.
  applyShl :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | @applyUShr x y@ treats @x@ as signed and @y@ as unsigned and shifts @x@
  -- to the right by @y@ bits.
  applyShr :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | @applyUShr x y@ treats @x@ and @y@ as unsigned values and shifts @x@
  -- to the right by @y@ bits.
  applyUshr :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)

  -- | Applys signed less-than-or-equal comparison.
  applySignedLeq   :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Applys signed less-than comparison.
  applySignedLt    :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Applys unsigned less-than-or-equal comparison.
  applyUnsignedLeq :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Applys unsigned less-than comparison.
  applyUnsignedLt  :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)

  -- | Adds two arguments.
  applyAdd :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Multiplies two arguments.
  applyMul :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Negates argument
  applyNeg :: MonadTerm m -> m (MonadTerm m)
  -- | Returns signed division of two arguments.
  applySignedDiv :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Returns signed remainder of two arguments.
  applySignedRem :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | Subtracts two integer arguments.
  applySub :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)

  -- | @applyGetArrayValue arr i@ returns value at @arr[i]@.
  applyGetArrayValue :: MonadTerm m -> MonadTerm m -> m (MonadTerm m)
  -- | @applySetArrayValue arr i v@ returns value at @arr[i] = v@.
  applySetArrayValue :: MonadTerm m -> MonadTerm m -> MonadTerm m -> m (MonadTerm m)

  -- | Constructs a symbolic array from a list of values and a default value for initial elements.
  symbolicArrayFromV :: DagType -- ^ Element type
                     -> V.Vector (MonadTerm m) -- ^ Elements
                     -> m (MonadTerm m)

  -- | Constructs a symbolic array from a list of values and a default value for initial elements.
  -- Note: There should be at most 2^31-1 values.
  symbolicArrayFromList :: DagType -- ^ Element type
                        -> [MonadTerm m] -- ^ Elements
                        -> m (MonadTerm m)
  symbolicArrayFromList eltType = symbolicArrayFromV eltType . V.fromList

-- AigOps {{{1

class ( Applicative m
      , BitBlastMonad m
      , CatchMIO m
      , LogMonad m
      , WordMonad m
      , Show (MonadTerm m)
      ) => AigOps m where

  -- | @evalAigIntegral' f ins out@ applies concrete inputs @ins@ to the AIG at the
  -- given symbolic output term @out@, applying @f@ to the @ins@ bit sequence
  evalAigIntegral :: ([Bool] -> [Bool]) -> [CValue] -> MonadTerm m -> m (MonadTerm m)

  -- | @evalAigArray w ins outs@ applies concrete inputs @ins@ to the AIG at the
  -- given symbolic output terms @outs@.  Each output is assumed to be w bits.
  evalAigArray    :: BitWidth -> [CValue] -> [MonadTerm m] -> m [MonadTerm m]

  writeAigToFile :: FilePath -> SV.Vector Lit -> m ()

  -- | Index the given lit vector and convert its result to a concrete Lit value
  unsafeIndexCvt :: SV.Vector Lit -> Int -> m Lit

  -- | Make a litresult for application.
  makeLitResult :: Op -> V.Vector (MonadTerm m) -> m (LitResult Lit)

  -- | Returns lit vector associated with given term, or fails
  -- if term cannot be bitblasted.
  -- May throw an exception if literal cannot be bitblasted.
  getVarLit :: MonadTerm m -> m (LitResult Lit)

-- SupportsSession {{{1

class SupportsSession m where
  runSymSession :: m a -> OpSession a

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
  prettyTerm       = renderStyle style {lineLength = ppLineLength defaultPPConfig} . prettyTermD
  prettyTermWith c = renderStyle style {lineLength = ppLineLength c              } . prettyTermWithD c

-- Misc utility functions {{{1

type OpIndex = Word32

intToHex :: CValue -> String
intToHex (CInt (Wx 32) c) =
  let r = showHex (fromIntegral c :: Word32) ""
   in replicate (8 - length r) '0' ++ r
intToHex (CInt (Wx 64)  c) =
  let r = showHex (fromIntegral c :: Word64) ""
   in replicate (16 - length r) '0' ++ r
intToHex _ = error $ "internal: Undefined intToHex for type"

intToBoolSeq :: CValue -> [Bool]
intToBoolSeq = hexToBoolSeq . intToHex

boolSeqToInt8 :: [Bool] -> Int32
boolSeqToInt8 bs = toEnum . fromEnum . head $ ns
  where ns :: [Int8]
        ns = hexToNumSeq 2 . boolSeqToHex $ bs

boolSeqToInt32 :: [Bool] -> Int32
boolSeqToInt32 = head . hexToIntSeq . boolSeqToHex

boolSeqToInt64 :: [Bool] -> Int64
boolSeqToInt64 = head . hexToLongSeq . boolSeqToHex

boolSeqToHex :: [Bool] -> String
boolSeqToHex bs = reverse (impl bs)
  where fn b i = if b then i else 0
        ch [x0, x1, x2, x3] = intToDigit (fn x3 8 + fn x2 4 + fn x1 2 + fn x0 1)
        ch _ = error "internal: 'char': unexpected input length"
        impl [] = []
        impl s = let (first',s1)  = splitAt 4 s
                     (second',s2) = splitAt 4 s1
                  in ch first' : ch second' : impl s2

hexToBoolSeq :: String -> [Bool]
hexToBoolSeq s =
  let ch c = map (testBit $ digitToInt c) [0..3]
      loop (x : y : rest) = ch x ++ ch y ++ loop rest
      loop [] = []
      loop _ = error "hexToBoolSeq: invalid string"
   in loop $ reverse s

hexToIntSeq :: String -> [Int32]
hexToIntSeq = reverse . impl
  where impl (x0 : x1 : x2 : x3 : x4 : x5 : x6 : x7 : r)
         = fromIntegral (  (digitToInt x0) `shiftL` 28
                         + (digitToInt x1) `shiftL` 24
                         + (digitToInt x2) `shiftL` 20
                         + (digitToInt x3) `shiftL` 16
                         + (digitToInt x4) `shiftL` 12
                         + (digitToInt x5) `shiftL`  8
                         + (digitToInt x6) `shiftL`  4
                         + (digitToInt x7))
           : impl r
        impl [] = []
        impl _ = error "internal: hexToIntSeq invalid input string"

hexToLongSeq :: String -> [Int64]
hexToLongSeq = reverse . impl
  where impl xs@(_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:r) =
          foldr (+) 0 [ dTo64 xi `shiftL` bi
                      | (xi, bi) <- xs `zip` reverse [4*i|i<-[0..15]]
                      ]
          : impl r
        impl [] = []
        impl _  = error "internal: hexToLongSeq invalid input string"
        dTo64   = fromIntegral . digitToInt

hexToNumSeq :: (Bits a) => Int -> String -> [a]
hexToNumSeq n = reverse . impl
  where impl xs | length xs >= n =
          foldr (+) 0 [ dToNum xi `shiftL` bi
                      | (xi, bi) <- xs `zip` reverse [4*i|i<-[0..n]]
                      ]
          : impl (drop n xs)
        impl [] = []
        impl _  = error "internal: hexToNumSeq invalid input string"
        dToNum  = fromIntegral . digitToInt

constInt :: Int32 -> CValue
constInt = mkCInt (Wx 32) . fromIntegral

intFromConst :: CValue -> Int32
intFromConst (CInt (Wx 32) x) = fromIntegral x
intFromConst v = error $ "internal: CValue " ++ show v ++ " not an int"

constLong :: Int64 -> CValue
constLong = CInt (Wx 64) . fromIntegral

longFromConst :: CValue -> Int64
longFromConst (CInt (Wx 64) x) = fromIntegral x
longFromConst v = error $ "internal: CValue " ++ show v ++ " not a long"

constBool :: Bool -> CValue
constBool = CBool

boolFromConst :: CValue -> Bool
boolFromConst (CBool b)         = b
boolFromConst (CInt (Wx 32) 0) = False
boolFromConst (CInt (Wx 32) 1) = True
boolFromConst v = error $ "internal: CValue " ++ show v ++ " not a bool"

evalAigArgs8 :: [Int32] -> [Bool]
evalAigArgs8 = concatMap (take 8 . intToBoolSeq) . map constInt

evalAigArgs32 :: [Int32] -> [Bool]
evalAigArgs32 = concatMap intToBoolSeq . map constInt

evalAigArgs64 :: [Int64] -> [Bool]
evalAigArgs64 = concatMap intToBoolSeq . map constLong

outsToInts8 :: Int -> [Bool] -> [Int32]
outsToInts8 n outs  = [ boolSeqToInt8 $ take 8 (drop (8*k) outs) | k <- [0..(n-1)] ]

outsToInts32 :: Int -> [Bool] -> [Int32]
outsToInts32 n outs  = [ boolSeqToInt32 $ take 32 (drop (32*k) outs) | k <- [0..(n-1)] ]

outsToInts64 :: Int -> [Bool] -> [Int64]
outsToInts64 n outs  = [ boolSeqToInt64 $ take 64 (drop (64*k) outs) | k <- [0..(n-1)] ]

intSeqToHex :: [CValue] -> String
intSeqToHex = foldl (\s c -> intToHex c ++ s) []

intSeqToBoolSeq :: [CValue] -> [Bool]
intSeqToBoolSeq = hexToBoolSeq . intSeqToHex

hexToByteSeq :: String -> [Int32]
hexToByteSeq (x : y : r)
  = fromIntegral (16 * (digitToInt x) + (digitToInt y)) : hexToByteSeq r
hexToByteSeq [] = []
hexToByteSeq _ = error "internal: hexToByteSeq: invalid input string"

byteSeqToHex :: [CValue] -> String
byteSeqToHex (CInt (Wx 32) c : r)
  = (intToDigit $ fromIntegral $ ((fromIntegral c :: Word32) `quot` 16) `rem` 16)
    : (intToDigit $ fromIntegral $ (fromIntegral c :: Word32) `rem`  16)
    : byteSeqToHex r
byteSeqToHex (CInt w _c : _r)
  = error $ "internal: byteSeqToHex unexpected width " ++ show w
byteSeqToHex (CArray _ : _) = error "internal: byteSeqToHex CArray"
byteSeqToHex (CBool _ : _) = error "internal: byteSeqToHex CBool"
byteSeqToHex (CRec{} : _) = error "internal: byteSeqToHex CRec"
byteSeqToHex [] = []
