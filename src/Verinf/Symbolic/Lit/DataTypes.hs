{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : stable
Point-of-contact : jhendrix
-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE CPP #-}
module Verinf.Symbolic.Lit.DataTypes
  ( SatResult(..)
    -- * Quantifiers for QBF.
  , Quantifiers, forall, exists, mempty, mappend, ppQDIMACSQuantifiers
  , BitEngine(..)
  , LitVector
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Foldable (Foldable)
import Data.Monoid
import Data.Traversable (Traversable)
#endif
import qualified Data.Foldable as Fold
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

-- | Result of checking if literal is satisfible.
data SatResult = Sat (SV.Vector Bool) | UnSat | Unknown
  deriving (Eq,Show)

data Quantifier = Forall | Exists
  deriving (Eq,Ord, Show)

data QuantifierSet l = QS !Quantifier !(Seq l)
  deriving (Functor, Foldable, Traversable)

-- | A list of quantifiers for variables when printing to QDIMACS.
-- Outer quantifiers appear earlier in the sequence.
-- This datatype is abstract so that quantifiers may be normalized
-- for pretty-printing.
newtype Quantifiers l = Q (Seq (QuantifierSet l))
  deriving (Functor, Foldable, Traversable)

instance Monoid (Quantifiers l) where
  mempty = Q Seq.empty
  mappend (Q x) (Q y) = Q $
    case (Seq.viewr x, Seq.viewl y) of
      (x' Seq.:> QS xq xl, QS yq yl Seq.:< y') | xq == yq ->
        x' Seq.>< (QS yq (xl `mappend` yl) Seq.<| y')
      _ -> x Seq.>< y

forall :: l -> Quantifiers l
forall l = Q $ Seq.singleton $ QS Forall $ Seq.singleton $! l

exists :: l -> Quantifiers l
exists l = Q $ Seq.singleton $ QS Exists $ Seq.singleton $! l

ppQDIMACSQuantifiers :: Show a => Quantifiers a -> String
ppQDIMACSQuantifiers qs = unlines $ lineFmt <$> viewQuantifiers qs
  where lineFmt (q,ql) = qFmt q : ' ' : unwords (show <$> ql) ++ " 0"
        qFmt Forall = 'a'
        qFmt Exists = 'e'

-- | View quantifiers as a list for printing.
viewQuantifiers :: Quantifiers l -> [(Quantifier,[l])]
viewQuantifiers (Q x) = toPair <$> Fold.toList x
  where toPair (QS q l) = (q,Fold.toList l)

data BitEngine l = BitEngine
  { -- | Literal for true constant.
    beTrue :: l
    -- | Literal for false constant.
  , beFalse :: l
    -- | Negate literal
  , beNeg :: l -> l
    -- | Return literal representing conjunction of inputs.
  , beAnd :: l -> l -> IO l
    -- | Return literal representing xor of inputs.
  , beXor :: l -> l -> IO l
    -- | Return literal representing mux of inputs.
  , beMux :: l -> l -> l -> IO l
    -- | Returns true if the literals are identical
  , beEqLit :: l -> l -> Bool
    -- | Returns number of input literals added to AIG.
  , beInputLitCount :: IO Int
    -- | Returns vector with inputs in order they were added.
  , beInputLits :: IO (SV.Vector l)
    -- | Create a new input entry.
  , beMakeInputLit :: IO l
    -- | Returns function for checking satisfiability if defined.
  , beCheckSat :: Maybe (l -> IO SatResult)
    -- | Evaluate given AIG on inputs
  , beEvalAigV :: SV.Vector Bool -> SV.Vector l -> IO (SV.Vector Bool)
    -- | Write aiger with given output bits to filename.
  , beWriteAigerV :: FilePath
                     -- Filename
                  -> SV.Vector l
                     -- Input lits to slice.
                  -> SV.Vector l
                     -- Output lits (justposed in order of given list)
                  -> IO ()
  -- | Write CNF file with given output bit, and return the mapping of
  -- CNF variables to AIG variables. The index corresponds to the AIG
  -- variable id, and the value indicates the CNF variable id. The
  -- second argument, which may be empty, lists variable
  -- quantification bindings for QDIMACS output.
  -- Outer quantifiers are listed first.
  , beWriteCNF :: FilePath -> Quantifiers l -> l -> IO (V.Vector (Maybe Int))
    -- | Free resources used by bit engine (may not be used after this).
  , beFree :: IO ()
  }

{-
beWriteAigerV :: Storable l => BitEngine l -> FilePath -> [SV.Vector l] -> IO ()
beWriteAigerV be path outputs = do
  cnt <- beInputLitCount be
  beWriteAigerV' be path (SV.generate cnt id) (SV.concat outputs)
-}

-- | Bit vector with ints that are stored least-significant byte first.
type LitVector = SV.Vector
