{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.Java.Backend
  ( Typeable
  , UnaryOp
  , BinaryOp
  , SBELit
  , SBETerm
  , AigOps
  , Backend(..)
  ) where

import Data.Int
import Data.Typeable
import qualified Data.Vector.Storable as SV
import Text.PrettyPrint (Doc)

type UnaryOp sbe = SBETerm sbe -> IO (SBETerm sbe)
type BinaryOp sbe = SBETerm sbe -> SBETerm sbe -> IO (SBETerm sbe)

-- | Returns term type associated with monad.
type family SBETerm (sbe :: *)
type family SBELit (sbe :: *)

class ( SV.Storable (SBELit m)
      , Show (SBETerm m)
      , Typeable (SBETerm m)
      ) => AigOps m where

data Backend sbe = Backend {
          -- | Allocates a fresh variable where the 8 low-order bits are fresh
          -- lits and the upper bits are zero.
         freshByte :: IO (SBETerm sbe)
       , freshInt  :: IO (SBETerm sbe)
       , freshLong :: IO (SBETerm sbe)
       , asBool :: SBETerm sbe -> Maybe Bool
       , asInt :: SBETerm sbe -> Maybe Int32
       , asLong :: SBETerm sbe -> Maybe Int64
       , termBool :: Bool  -> IO (SBETerm sbe)
       , termInt  :: Int32 -> IO (SBETerm sbe)
       , termLong :: Int64 -> IO (SBETerm sbe)
       , termByteFromInt :: UnaryOp sbe
       , termLongFromInt :: UnaryOp sbe
       , termIntFromLong :: UnaryOp sbe
         -- | Complement argument.
       , termNot :: UnaryOp sbe
         -- | Return conjunction of two arguments.
       , termAnd :: BinaryOp sbe
         -- | Compare equality of arguments.
       , termEq :: BinaryOp sbe
         -- | Form if-then-else comparing arguments.
       , termIte :: SBETerm sbe -> SBETerm sbe -> SBETerm sbe -> IO (SBETerm sbe)

         -- | Take two 32-bit integers, and return true if first is less than second.
       , termILeq :: BinaryOp sbe
         -- | Bitwise and of two 32bit integers.
       , termIAnd :: BinaryOp sbe
         -- | Bitwise or of two 32bit integers.
       , termIOr  :: BinaryOp sbe
         -- | Bitwise exclusive or of arguments.
       , termIXor :: BinaryOp sbe
         -- | Java shift-left on int values.
       , termIShl  :: BinaryOp sbe
         -- | Java signed shift-right on int values.
       , termIShr  :: BinaryOp sbe
         -- | Java unsigned shift-right on int values.
       , termIUshr :: BinaryOp sbe
         -- | Negates 32bit integer argument
       , termINeg :: UnaryOp sbe
         -- | Adds two 32bit integer arguments.
       , termIAdd :: BinaryOp sbe
         -- | Subtracts one 32bit integer from another.
       , termISub :: BinaryOp sbe
         -- | Multiplies two 32bit integers.
       , termIMul :: BinaryOp sbe
         -- | Returns signed division of two 32bit integers.
       , termIDiv :: BinaryOp sbe
         -- | Returns signed remainder of two 32bit integers.
       , termIRem :: BinaryOp sbe

         -- | Compare two 64bit integers (x & y), and return one of three 32-bit integers:
         -- if x < y then return -1
         -- if x == y then return 0
         -- if x > y then return 1
       , termLCompare :: BinaryOp sbe
         -- | Bitwise and of two 32bit integers.
       , termLAnd  :: BinaryOp sbe
         -- | Bitwise or of two 32bit integers.
       , termLOr   :: BinaryOp sbe
         -- | Bitwise exclusive or of arguments.
       , termLXor  :: BinaryOp sbe
         -- | Java shift-left on long values.
       , termLShl  :: BinaryOp sbe
         -- | Java signed shift-right on long values.
       , termLShr  :: BinaryOp sbe
         -- | Java unsigned shift-right on long values.
       , termLUshr :: BinaryOp sbe
         -- | Negates 64bit integer.
       , termLNeg :: UnaryOp sbe
         -- | Adds two 64bit integers.
       , termLAdd :: BinaryOp sbe
         -- | Subtracts one 64bit integer from another.
       , termLSub :: BinaryOp sbe
         -- | Multiplies two 64bit integers.
       , termLMul :: BinaryOp sbe
         -- | Returns signed division of two 64bit integers.
       , termLDiv :: BinaryOp sbe
         -- | Returns signed remainder of two 64bit integers.
       , termLRem :: BinaryOp sbe

         -- | @termIntArray l@ returns an integer array of zeros with
         -- length @l@.  Will return @Nothing@ is operation cannot be
         -- performed because length is symbolic and symbolic lengths
         -- are unsupported.
       , termIntArray :: SBETerm sbe -> IO (Maybe (SBETerm sbe))
         -- | @termLongArray l@ returns a long array of zeros with length @l@.
       , termLongArray :: SBETerm sbe -> IO (Maybe (SBETerm sbe))
         -- | @termGetIntArray l arr i@ returns value at @arr[i]@.
         -- The length @l@ is the expected length of the array.  The
         -- length is passed in case the backend needs it.
       , termGetIntArray :: SBETerm sbe
                         -> SBETerm sbe
                         -> SBETerm sbe
                         -> IO (SBETerm sbe)
         -- | @termGetLongArray l arr i@ returns value at @arr[i]@,
         -- where @l@ is the expected lengthe array.
       , termGetLongArray :: SBETerm sbe
                          -> SBETerm sbe
                          -> SBETerm sbe
                          -> IO (SBETerm sbe)
         -- | @termSetIntArray l arr i v@ returns value at @arr[i] =
         -- v@, where @l@ is the expected lengthe array.
       , termSetIntArray :: SBETerm sbe
                         -> SBETerm sbe
                         -> SBETerm sbe
                         -> SBETerm sbe
                         -> IO (SBETerm sbe)
         -- | @termSetLongArray l arr i v@ returns value at @arr[i] =
         -- v@, where @l@ is the expected lengthe array.
       , termSetLongArray :: SBETerm sbe
                          -> SBETerm sbe
                          -> SBETerm sbe
                          -> SBETerm sbe
                          -> IO (SBETerm sbe)
         -- | @blastTerm t@ bitblasts the Boolean term @t@ and returns
         -- a 'Maybe' value indicating if it is equivalent to the
         -- constant 'True' or 'False'.
       , blastTerm :: SBETerm sbe -> IO (Maybe Bool)
        -- TODO: we may, at some point, want to get back a satisfying assignment
       , satTerm :: SBETerm sbe -> IO Bool
         -- | @evalAigIntegral f ins out@ applies concrete inputs @ins@ to the
         -- AIG at the given symbolic output term @out@, applying @f@ to the
         -- @ins@ bit sequence
       , evalAigIntegral :: ([Bool] -> [Bool])
                         -> [SBETerm sbe]
                         -> SBETerm sbe
                         -> IO (SBETerm sbe)
         -- | @evalAigArray w ins outs@ applies concrete inputs @ins@ to the
         -- AIG at the given symbolic output terms @outs@.  Each output is
         -- assumed to be w bits.  If @ins@ is not a constant, then this fails.
       , evalAigArray :: Int -> [SBETerm sbe] -> [SBETerm sbe] -> IO [SBETerm sbe]
       , writeAigToFile :: FilePath -> SV.Vector (SBELit sbe) -> IO ()
         -- | Returns lit vector associated with given term, or fails
         -- if term cannot be bitblasted.
       , getVarLit :: SBETerm sbe -> IO (SV.Vector (SBELit sbe))
       , prettyTermD :: SBETerm sbe -> Doc
       }
