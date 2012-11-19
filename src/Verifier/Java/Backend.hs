{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.Java.Backend 
  ( PrettyTerm(..)
  , Typeable
  , UnaryOp
  , BinaryOp
  , MonadTerm
  , AigOps
  , Backend(..)
  , toLsbfV
  ) where

import Data.Int
import Data.Typeable
import qualified Data.Vector.Storable as SV

import Verinf.Symbolic (BitWidth, PrettyTerm(..), Lit, LitResult, toLsbfV)

type UnaryOp sym = MonadTerm sym -> IO (MonadTerm sym)
type BinaryOp sym = MonadTerm sym -> MonadTerm sym -> IO (MonadTerm sym)

-- | Returns term type associated with monad.
type family MonadTerm m

class ( PrettyTerm (MonadTerm m)
      , Show (MonadTerm m)
      , Typeable (MonadTerm m)
      ) => AigOps m where
 
data Backend sym = Backend {
          -- | Allocates a fresh variable where the 8 low-order bits are fresh
          -- lits and the upper bits are zero.
         freshByte :: IO (MonadTerm sym)
       , freshInt  :: IO (MonadTerm sym)
       , freshLong :: IO (MonadTerm sym)
       , asBool :: MonadTerm sym -> Maybe Bool
       , asInt :: MonadTerm sym -> Maybe Int32
       , asLong :: MonadTerm sym -> Maybe Int64
       , termBool :: Bool  -> IO (MonadTerm sym)  
       , termInt  :: Int32 -> IO (MonadTerm sym)
       , termLong :: Int64 -> IO (MonadTerm sym)
       , termByteFromInt :: UnaryOp sym
       , termLongFromInt :: UnaryOp sym
       , termIntFromLong :: UnaryOp sym
         -- | Complement argument.
       , termNot :: UnaryOp sym
         -- | Return conjunction of two arguments. 
       , termAnd :: BinaryOp sym
         -- | Compare equality of arguments.
       , termEq :: BinaryOp sym
         -- | Form if-then-else comparing arguments.
       , termIte :: MonadTerm sym -> MonadTerm sym -> MonadTerm sym -> IO (MonadTerm sym)

         -- | Take two 32-bit integers, and return true if first is less than second.
       , termILeq :: BinaryOp sym
         -- | Bitwise and of two 32bit integers.
       , termIAnd :: BinaryOp sym
         -- | Bitwise or of two 32bit integers.
       , termIOr  :: BinaryOp sym
         -- | Bitwise exclusive or of arguments.
       , termIXor :: BinaryOp sym
         -- | Java shift-left on int values.
       , termIShl  :: BinaryOp sym
         -- | Java signed shift-right on int values.
       , termIShr  :: BinaryOp sym
         -- | Java unsigned shift-right on int values.
       , termIUshr :: BinaryOp sym
         -- | Negates 32bit integer argument
       , termINeg :: UnaryOp sym
         -- | Adds two 32bit integer arguments.
       , termIAdd :: BinaryOp sym
         -- | Subtracts one 32bit integer from another.
       , termISub :: BinaryOp sym
         -- | Multiplies two 32bit integers.
       , termIMul :: BinaryOp sym
         -- | Returns signed division of two 32bit integers.
       , termIDiv :: BinaryOp sym
         -- | Returns signed remainder of two 32bit integers.
       , termIRem :: BinaryOp sym

         -- | Compare two 64bit integers (x & y), and return one of three 32-bit integers:
         -- if x < y then return -1
         -- if x == y then return 0
         -- if x > y then return 1
       , termLCompare :: BinaryOp sym
         -- | Bitwise and of two 32bit integers.
       , termLAnd  :: BinaryOp sym
         -- | Bitwise or of two 32bit integers.
       , termLOr   :: BinaryOp sym
         -- | Bitwise exclusive or of arguments.
       , termLXor  :: BinaryOp sym
         -- | Java shift-left on long values.
       , termLShl  :: BinaryOp sym
         -- | Java signed shift-right on long values.
       , termLShr  :: BinaryOp sym
         -- | Java unsigned shift-right on long values.
       , termLUshr :: BinaryOp sym
         -- | Negates 64bit integer.
       , termLNeg :: UnaryOp sym
         -- | Adds two 64bit integers.
       , termLAdd :: BinaryOp sym
         -- | Subtracts one 64bit integer from another.
       , termLSub :: BinaryOp sym
         -- | Multiplies two 64bit integers.
       , termLMul :: BinaryOp sym
         -- | Returns signed division of two 64bit integers.
       , termLDiv :: BinaryOp sym
         -- | Returns signed remainder of two 64bit integers.
       , termLRem :: BinaryOp sym

         -- | @termIntArray l@ returns an integer array of zeros with length
         -- @l@.  Will return @Nothing@ is operation cannot be performed because
         -- length is symbolic and symbolic lengths are unsupported. 
       , termIntArray :: MonadTerm sym -> IO (Maybe (MonadTerm sym))
         -- | @termLongArray l@ returns a long array of zeros with length @l@.
       , termLongArray :: MonadTerm sym -> IO (Maybe (MonadTerm sym))
         -- | @termGetIntArray l arr i@ returns value at @arr[i]@.  The length is
         -- the expected length of the array.  The length is passed in case the backend
         -- needs it.
       , termGetIntArray :: MonadTerm sym -- ^ length
                         -> MonadTerm sym -- ^ array
                         -> MonadTerm sym -- ^ index
                         -> IO (MonadTerm sym)
         -- | @termGetLongArray l arr i@ returns value at @arr[i]@.
       , termGetLongArray :: MonadTerm sym -- ^ length
                          -> MonadTerm sym -- ^ array
                          -> MonadTerm sym -- ^ index
                          -> IO (MonadTerm sym)
         -- | @termSetIntArray l arr i v@ returns value at @arr[i] = v@.
       , termSetIntArray :: MonadTerm sym -- ^ length
                         -> MonadTerm sym -- ^ array
                         -> MonadTerm sym -- ^ index
                         -> MonadTerm sym -- ^ value
                         -> IO (MonadTerm sym)
         -- | @termSetLongArray l arr i v@ returns value at @arr[i] = v@.
       , termSetLongArray :: MonadTerm sym -- ^ length
                          -> MonadTerm sym -- ^ array
                          -> MonadTerm sym -- ^ index
                          -> MonadTerm sym -- ^ value
                          -> IO (MonadTerm sym)
         -- | @blastTerm t@ bitblasts the Boolean term @t@ and returns a maybe value indicating
         -- if it is equivalent to the constant true or false.
       , blastTerm :: MonadTerm sym -> IO (Maybe Bool)
        -- TODO: we may, at some point, want to get back a satisfying assignment
       , satTerm :: MonadTerm sym -> IO Bool
         -- | @evalAigIntegral f ins out@ applies concrete inputs @ins@ to the 
         -- AIG at the given symbolic output term @out@, applying @f@ to the
         -- @ins@ bit sequence
       , evalAigIntegral :: ([Bool] -> [Bool])
                         -> [MonadTerm sym] 
                         -> MonadTerm sym
                         -> IO (MonadTerm sym)
         -- | @evalAigArray w ins outs@ applies concrete inputs @ins@ to the
         -- AIG at the given symbolic output terms @outs@.  Each output is
         -- assumed to be w bits.  If @ins@ is not a constant, then this fails.
       , evalAigArray :: BitWidth -> [MonadTerm sym] -> [MonadTerm sym] -> IO [MonadTerm sym]
       , writeAigToFile :: FilePath -> SV.Vector Lit -> IO ()
         -- | Returns lit vector associated with given term, or fails
         -- if term cannot be bitblasted.
       , getVarLit :: MonadTerm sym -> IO (LitResult Lit)
       }
