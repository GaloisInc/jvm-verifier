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

class ( Ord (MonadTerm m)
      , PrettyTerm (MonadTerm m)
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
         -- | Bitwise and of arguments.
       , termIAnd :: BinaryOp sym
         -- | Bitwise or of arguments.
       , termIOr  :: BinaryOp sym
         -- | Bitwise exclusive or of arguments.
       , termIXor :: BinaryOp sym
         -- | Java shift-left on int values.
       , termIShl  :: BinaryOp sym
         -- | Java signed shift-right on int values.
       , termIShr  :: BinaryOp sym
         -- | Java unsigned shift-right on int values.
       , termIUshr :: BinaryOp sym
         -- | Java shift-left on long values.
       , termLShl  :: BinaryOp sym
         -- | Java signed shift-right on long values.
       , termLShr  :: BinaryOp sym
         -- | Java unsigned shift-right on long values.
       , termLUshr :: BinaryOp sym
         -- | Less than or equal.
       , termLeq :: BinaryOp sym
         -- |Less than
       , termLt :: BinaryOp sym
         -- | Negates argument
       , termNeg :: UnaryOp sym
         -- | Adds two arguments.
       , termAdd :: BinaryOp sym
         -- | Subtracts two integer arguments.
       , termSub :: BinaryOp sym
         -- | Multiplies two arguments.
       , termMul :: BinaryOp sym
         -- | Returns signed division of two arguments.
       , termDiv :: BinaryOp sym 
         -- | Returns signed remainder of two arguments.
       , termRem :: BinaryOp sym
         -- | @termIntArray l@ returns an integer array of zeros with length
         -- @l@.  Will return @Nothing@ is operation cannot be performed because
         -- length is symbolic and symbolic lengths are unsupported. 
       , termIntArray :: MonadTerm sym -> IO (Maybe (MonadTerm sym))
         -- | @termLongArray l@ returns a long array of zeros with length @l@.
       , termLongArray :: MonadTerm sym -> IO (Maybe (MonadTerm sym))
         -- | @applyGetArrayValue arr i@ returns value at @arr[i]@.
       , applyGetArrayValue :: BinaryOp sym
         -- | @applySetArrayValue arr i v@ returns value at @arr[i] = v@.
       , applySetArrayValue :: MonadTerm sym -> MonadTerm sym -> MonadTerm sym -> IO (MonadTerm sym)
       , blastTerm :: MonadTerm sym -> IO (Maybe Bool)
         -- | @evalAigIntegral f ins out@ applies concrete inputs @ins@ to the 
         -- AIG at the given symbolic output term @out@, applying @f@ to the
         -- @ins@ bit sequence
       , evalAigIntegral :: ([Bool] -> [Bool]) -> [MonadTerm sym] 
                             -> MonadTerm sym -> IO (MonadTerm sym)
         -- | @evalAigArray w ins outs@ applies concrete inputs @ins@ to the
         -- AIG at the given symbolic output terms @outs@.  Each output is
         -- assumed to be w bits.  If @ins@ is not a constant, then this fails.
       , evalAigArray :: BitWidth -> [MonadTerm sym] -> [MonadTerm sym] -> IO [MonadTerm sym]
       , writeAigToFile :: FilePath -> SV.Vector Lit -> IO ()
         -- | Returns lit vector associated with given term, or fails
         -- if term cannot be bitblasted.
       , getVarLit :: MonadTerm sym -> IO (LitResult Lit)
       }