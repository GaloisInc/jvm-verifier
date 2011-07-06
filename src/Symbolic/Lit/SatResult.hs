{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Symbolic.Lit.SatResult where

import Data.Vector.Storable as SV

-- | Result of checking if literal is satisfible.
data SatResult = Sat (SV.Vector Bool) | UnSat | Unknown
  deriving (Eq,Show)

type family MonadLit m

class ( Eq (MonadLit m)
      , SV.Storable (MonadLit m)
      , Functor m
      , Monad m)
      => LitMonad m where

  -- | Literal for true constant.
  litTrue :: m (MonadLit m)
  -- | Literal for false constant.
  litFalse :: m (MonadLit m)
  -- | Create and literal.
  litAnd :: MonadLit m -> MonadLit m -> m (MonadLit m)

  -- | Returns number of input literalas added to AIG.
  getInputLitCount :: m Int
  -- | Create a new input entry.
  makeInputLit :: m (MonadLit m)

  -- | Returns true if checking satisfiability is supported by this monad.
  canCheckSat :: m Bool
  -- | Attempts to check satisfiability of lit.
  checkSat :: MonadLit m -> m SatResult

  -- | Evaluate given AIG on inputs
  evalAigV :: SV.Vector Bool
           -> SV.Vector (MonadLit m)
           -> m (SV.Vector Bool)
  -- | Write aiger with given output bits to filename.
  writeAigerV :: FilePath -- ^ Filename
              -> SV.Vector (MonadLit m) -- ^ Output lits
              -> m ()
