{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix
-}

-- This module provides operations on literals and data structures for word level operations on them.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
module Symbolic.Lit
#if defined(UseABC)
  ( module Symbolic.Lit.ABC
#else
  ( module Symbolic.Lit.Pure
#endif
  , canCallABC
  , litFromBool
  ) where

#if defined(UseABC)
import Symbolic.Lit.ABC
#else
import Symbolic.Lit.Pure
#endif

type AigMonad = AigComputation IO

{-# DEPRECATED canCallABC "Use canCheckSat" #-}
canCallABC :: AigMonad Bool
canCallABC = canCheckSat

-- | Returns literal associated with Boolean value.
litFromBool :: LitMonad m => Bool -> m (MonadLit m)
litFromBool True = litTrue
litFromBool False = litFalse

