{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix
-}

module Utils.LogMonad (LogMonad(..)) where

import Control.Monad
import Utils.IOStateT


class Monad m => LogMonad m where
  -- | Gets verbosity of logging
  getVerbosity :: m Int
  -- | Set verbosity of logging
  setVerbosity :: Int -> m ()
  -- | Execute term computation with given verbosity.
  withVerbosity :: Int -> m a -> m a
  withVerbosity v m = do
    old <- getVerbosity
    setVerbosity v
    res <- m
    setVerbosity old
    return res
  -- | Execute computation when verbosity has specific level.
  whenVerbosity :: (Int -> Bool) -> m () -> m ()
  whenVerbosity f act = do
    v <- getVerbosity
    when (f v) act

instance (Monad m, LogMonad m) => LogMonad (StateT s m) where
  getVerbosity = lift getVerbosity
  setVerbosity = lift . setVerbosity


