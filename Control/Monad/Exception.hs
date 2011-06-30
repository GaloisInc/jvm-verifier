{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : matthews
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Exception
  where

import Control.Exception
import Control.Monad.Error
import Control.Monad.State

{-- An error-handling monad class where an error can be any exception.
 -- This allows new exceptions to be defined without having to alter the
 -- type of the monad.
 --}

class Monad m => MonadException m where
  throwExc :: Exception e => e -> m a
  catchExc :: Exception e => m a -> (e -> m a) -> m a

instance Error SomeException

-- | MonadException transformer.
newtype ExcT m a = ExcT { fromExcT :: ErrorT SomeException m a }
  deriving (Functor, Monad, MonadIO, MonadError SomeException, MonadState s, MonadTrans)

runExcT :: ExcT m a -> m (Either SomeException a)
runExcT = runErrorT . fromExcT

instance Monad m => MonadException (ExcT m) where
  throwExc e = ExcT $ throwError (toException e)
  catchExc (ExcT m) hdlr = ExcT $ catchError m hdlr'
    where
      hdlr' e = case fromException e of
                  Nothing -> throwError e
                  Just e' -> fromExcT (hdlr e')

instance MonadException m => MonadException (StateT s m) where
  throwExc e = lift (throwExc e)
  catchExc m hdlr = StateT m'
    where
      m' s = runStateT m s `catchExc` runHdlr s
      runHdlr s e = runStateT (hdlr e) s
