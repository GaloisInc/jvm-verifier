{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley, matthews
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Utils.CatchMIO where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Prelude hiding (catch)

-- | Types in the CatchMIO typeclass allow propagation of CE.Exception values
-- via a MonadIO implementation, so we can catch exceptions raised via CE.throw
-- and IO exceptions.
class (MonadIO m) => CatchMIO m where
  catchMIO :: Exception e => m a -> (e -> m a) -> m a

instance CatchMIO IO where catchMIO = catch

-- | Throw an exception from a MonadIO.
throwMIO :: (Exception e, MonadIO m) => e -> m a
throwMIO = liftIO . throwIO

-- | Note: @catchMIO m@ on the StateT type will reset the state to whatever
-- it was at the start of running @m@.
instance CatchMIO m => CatchMIO (ReaderT s m) where
  catchMIO m h = ReaderT $ \s ->
    runReaderT m s `catchMIO` \e -> runReaderT (h e) s

-- | Note: @catchMIO m@ on the StateT type will reset the state to whatever
-- it was at the start of running @m@.
instance CatchMIO m => CatchMIO (StateT s m) where
  catchMIO m hdlr = StateT m'
    where
      m' s = runStateT m s `catchMIO` \e -> runStateT (hdlr e) s

finallyMIO :: CatchMIO m => m a -> m b -> m a
finallyMIO c f = do
  res <- catchMIO c (\e -> f >> throwMIO (e :: SomeException))
  _ <- f
  return res
