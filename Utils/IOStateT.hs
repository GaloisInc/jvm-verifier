{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Utils.IOStateT
  ( -- * MTL reexports
    MonadIO(..)
  , MonadState(..)
  , MonadTrans(..)
  , gets
  , modify
  -- * CatchMIO rexport
  , CatchMIO(..)
  -- * IOStateT
  , StateT
  , runStateT
  , evalStateT
  , execStateT
  ) where

import Control.Monad.Reader
import Control.Monad.State.Class
import Data.IORef
import Utils.CatchMIO
import IO.Session

newtype StateT s m a = ST { unST :: ReaderT (IORef s) m a }
  deriving ( Monad
           , MonadIO
           , MonadTrans)

instance Monad m => Functor (StateT s m) where
  fmap fn m = return . fn =<< m

instance MonadIO m => MonadState s (StateT s m) where
  get = ST $ ReaderT $ liftIO . readIORef
  put v = v `seq` ST $ ReaderT $ liftIO . flip writeIORef v

instance CatchMIO m => CatchMIO (StateT s m) where
  (ST m) `catchMIO` h = ST $ ReaderT $ \r ->
    runReaderT m r `catchMIO` \e -> runReaderT (unST (h e)) r

instance (SessionMonad m, SessionState s) => SessionMonad (StateT s m) where
  makeSession = do
    Session { toIO = mToIO, finishIO } <- makeSession
    r <- newIORef initialState
    return Session {
        toIO = \(ST (ReaderT m)) -> mToIO (m r)
      , finishIO
      }

-- | Run state computation and return pair containing result and
-- final state.
runStateT :: MonadIO m => StateT s m a -> s -> m (a, s)
runStateT (ST (ReaderT m)) s = do
  r <- liftIO $ newIORef s
  v <- m r
  s' <- liftIO $ readIORef r
  return (v,s')

-- | Run state computation and return result.
evalStateT :: MonadIO m => StateT s m a -> s -> m a
evalStateT m s = runStateT m s >>= \(a,_) -> return a

-- | Run state computation and return final state.
execStateT :: MonadIO m => StateT s m a -> s -> m s
execStateT m s = runStateT m s >>= \(_,s') -> return s'
