{- |
Module           : $Header$
Description      : Provides monad transformer for writing computations that may
                   fail, but collect multiple errors along the way.
Stability        : provisional
Point-of-contact : jhendrix
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module SAWScript.ErrorPlus
  ( ErrorCollectorT(..)
  , throwError
  , handleIO
  , try
  , both
  , sequenceAll_
  ) where

-- Imports {{{1

import Control.Applicative
import Control.Exception (Exception,handle)
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Data.Maybe
import Data.Monoid

-- ErrorCollectorT {{{1

-- | ErrorColllectorT is a monad transformer that allows errors to be throw,
-- caught, and recovered from (so unlike ErrorCollectorT one can have errors
-- and still be running).
newtype ErrorCollectorT e m v =
  ErrorCollectorT { runErrorCollectorT :: m (e,Maybe v) }

instance Monad m => Functor (ErrorCollectorT e m) where
  fmap fn t = ErrorCollectorT $ return . next =<< runErrorCollectorT t
    where next (e, v) = (e, fmap fn v)

instance (Monoid e, Monad m) => Applicative (ErrorCollectorT e m) where
  pure = return
  (<*>) = ap

instance (Monoid e, Monad m) => Monad (ErrorCollectorT e m) where
  f >>= g = ErrorCollectorT (runErrorCollectorT f >>= next)
   where next (e, Just v)  = runErrorCollectorT (g v) >>= merge e
         next (e, Nothing) = return (e, Nothing)
         merge e (e',r) = return (e `mappend` e', r)
  return v = ErrorCollectorT (return (mempty, Just v))
  fail err = ErrorCollectorT (fail err)

instance (Monoid e, MonadIO m) => MonadIO (ErrorCollectorT e m) where
  liftIO m = ErrorCollectorT (liftIO (fmap (\v -> (mempty, Just v)) m))

instance (Monoid e) => MonadTrans (ErrorCollectorT e) where
  lift m = ErrorCollectorT (next =<< m)
    where next vr = return (mempty, Just vr)

instance (Monoid e, MonadState s m)
      => MonadState s (ErrorCollectorT e m) where
  get   = lift get
  put s = lift (put s)

instance (Monoid e, Monad m) => MonadError e (ErrorCollectorT e m) where
  throwError e = ErrorCollectorT (return (e, Nothing))
  catchError f h = ErrorCollectorT $ runErrorCollectorT f >>= next
   where next (e, Nothing) = runErrorCollectorT (h e)
         next v = return v

-- MonadPlus instance that returns first value that succeeds.
instance (Monoid e, Monad m) => MonadPlus (ErrorCollectorT e m) where
  mzero = ErrorCollectorT (return (mempty, Nothing))
  -- | Return first result that succeeds.
  mplus f g = try f >>= maybe g return

-- | Try running a computation, if it fails return Nothing, otherwise return
-- the value.
try :: Monad m => ErrorCollectorT e m v -> ErrorCollectorT e m (Maybe v)
try m = ErrorCollectorT (next =<< runErrorCollectorT m)
  where next (e,v) = return (e, Just v)

-- | Handler for catching an IO exception and recording it as an error.
handleIO :: (Exception ex, Monoid e, MonadIO m)
         => (ex -> IO e) 
         -> IO a
         -> ErrorCollectorT e m a
handleIO h m = ErrorCollectorT $ liftIO $ 
                handle (\ex -> h ex >>= \e -> return (e,Nothing))
                       (m >>= \r -> return (mempty, Just r))
                      
-- | Runs both computations, returning first value that succeeds.
both :: (Monoid e, Monad m)
     => ErrorCollectorT e m v
     -> ErrorCollectorT e m v
     -> ErrorCollectorT e m v
both f g = do
  mfr <- try f
  case mfr of
    Just fr -> try g >> return fr
    _ -> g

-- | Run each computation and collect errors, even if one fails.  Do not
-- continue later computations if one contains an error
sequenceAll_ :: (Monoid e, Monad m)
             => [ErrorCollectorT e m v] -> ErrorCollectorT e m ()
sequenceAll_ l = do
  res <- mapM try l
  -- Stop if any computation failed.
  when (any isNothing res) $ throwError mempty
