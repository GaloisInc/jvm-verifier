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
  ( ErrorT(..)
  , runErrorT
  , throwError
  , ErrorPlusT(..)
  , runErrorPlusT
  ) where

-- Imports {{{1

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Data.Monoid

-- ErrorPlusT {{{1 

newtype ErrorPlusT e m v = ErrorPlusT { runErrorPlusT :: m (Either e v) }

instance Functor m => Functor (ErrorPlusT e m) where
  fmap fn t = ErrorPlusT (fmap (either Left (Right . fn)) (runErrorPlusT t))

instance (Monoid e, Applicative m) => Applicative (ErrorPlusT e m) where
  pure v = ErrorPlusT (pure (Right v))
  tf <*> tv = ErrorPlusT $ liftA2 merge (runErrorPlusT tf) (runErrorPlusT tv)
   where merge (Left ef) (Left ev) = Left (ef `mappend` ev)
         merge (Left ef) (Right _) = Left ef
         merge (Right _) (Left ev) = Left ev
         merge (Right f) (Right v) = Right (f v)

instance MonadTrans (ErrorPlusT e) where
  lift m = ErrorPlusT (return . Right =<< m)

-- ErrorT {{{1

-- | ErrorT definition (Unlike MTL, does not require error implement Error
-- class).
newtype ErrorT e m v = ErrorT { runErrorT :: m (Either e v) }

instance Monad m => Functor (ErrorT e m) where
  fmap fn t = ErrorT $ return . next =<< runErrorT t
    where next (Right v) = Right (fn v)
          next (Left e) = (Left e)

instance Monad m => Applicative (ErrorT e m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ErrorT e m) where
  f >>= g = ErrorT (runErrorT f >>= next)
   where next (Right v) = runErrorT (g v)
         next (Left e)  = return (Left e)
  return v = ErrorT (return (Right v))
  fail err = ErrorT (fail err)

instance MonadIO m => MonadIO (ErrorT e m) where
  liftIO m = ErrorT (return . Right =<< liftIO m)

instance MonadState s m => MonadState s (ErrorT e m) where
  get = ErrorT (return . Right =<< get)
  put s = ErrorT (put s >> return (Right ()))

instance Monad m => MonadError e (ErrorT e m) where
  throwError e = ErrorT (return (Left e))
  catchError f h = ErrorT $ runErrorT f >>= next
   where next (Left  e) = runErrorT (h e)
         next v = return v
