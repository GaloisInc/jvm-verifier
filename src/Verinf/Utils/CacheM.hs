{- |
Module           : $Header$
Description      :
License          : BSD3
Stability        : provisional
Point-of-contact : jhendrix
-}
module Verinf.Utils.CacheM (Cache, CacheM(..)) where

import qualified Data.Map as Map
import Data.IORef
import Control.Monad.Trans
import Control.Monad.State as MTLL

newtype Cache a b = C (IORef (Map.Map a b))

class Monad m => CacheM m where
  newCache    :: m (Cache a b)
  lookupCache :: Ord a => Cache a b -> a -> m (Maybe b)
  updateCache :: Ord a => Cache a b -> a -> b -> m ()

instance CacheM IO where
  newCache              = C `fmap` newIORef Map.empty
  lookupCache (C r) a   = Map.lookup a `fmap` readIORef r
  updateCache (C r) a b = do mp <- readIORef r
                             writeIORef r $! Map.insert a b mp


instance CacheM m => CacheM (MTLL.StateT s m) where
  newCache = lift newCache
  lookupCache c k   = lift (lookupCache c k)
  updateCache c k v = lift (updateCache c k v) 

