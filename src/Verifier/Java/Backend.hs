{-# LANGUAGE ImplicitParams #-}
module Verifier.Java.Backend where

import Data.IORef
import qualified Data.Map as Map
import Verinf.Symbolic.Common
import Verinf.Symbolic
import Verinf.Symbolic.Lit.Functional
import qualified Data.Vector.Storable as SV

type BackendTerm sym = MonadTerm sym
type BackendLong sym = MonadTerm sym

data Backend sym = Backend {
         freshLong :: IO (BackendLong sym)
       }
                   
symbolicBackend :: SymbolicMonadState -> Backend SymbolicMonad
symbolicBackend sms =
    Backend {
        freshLong = do
          lv <- SV.replicateM 64 lMkInput
          n@(InputTerm i _) <- deFreshInput (smsDagEngine sms) int64Type
          do let lr = smsInputLitRef sms      
             m <- readIORef lr
             writeIORef lr $! (Map.insert i (LV lv) m)
          return n
      }
  where ?be = smsBitEngine sms