{-# LANGUAGE CPP #-}
{- |
Module           : Backends
Description      : The command line driver for the Java Symbolic Simulator
License          : BSD3
Stability        : stable
Point-of-contact : atomb
-}

module Backends where

import Control.Exception (bracket)

#if USE_BUILTIN_ABC
import qualified Data.ABC as ABC
import Verinf.Symbolic.Lit.ABC
#else
import qualified Data.AIG as AIG
#endif
import Verifier.Java.Simulator
import qualified Verifier.Java.WordBackend as W
import qualified Verifier.Java.SAWBackend as S
import Verinf.Symbolic

withFreshSAWBackend :: (Backend S.SharedContext -> IO a) -> IO a
withFreshSAWBackend f = do
  sc <- S.mkSharedContext
  S.scLoadPreludeModule sc
  S.scLoadCryptolModule sc
  S.scLoadJavaModule sc
#if USE_BUILTIN_ABC
  f =<< S.sawBackend sc Nothing ABC.giaNetwork
#else
  f =<< S.sawBackend sc Nothing AIG.basicProxy
#endif

#if USE_BUILTIN_ABC
-- | Create a fresh symbolic backend with a new op cache, and execute it.
withFreshWordBackend :: (Backend W.SymbolicMonad -> IO a) -> IO a
withFreshWordBackend f = do
  oc <- W.mkOpCache
  withBitEngine $ \be -> do
    de <- mkConstantFoldingDagEngine
    sms <- W.mkSymbolicMonadState oc be de
    f (W.symbolicBackend sms)

withBitEngine :: (BitEngine Lit -> IO a) -> IO a
withBitEngine = bracket createBitEngine beFree

withSymbolicMonadState :: OpCache -> (W.SymbolicMonadState Lit -> IO a) -> IO a
withSymbolicMonadState oc f =
  withBitEngine $ \be -> do
    de <- mkConstantFoldingDagEngine
    f =<< W.mkSymbolicMonadState oc be de
#endif
