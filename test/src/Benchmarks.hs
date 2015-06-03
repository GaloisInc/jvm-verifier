{- |
Module           : $Header$
Description      : various performance benchmarks for JSS
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : acfoltzer
-}

module Main where

import Control.Monad

import Criterion.Main

import Tests.Common
import Tests.PathStateMerges
import Tests.RC564

main :: IO ()
main = do cb <- commonLoadCB
          -- run each sim once so lazy initialization doesn't pollute result
          mul2WithFlags cb bitblastFlags
          simRC564 cb defaultSimFlags
          defaultMain [ mul2s cb, rc564s cb ]

mul2s :: Codebase -> Benchmark
mul2s cb = bgroup "mul2" $ [ bench "bitblast" $ mul2WithFlags cb bitblastFlags
                           , bench "sat" $ mul2WithFlags cb satFlags
                           ]

rc564s :: Codebase -> Benchmark
rc564s cb = bgroup "rc564" $ [ bench "default" $ simRC564 cb defaultSimFlags
                             , bench "bitblast" $ simRC564 cb bitblastFlags
                             , bench "sat" $ simRC564 cb satFlags
                             ]

simRC564 :: Codebase -> SimulationFlags -> IO ()
simRC564 cb flags = do
  oc <- mkOpCache
  void . withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms
    keyVars <- replicateM 16 $ freshByte sbe
    inpVars <- replicateM 16 $ freshByte sbe
    runSimulator cb sbe defaultSEH (Just flags) $ runRC564 keyVars inpVars
