{- |
Module           : $Header$
Description      :
License          : BSD3
Stability        : provisional
Point-of-contact : acfoltzer
-}

module Tests.Regressions (regressionTests) where

import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit


import Tests.Common
import Overrides

regressionTests :: Codebase -> TestTree
regressionTests cb = testGroup "Regressions" $
   [
     testCase "one-armed if postdom" $ t1 cb
   , testCase "String class initializer" $ t2 cb
   ]

--------------------------------------------------------------------------------
-- "trivial" tests


-- fixed bad postdominator analysis for one-armed ifs
t1 :: TrivialCase
t1 cb =
  mkSymAssertion $ \sbe ->
    void $ runDefSimulator cb sbe $ do
      jssOverrides
      runStaticMethod (mkClassName "Regressions") "one_armed_if" "()V" []

-- added overrides for native methods called by the String class
-- initializer in Java 7+
-- (see a161eb3efdf4cba716d1a089c810267c47d2492b)
t2 :: TrivialCase
t2 cb =
  mkSymAssertion $ \sbe ->
    void $ runDefSimulator cb sbe $ do
      jssOverrides
      runStaticMethod (mkClassName "Regressions") "use_string" "()V" []
