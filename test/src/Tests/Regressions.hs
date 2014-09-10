{- |
Module           : $Header$
Description      :
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
   ]

--------------------------------------------------------------------------------
-- "trivial" tests


-- fixed bad postdominator analysis for one-armed ifs
t1 :: TrivialCase
t1 cb =
  mkSymAssertion $ \sbe ->
    void $ runDefSimulator cb sbe $ do
      jssOverrides
      runStaticMethod "Regressions" "one_armed_if" "()V" []

