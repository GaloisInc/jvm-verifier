{- |
Module           : Tests.Debugger
Description      :
License          : BSD3
Stability        : provisional
Point-of-contact : acfoltzer
-}

module Tests.Debugger (debuggerTests) where

import Verifier.Java.Debugger

import Test.Tasty
import Test.Tasty.HUnit

import Tests.Common

--debuggerTestMain :: IO ()
--debuggerTestMain =
--  do cb <- commonLoadCB
--     defaultMain [debuggerTests cb]


debuggerTests :: Codebase -> TestTree
debuggerTests cb = testGroup "Debugger" $
  [
    testCase "tbp" $ tbp cb
  , testCase "tmain" $ tmain cb
-- FIXME, what should this test do?
--  , testCase "trepl" $ trepl cb
  ]

-- | Test entry and line number breakpoints for arbitrary methods
tbp :: TrivialCase
tbp cb =
  mkSymAssertion $ \sbe -> do
    sym <- freshInt sbe
    let clName = mkClassName "Trivial"
        mName  = "long_array_idx_f7"
        mType  = "(I)J"
        mKey   = makeMethodKey mName mType
        dbgSEH = defaultSEH { onPreStep = breakpointLogger }
    _ <- runSimulator cb sbe dbgSEH Nothing $ do
      addBreakpoint clName mKey BreakEntry
      addBreakpoint clName mKey (BreakLineNum 125)
      runStaticMethod clName mName mType [IValue sym]
    return ()

-- | Test break on main
tmain :: TrivialCase
tmain cb =
  mkSymAssertion $ \sbe -> do
    sym <- freshInt sbe
    let clName = mkClassName "IVTDriver"
        mKey   = mainKey
        dbgSEH = defaultSEH { onPreStep = breakpointLogger }
    _ <- runSimulator cb sbe dbgSEH Nothing $ do
      breakOnMain clName
      runStaticMethod clName (methodKeyName mKey) (unparseMethodDescriptor mKey) [IValue sym]
    return ()

{-
-- | Temporary: go into repl manually. TODO: feed repl a script
trepl :: TrivialCase
trepl cb =
  mkSymAssertion $ \sbe -> do
    sym <- freshInt sbe
    let clName = "IVTDriver"
        mKey   = mainKey
        dbgSEH = defaultSEH { onPreStep = runAtBreakpoints debuggerREPL }
    _ <- runSimulator cb sbe dbgSEH Nothing $ do
      breakOnMain clName
      runStaticMethod clName (methodKeyName mKey) (unparseMethodDescriptor mKey) [IValue sym]
    return ()
-}
