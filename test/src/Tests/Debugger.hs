{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : acfoltzer
-}

module Tests.Debugger (debuggerTests) where

import Verifier.Java.Debugger

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck as QC hiding ((.&.))
import Test.QuickCheck.Monadic as QC

import Tests.Common

main :: IO ()
main = do cb <- commonLoadCB
          defaultMain [debuggerTests cb]

debuggerTests :: Codebase -> Test
debuggerTests cb = testGroup "Debugger" $
  [
    testCase "tbp" $ tbp cb
  , testCase "tmain" $ tmain cb
  ]

-- | Test entry and line number breakpoints for arbitrary methods
tbp :: TrivialCase
tbp cb =
  mkSymAssertion $ \sbe -> do
    sym <- freshInt sbe
    let clName = "Trivial"
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
    let clName = "IVTDriver"
        mKey   = mainKey
        dbgSEH = defaultSEH { onPreStep = breakpointLogger }
    _ <- runSimulator cb sbe dbgSEH Nothing $ do
      breakOnMain clName
      runStaticMethod clName (methodKeyName mKey) (unparseMethodDescriptor mKey) [IValue sym]
    return ()
