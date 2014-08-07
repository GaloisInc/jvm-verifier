{-# LANGUAGE Rank2Types #-}
module Main where

import Test.Framework

import Tests.Common
import Tests.Arrays
import Tests.Debugger
import Tests.ExpectedErrors
import Tests.PathStateMerges
import Tests.PrimOps
import Tests.RC564
import Tests.SHA384
import Tests.JAPI
import Tests.Regressions

main :: IO ()
main = do cb <- commonLoadCB
          defaultMain (allTests cb)

allTests :: Codebase -> [Test]
allTests cb =
      [ primOpTests cb
      , arrayTests cb
      , psmsTests cb
      , expErrTests cb
      , rc564Tests
      , sha384Tests
      , japiTests cb
      , debuggerTests cb
      , regressionTests cb
      ]
