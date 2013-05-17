{-# LANGUAGE Rank2Types #-}
module Main where

import Control.Monad
import System.Environment(getArgs)
import System.Exit
import Test.Framework

import Tests.Common
import Tests.Arrays
import Tests.Debugger
import Tests.ExampleErrorSession
import Tests.ExampleExceptionSession
import Tests.ExpectedErrors
import Tests.PathStateMerges
import Tests.PrimOps
import Tests.RC564
import Tests.SBVParser
import Tests.SHA384
import Tests.JAPI
import Tests.PrettyPrint
import Tests.RuleChecker
import Tests.Regressions

main :: IO ()
main = do cb <- commonLoadCB
          defaultMain (allTests cb)

allTests :: Codebase -> [Test]
allTests cb =
      [ exampleExceptionSessionTests
      , exampleErrorSessionTests
      , sbvParserTests
      , primOpTests cb
      , arrayTests cb
      , psmsTests cb
      , expErrTests cb
      , rc564Tests
      , sha384Tests
      , japiTests cb
      , prettyPrintTests
      , ruleCheckerTests
      , debuggerTests cb
      , regressionTests cb
      ]
