{-# LANGUAGE Rank2Types #-}
{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : atomb
-}
module Main where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Runners.AntXML


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
          defaultMainWithIngredients ingrs (allTests cb)

ingrs :: [Ingredient]
ingrs =
   [ antXMLRunner
   ]
   ++
   defaultIngredients

allTests :: Codebase -> TestTree
allTests cb =
    testGroup "Java"
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
