{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

module Tests.JAPI(japiTests) where

import Control.Applicative
import qualified Control.Exception as CE
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.Common
import Overrides

-- Test names match those found in PrimOps where possible.

-- NB: It's possible to run these tests individually from the JAPI class
-- bytecode, e.g. jss --classpath=... --opts="outarr" JAPI

japiTests :: [(Args, Property)]
japiTests =
  [ test1 (doTest "simpleByte")   "JAPI: simple byte test"
  , test1 (doTest "t2")           "JAPI: 32b symint add"
  , test1 (doTest "t8")           "JAPI: 64b symlong add"
  , test1 (doTest "tarr1")        "JAPI: symbolic int array idx/upd"
  , test1 (doTest "tarr2")        "JAPI: symbolic long array idx/upd"
  , test1 (doTest "t9")           "JAPI: symbolic long array sum"
  , test1 (doTest "byteProd")     "JAPI: symbolic byte array product"
  , test1 (doTest "outArr")       "JAPI: symbolic array out param"
  ]

mkTestArgs :: String -> Simulator SymbolicMonad IO [Value DagTerm]
mkTestArgs tn = do
  args <- newMultiArray (ArrayType (ClassType "java/lang/String")) [mkCInt 32 1]
  setArrayValue args (mkCInt 32 0) =<< (RValue <$> refFromString tn)
  return [RValue args]

doTest :: String -> TrivialProp
doTest tn cb =
  runSymTest $ \sbe -> do
    runDefSimulator cb sbe $ do
      jssOverrides
      rs <- runMain "JAPI" =<< mkTestArgs tn
      CE.assert (length rs == 1) $ return [True]
      
-- --------------------------------------------------------------------------------
-- -- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests japiTests
