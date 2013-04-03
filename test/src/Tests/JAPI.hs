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

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.Common
import Overrides

-- Test names match those found in PrimOps where possible.

-- NB: It's possible to run these tests individually from the JAPI class
-- bytecode, e.g. jss --classpath=... --opts="outarr" JAPI

main :: IO ()
main = do cb <- commonLoadCB
          defaultMain [japiTests cb]

japiTests :: Codebase -> Test
japiTests cb = testGroup "JAPI" $
  [ testCase "JAPI: simple byte test" (doTest "simpleByte" cb)
  , testCase "JAPI: 32b symint add" (doTest "t2" cb)
  , testCase "JAPI: 64b symlong add" (doTest "t8" cb)
  , testCase "JAPI: symbolic int array idx/upd" (doTest "tarr1" cb)
  , testCase "JAPI: symbolic long array idx/upd" (doTest "tarr2" cb)
  , testCase "JAPI: symbolic long array sum" (doTest "t9" cb)
  , testCase "JAPI: symbolic byte array product" (doTest "byteProd" cb)
  , testCase "JAPI: symbolic array out param" (doTest "outArr" cb)
  ]

mkTestArgs :: String -> Simulator SymbolicMonad IO [Value DagTerm]
mkTestArgs tn = do
  args <- newMultiArray (ArrayType (ClassType "java/lang/String")) [mkCInt 32 1]
  setArrayValue args (mkCInt 32 0) =<< (RValue <$> refFromString tn)
  return [RValue args]

doTest :: String -> TrivialCase
doTest tn cb =
  mkSymAssertion $ \sbe -> do
    rs <- runDefSimulator cb sbe $ do
      jssOverrides
      runMain "JAPI" =<< mkTestArgs tn
    length rs @?= 1

-- --------------------------------------------------------------------------------
-- -- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main
