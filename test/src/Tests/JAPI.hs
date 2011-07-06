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
import Test.QuickCheck

import JavaParser
import Simulation hiding (run)
import Tests.Common
import Utils

import Verinf.Symbolic
import Verinf.Utils.CatchMIO

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
--  , test1 (doTest "dbugEvalTest") "JAPI: Symbolic.Debug.dbugEval test"
  ]

mkTestArgs :: (ConstantInjection (JSInt m), JavaSemantics m) =>
              String -> m [JSValue m]
mkTestArgs tn = do
  args <- newMultiArray (ArrayType (ClassType "java/lang/String")) [mkCInt (Wx 32) 1]
  setArrayValue args (mkCInt (Wx 32) 0) =<< (RValue <$> refFromString tn)
  return [RValue args]

doTest :: String -> TrivialProp
doTest tn cb = runTest (go `catchMIO` simExcHndlr failMsg)
  -- ^ NB: It's the type of runTest that forces use of SymbolicMonad here;
  -- simExcHndlr is polymorphic in the symbolic backend
  where
    failMsg = "Unexpected error caught in JAPI test: " ++ tn
    go = runSimulator cb $ do
           rs <- runMain "JAPI" =<< mkTestArgs tn
           CE.assert (length rs == 1) $ return ()
           return [True]

-- --------------------------------------------------------------------------------
-- -- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests japiTests
