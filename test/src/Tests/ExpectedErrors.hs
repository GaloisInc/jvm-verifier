{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies     #-}

module Tests.ExpectedErrors (expErrTests) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Test.Tasty
import Test.Tasty.HUnit

import Tests.Common


expErrTests :: Codebase -> TestTree
expErrTests cb = testGroup "ExpectedErrors" $
  [
    testCase "(-) on single path exception" $ exc1 cb
  , testCase "(-) when all paths raise an exception" $ exc2 cb
  , testCase "(-) symbolic index into array of refs" $ sa1 cb
  , testCase "(-) floats not supported" $ sa2 FloatType cb
  , testCase "(-) doubles not supported" $ sa2 DoubleType cb
  , testCase "(-) symbolic array sizes not supported" $ sa3 cb
  , testCase "(-) update @ symidx into array of refs" $ sa4 cb
  ]

go :: Codebase
   -> Simulator SymbolicMonad IO a
   -> Assertion
go cb a = do
  -- For negative test cases, we don't want to report success of *any* failure,
  -- just the failures that we want to see, so we explicitly fail if any
  -- unexpected exception escapes out of m.
  b <- do 
    oc <- mkOpCache
    withSymbolicMonadState oc $ \sms -> do
      let sbe = symbolicBackend sms
          a' = (void a) `catchSM` h
          h (ErrorPathExc (FailRsn rsn) _) = succeed rsn
          h (UnknownExc (Just (FailRsn rsn))) = succeed rsn
          h _ = liftIO $ assertFailure "unknown exception"
      runDefSimulator cb sbe a'
  assert b
  where
    succeed _msg = return ()

--------------------------------------------------------------------------------
-- Exception (negative) tests

-- | Negative test case: fail on single path exception
exc1 :: TrivialCase
exc1 cb = go cb . void $ runStaticMethod "Trivial" "always_throws" "()V" []

-- | Negative test case: expect fail when all paths raise an exception
exc2 :: TrivialCase
exc2 cb = go cb $ do
  sbe <- use backend
  b <- liftIO $ freshInt sbe
  void $ runStaticMethod "Errors" "allPathsThrowExc" "(Z)V" [IValue b]

--------------------------------------------------------------------------------
-- Array (negative) tests

-- -- | Expected to fail: Symbolic lookup in array of refs
sa1 :: TrivialCase
sa1 cb = go cb $ do
  sbe <- use backend
  symIdx <- liftIO $ IValue <$> freshInt sbe
  arr <- newMultiArray (ArrayType intArrayTy) [mkCInt 32 1, mkCInt 32 1]
  [(_, Just (RValue r))] <-
    runStaticMethod "Errors" "getArrayRef" "(I[[I)[I"
      [symIdx , RValue arr]
  getIntArray r

-- | Expected to fail: arrays with given element type are not supported
sa2 :: Type -> TrivialCase
sa2 ety cb = go cb $ newMultiArray (ArrayType ety) [mkCInt 32 1]

-- | Expected to fail: arrays with symbolic size are not supported
sa3 :: TrivialCase
sa3 cb = go cb $ do
  sbe <- use backend
  v <- liftIO $ freshInt sbe
  newMultiArray intArrayTy [v]

-- | Expected to fail: update an array of refs at a symbolic index
sa4 :: TrivialCase
sa4 cb = go cb $ do
  sbe <- use backend
  symIdx <- liftIO $ IValue <$> freshInt sbe
  arr <- newMultiArray (ArrayType intArrayTy) [mkCInt 32 1, mkCInt 32 1]
  elm <- newIntArray intArrayTy [mkCInt 32 1]
  [(_pd, Nothing)] <-
    runStaticMethod "Errors" "updArrayRef" "(I[I[[I)V"
      [symIdx, RValue elm, RValue arr]
  return ()

--------------------------------------------------------------------------------
-- Scratch

-- _ignore_nouse :: a
-- _ignore_nouse = undefined main

--main :: IO ()
--main = do cb <- commonLoadCB
--          defaultMain [expErrTests cb]
