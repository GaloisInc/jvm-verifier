{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Tests.ExpectedErrors (expErrTests) where

import Control.Applicative
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

import JavaParser.Common
import Simulation hiding (run)
import qualified Simulation as Sim
import Tests.Common
import Utils
import Utils.Simulation

import Verinf.Symbolic

-- NB: REVISIT: The poorly named "runNegTest" is what grounds to the default
-- symbolic backend; we'll want better names when we support multiple symbolic
-- backends (runNegTestSymWordRep or somesuch, perhaps, etc.)

expErrTests :: [(Args, Property)]
expErrTests =
  [
    test1Neg (exc1 runNegTest)           "(-) on single path exception"
  , test1Neg (exc2 runNegTest)           "(-) when all paths raise an exception"
  , test1Neg (sa1 runNegTest)            "(-) symbolic index into array of refs"
  , test1Neg (sa2 runNegTest FloatType)  "(-) floats not supported"
  , test1Neg (sa2 runNegTest DoubleType) "(-) doubles not supported"
  , test1Neg (sa3 runNegTest)            "(-) symbolic array sizes not supported"
  , test1Neg (sa4 runNegTest)            "(-) update @ symidx into array of refs"
  --, test1Neg (sy1 runNegTest)            "(-) symbolic backend: no bitblasting uninterpreted input vars"
  ]

-- NB: One can set verb > 0 to see the error output (stack traces, error
-- messages, etc.)  during execution of the tests.
verb :: Int
verb = 0

type RunTest sym = sym [Bool] -> PropertyM IO ()

go ::
  ( AigOps sym
  )
  => RunTest sym
  -> Codebase
  -> Simulator sym a
  -> PropertyM IO ()
go rt cb act =
  rt $ runSimulator cb . Sim.withVerbosity verb $ act >> return [False]

--------------------------------------------------------------------------------
-- Exception (negative) tests

-- | Negative test case: fail on single path exception
exc1 ::
  ( AigOps sym
  )
  => RunTest sym -> TrivialProp
exc1 rt cb = go rt cb $ runStaticMethod_ "Trivial" "always_throws" "()V" []

-- | Negative test case: expect fail when all paths raise an exception
exc2 ::
  ( AigOps sym
  )
  => RunTest sym -> TrivialProp
exc2 rt cb = go rt cb $ do
  b <- liftSymbolic freshInt
  runStaticMethod_ "Errors" "allPathsThrowExc" "(Z)V" [IValue b]

--------------------------------------------------------------------------------
-- Array (negative) tests

-- -- | Expected to fail: Symbolic lookup in array of refs
sa1 ::
  ( AigOps sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym -> TrivialProp
sa1 rt cb = go rt cb $ do
  symIdx <- liftSymbolic $ IValue <$> freshInt
  arr <- newMultiArray (ArrayType intArrayTy) [mkCInt (Wx 32) 1, mkCInt (Wx 32) 1]
  [(pd, ReturnVal (RValue r))] <-
    withoutExceptions
    <$> runStaticMethod "Errors" "getArrayRef" "(I[[I)[I"
          [symIdx , RValue arr]
  getIntArray pd r

-- | Expected to fail: arrays with given element type are not supported
sa2 ::
  ( AigOps sym
  )
  => RunTest sym -> Type -> TrivialProp
sa2 rt ety cb = go rt cb $ newMultiArray (ArrayType ety) [mkCInt (Wx 32) 1]

-- | Expected to fail: arrays with symbolic size are not supported
sa3 ::
  ( AigOps sym
  )
  => RunTest sym -> TrivialProp
sa3 rt cb = go rt cb $ liftSymbolic freshInt >>= newMultiArray intArrayTy . (:[])

-- | Expected to fail: update an array of refs at a symbolic index
sa4 :: (AigOps sym) => RunTest sym -> TrivialProp
sa4 rt cb = go rt cb $ do
  symIdx <- liftSymbolic $ IValue <$> freshInt
  arr <- newMultiArray (ArrayType intArrayTy) [mkCInt (Wx 32) 1, mkCInt (Wx 32) 1]
  elm <- newIntArray intArrayTy [mkCInt (Wx 32) 1]
  [(_pd, Terminated)] <-
    withoutExceptions
    <$> runStaticMethod "Errors" "updArrayRef" "(I[I[[I)V"
          [symIdx, RValue elm, RValue arr]
  return ()

--------------------------------------------------------------------------------
-- Symbolic (negative) tests

{-
sy1 :: (AigOps sym) => RunTest sym -> TrivialProp
sy1 rt cb = rt $ do
  uv <- IValue <$> freshUninterpretedVar (SymInt (constantWidth 32))
  outVar <- runSimulator cb $ do
    setVerbosity verb
    liftSymbolic $ setVerbosity verb
    arr <- newIntArray intArrayTy [mkCInt (Wx 32) 0]
    Right rslt <- snd . takeIntRslt . withoutExceptions
                  <$> runStaticMethod "Arrays" "index" "(I[I)I"
                        [uv, RValue arr]
    return rslt
  s <- getVarLit outVar
  -- fail if we've not thrown an exception by now
  s `seq` return [False]
  -}

--------------------------------------------------------------------------------
-- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests expErrTests
