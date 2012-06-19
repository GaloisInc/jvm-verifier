{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies     #-}

module Tests.ExpectedErrors (expErrTests) where

import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad.State
import Data.Typeable
import System.IO
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

import JavaParser.Common
import Simulation hiding (run)
import qualified Simulation as Sim
import Tests.Common
import Utils
import Verinf.Symbolic

-- NB: REVISIT: The poorly named "runNegTest" is what grounds to the default
-- symbolic backend; we'll want better names when we support multiple symbolic
-- backends (runNegTestSymWordRep or somesuch, perhaps, etc.)

expErrTests :: [(Args, Property)]
expErrTests =
  [
    test1Neg exc1             "(-) on single path exception"
  , test1Neg exc2             "(-) when all paths raise an exception"
  , test1Neg sa1              "(-) symbolic index into array of refs"
  , test1Neg (sa2 FloatType)  "(-) floats not supported"
  , test1Neg (sa2 DoubleType) "(-) doubles not supported"
  , test1Neg sa3              "(-) symbolic array sizes not supported"
  , test1Neg sa4              "(-) update @ symidx into array of refs"
  ]

-- NB: One can set verb > 0 to see the error output (stack traces, error
-- messages, etc.)  during execution of the tests.
verb :: Int
verb = 0

go :: Codebase
   -> Simulator SymbolicMonad a
   -> PropertyM IO ()
go cb act = do
  -- For negative test cases, we don't want to report success of *any* failure,
  -- just the failures that we want to see, so we explicitly fail if any
  -- unexpected exception escapes out of m.
  eea <- run $ do
   oc <- mkOpCache
   CE.try $ withSymbolicMonadState oc $ \sms ->  do
     let sbe = symbolicBackend sms 
     _ <- runDefSimulator sbe cb $ Sim.withVerbosity verb $ act
     return [False]
  case eea of
    Left (e :: CE.SomeException) ->
      case CE.fromException e of
        Just (SymExtErr msg v) -> succeed msg v
        _ -> let h :: (Show t, Typeable t, t ~ MonadTerm SymbolicMonad) =>
                      Maybe (SimulatorExc t)
                 h = CE.fromException e
             in case h of
                  Just (SimExtErr msg v _) -> succeed msg v
                  _ -> emitErr e
    Right chks -> assert (all not chks)
  where
    succeed msg v | v > 0     = run (putStrLn msg) >> assert False
                  | otherwise = assert False
    emitErr e = do
      run $ mapM_ (hPutStrLn stderr)
              [ "vvv Test witnessed unexpected exception vvv"
              , show e
              , "^^^ Test witnessed unexpected exception ^^^"
              ]
      assert True -- fail

--------------------------------------------------------------------------------
-- Exception (negative) tests

-- | Negative test case: fail on single path exception
exc1 :: TrivialProp
exc1 cb = go cb $ runStaticMethod_ "Trivial" "always_throws" "()V" []

-- | Negative test case: expect fail when all paths raise an exception
exc2 :: TrivialProp
exc2 cb = go cb $ do
  sbe <- gets backend
  b <- liftIO $ freshInt sbe
  runStaticMethod_ "Errors" "allPathsThrowExc" "(Z)V" [IValue b]

--------------------------------------------------------------------------------
-- Array (negative) tests

-- -- | Expected to fail: Symbolic lookup in array of refs
sa1 :: TrivialProp
sa1 cb = go cb $ do
  sbe <- gets backend
  symIdx <- liftIO $ IValue <$> freshInt sbe
  arr <- newMultiArray (ArrayType intArrayTy) [mkCInt 32 1, mkCInt 32 1]
  [(pd, ReturnVal (RValue r))] <-
    withoutExceptions
    <$> runStaticMethod "Errors" "getArrayRef" "(I[[I)[I"
          [symIdx , RValue arr]
  getIntArray pd r

-- | Expected to fail: arrays with given element type are not supported
sa2 :: Type -> TrivialProp
sa2 ety cb = go cb $ newMultiArray (ArrayType ety) [mkCInt 32 1]

-- | Expected to fail: arrays with symbolic size are not supported
sa3 :: TrivialProp
sa3 cb = go cb $ do
  sbe <- gets backend
  v <- liftIO $ freshInt sbe
  newMultiArray intArrayTy [v]

-- | Expected to fail: update an array of refs at a symbolic index
sa4 :: TrivialProp
sa4 cb = go cb $ do
  sbe <- gets backend
  symIdx <- liftIO $ IValue <$> freshInt sbe
  arr <- newMultiArray (ArrayType intArrayTy) [mkCInt (Wx 32) 1, mkCInt (Wx 32) 1]
  elm <- newIntArray intArrayTy [mkCInt (Wx 32) 1]
  [(_pd, Terminated)] <-
    withoutExceptions
    <$> runStaticMethod "Errors" "updArrayRef" "(I[I[[I)V"
          [symIdx, RValue elm, RValue arr]
  return ()

--------------------------------------------------------------------------------
-- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests expErrTests
