{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Tests.Common where

import Control.Monad
import qualified Control.Exception as CE
import Data.Int
import Data.Typeable
import Prelude hiding (catch)
import System.IO
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Test as T

import Execution.Codebase
import Verifier.Java.Backend
import Simulation (SimulatorExc(..), liftIO)

import Verinf.Symbolic

assertMsg :: Bool -> String -> PropertyM IO ()
assertMsg b s = when (not b) (run $ putStrLn s) >> assert b

int32s :: Int -> Gen [Int32]
int32s n = choose (1,n) >>= (`vectorOf` (arbitrary :: Gen Int32))

bytes :: Int -> Gen String
bytes n =
  vectorOf (n*2)
  $ arbitrary `suchThat` (\c -> c `elem` (['0'..'9'] ++ ['a'..'f']))

type TrivialProp = Codebase -> PropertyM IO ()
newtype FailMsg  = FailMsg String
instance Show FailMsg where show (FailMsg s) = s

test1 :: (Codebase -> PropertyM IO ()) -> String -> (Args, Property)
test1 = test 1 False

test1Neg :: (Codebase -> PropertyM IO ()) -> String -> (Args, Property)
test1Neg = test 1 True

test :: Int -> Bool -> (Codebase -> PropertyM IO ()) -> String -> (Args, Property)
test n shouldFail f desc =
  ( stdArgs{ maxSuccess = n }
  , label desc $ handleNeg $ monadicIO $ withFailMsg $ commonCB >>= f
  )
  where
    handleNeg   = if shouldFail then expectFailure else id
    withFailMsg = if not shouldFail then (forAllM (return msg) . const) else id
    msg         = FailMsg $ "Test failed: '" ++ desc ++ "'"

commonJars :: [String]
commonJars = [ "support/galois.jar"        -- primitives & symbolic API
             , "jdk1.6/classes.jar"        -- jdk
             , "user/bcprov-jdk16-145.jar" -- bouncy castle
             ]

commonClassPaths :: [String]
commonClassPaths = ["test/src/support"]

commonCB :: PropertyM IO Codebase
commonCB = run commonLoadCB

commonLoadCB :: IO Codebase
commonLoadCB = loadCodebase commonJars commonClassPaths

runTest :: SymbolicMonad [Bool] -> PropertyM IO ()
runTest m = run (mkOpCache >>= \oc -> runSymbolic oc m) >>= mapM_ assert

runSymTest :: (Backend SymbolicMonad -> IO [Bool]) -> PropertyM IO ()
runSymTest m = runTest (getBackend >>= \sbe -> liftIO (m sbe))

runNegTest :: SymbolicMonad [Bool] -> PropertyM IO ()
runNegTest m = do
  -- For negative test cases, we don't want to report success of *any* failure,
  -- just the failures that we want to see, so we explicitly fail if any
  -- unexpected exception escapes out of m.
  eea <- run $ do
   oc <- mkOpCache
   CE.try (runSymbolic oc m)
  case eea of
    Left (e :: CE.SomeException) ->
      case CE.fromException e of
        Just (SymExtErr msg v) -> succeed msg v
        _ -> let h :: (Show t, Typeable t, t ~ MonadTerm SymbolicMonad) =>
                      Maybe (SimulatorExc t)
                 h = CE.fromException e
             in
               case h of
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

runTests :: [(Args, Property)] -> IO ()
runTests tests = do
  results <- mapM (uncurry quickCheckWithResult) tests
  if all T.isSuccess results
   then do
     putStrLn "All tests successful."
   else do
     putStrLn "One or more tests failed."

--------------------------------------------------------------------------------
-- Misc instances & instance helpers

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR (a,b) g =
  case randomR (fromIntegral a :: Integer, fromIntegral b :: Integer) g of
    (x, g') -> (fromIntegral x, g')
