{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}
module Tests.Common 
  ( module Tests.Common
  , module Language.JVM.Parser
  , module Verifier.Java.Simulator
  , module Verifier.Java.Utils
  , module Verifier.Java.WordBackend
  , BitEngine(..)
  ) where

import Control.Monad
import Data.Int
import Prelude hiding (catch)
import System.FilePath
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Test as T

import Language.JVM.Parser
import Verifier.Java.Simulator hiding (run, assert)
import Verifier.Java.Utils
import Verifier.Java.WordBackend

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
commonJars = [ "support" </> "galois.jar"        -- primitives & symbolic API
             , "jdk1.6" </> "classes.jar"        -- jdk
             , "user" </> "bcprov-jdk16-145.jar" -- bouncy castle
             ]

commonClassPaths :: [String]
commonClassPaths = ["test" </> "src" </> "support"]

commonCB :: PropertyM IO Codebase
commonCB = run commonLoadCB

commonLoadCB :: IO Codebase
commonLoadCB = loadCodebase commonJars commonClassPaths

runWithSymbolicMonadState :: (SymbolicMonadState -> IO [Bool]) ->  PropertyM IO ()
runWithSymbolicMonadState f = do
  res <- run $ do
    oc <- mkOpCache
    withSymbolicMonadState oc f
  mapM_ assert res

runSymTest :: (Backend SymbolicMonad -> IO [Bool]) -> PropertyM IO ()
runSymTest m = runWithSymbolicMonadState (m . symbolicBackend)

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
