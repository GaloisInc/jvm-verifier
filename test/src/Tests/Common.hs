{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-fields #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}
module Tests.Common 
  ( module Text.PrettyPrint
  , module Tests.Common
  , module Execution
  , module Language.JVM.Parser
  , module Verifier.Java.Simulator
  , module Verifier.Java.Utils
  , module Verifier.Java.WordBackend
  , BitEngine(..)
  ) where

import Control.Applicative
import Control.Monad
import qualified Control.Exception as CE
import Data.Int
import Data.Maybe
import Data.Monoid
import Prelude hiding (catch)
import System.Environment
import System.IO.Unsafe
import System.FilePath
import System.Random
import Text.PrettyPrint
import Text.Read

import Test.HUnit as HUnit hiding (test)
import Test.HUnit.Lang (HUnitFailure)
import Test.Framework as TF
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck as QC
import Test.QuickCheck.Monadic as QC

import Execution (runMain)
import Language.JVM.Parser
import Verifier.Java.Simulator hiding (run, assert, runSimulator, runDefSimulator)
import qualified Verifier.Java.Simulator as Sim 
import Verifier.Java.Utils
import Verifier.Java.WordBackend

-- | Bake a particular verbosity level into all simulator calls for
-- the test suite. Configurable using the environment variable
-- @VERBOSITY@.
verb :: Int
verb = fromMaybe 0 . unsafePerformIO $ do
         mvs <- lookupEnv "VERBOSITY"
         case mvs of
           Just vs -> return $ readMaybe vs
           Nothing -> return Nothing
           

runSimulator cb sbe seh msf m =
  Sim.runSimulator cb sbe seh msf (setVerbosity verb >> m)
runDefSimulator cb sbe m = 
  Sim.runDefSimulator cb sbe (setVerbosity verb >> m)

expectFailure :: String -> Assertion -> Assertion
expectFailure msg a = CE.catch (a >> assertFailure msg)
                               (\(_ :: HUnitFailure) -> return ())

assertMsg :: Bool -> String -> PropertyM IO ()
assertMsg b s = when (not b) (run $ putStrLn s) >> QC.assert b

int32s :: Int -> Gen [Int32]
int32s n = choose (1,n) >>= (`vectorOf` (arbitrary :: Gen Int32))

bytes :: Int -> Gen String
bytes n =
  vectorOf (n*2)
  $ arbitrary `suchThat` (\c -> c `elem` (['0'..'9'] ++ ['a'..'f']))

commonJars :: [String]
commonJars = [ "support" </> "galois.jar"        -- primitives & symbolic API
             , "jdk1.6" </> "classes.jar"        -- jdk
             , "user" </> "bcprov-jdk16-145.jar" -- bouncy castle
             ]

commonClassPaths :: [String]
commonClassPaths = ["test" </> "src" </> "support"]

commonLoadCB :: IO Codebase
commonLoadCB = loadCodebase commonJars commonClassPaths

-- | The most "trivial" of any JSS property; just needs a codebase
type TrivialProp = Codebase -> PropertyM IO ()
-- | The most "trivial" of any JSS test case; just needs a codebase
type TrivialCase = Codebase -> Assertion

mkPropWithSMS :: (SymbolicMonadState -> IO [Bool]) -> PropertyM IO ()
mkPropWithSMS f = do
  res <- run $ do
    oc <- mkOpCache
    withSymbolicMonadState oc f
  mapM_ QC.assert res

mkSymProp :: (Backend SymbolicMonad -> IO [Bool]) -> PropertyM IO ()
mkSymProp m = mkPropWithSMS (m . symbolicBackend)

testPropertyN :: Int -> TestName -> PropertyM IO () -> TF.Test
testPropertyN n name test = opts `plusTestOptions` prop
  where opts = mempty { topt_maximum_generated_tests = (Just n) }
        prop = testProperty name (monadicIO test)

testNegPropertyN :: Int -> TestName -> PropertyM IO () -> TF.Test
testNegPropertyN n name test = opts `plusTestOptions` prop
  where opts = mempty { topt_maximum_generated_tests = (Just n) }
        prop = testProperty name . QC.expectFailure . monadicIO $ test

mkAssertionWithSMS :: (SymbolicMonadState -> Assertion) -> Assertion
mkAssertionWithSMS f = do
  oc <- mkOpCache
  withSymbolicMonadState oc f    

mkSymAssertion :: (Backend SymbolicMonad -> Assertion) -> Assertion
mkSymAssertion m = mkAssertionWithSMS (m . symbolicBackend)

--------------------------------------------------------------------------------
-- Misc instances & instance helpers

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR (a,b) g =
  case randomR (fromIntegral a :: Integer, fromIntegral b :: Integer) g of
    (x, g') -> (fromIntegral x, g')
