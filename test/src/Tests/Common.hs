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
{-# LANGUAGE CPP                  #-}
module Tests.Common 
  ( module Text.PrettyPrint
  , module Tests.Common
  , module Execution
  , module Language.JVM.Parser
  , module Verifier.Java.Simulator
  , module Verifier.Java.Utils
  , module Verifier.Java.WordBackend
  , module Verifier.Java.TestInterface
  , BitEngine(..)
  
  ) where

import Control.Monad
import Data.Char
import Data.Int
import Data.Maybe
import Data.Word
import Numeric
import System.Environment
import System.IO.Unsafe
import System.FilePath
import System.Random
import Text.PrettyPrint
import Text.Read

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic as QC

import Execution (runMain)
import Language.JVM.Parser
import Verifier.Java.Simulator hiding (run, assert, runSimulator, runDefSimulator)
import qualified Verifier.Java.Simulator as Sim
import Verifier.Java.Utils
import Verifier.Java.WordBackend
import Verifier.Java.TestInterface

#if __GLASGOW_HASKELL__ < 706
import Text.ParserCombinators.ReadP as P

lookupEnv :: String -> IO (Maybe String)
lookupEnv key = lookup key `liftM` getEnvironment

readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a
#endif

-- | Bake a particular verbosity level into all simulator calls for
-- the test suite. Configurable using the environment variable
-- @VERBOSITY@.
verb :: Int
verb = fromMaybe 0 . unsafePerformIO $ do
         mvs <- lookupEnv "VERBOSITY"
         case mvs of
           Just vs -> return $ readMaybe vs
           Nothing -> return Nothing

runSimulator :: AigOps sbe =>
                Codebase
             -> Backend sbe
             -> SEH sbe IO
             -> Maybe SimulationFlags
             -> Simulator sbe IO a
             -> IO a
runSimulator cb sbe seh msf m =
  Sim.runSimulator cb sbe seh msf (setVerbosity verb >> m)

runDefSimulator :: AigOps sbe =>
                   Codebase -> Backend sbe -> Simulator sbe IO a -> IO a
runDefSimulator cb sbe m = 
  Sim.runDefSimulator cb sbe (setVerbosity verb >> m)

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

testPropertyN :: Int -> TestName -> PropertyM IO () -> TestTree
testPropertyN n name test = adjustOption optf prop
  where optf (QuickCheckTests old) = QuickCheckTests (min n old)
        prop = testProperty name (monadicIO test)

testNegPropertyN :: Int -> TestName -> PropertyM IO () -> TestTree
testNegPropertyN n name test = adjustOption optf prop
  where optf (QuickCheckTests old) = QuickCheckTests (min n old)
        prop = testProperty name . expectFailure . monadicIO $ test

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

byteSeqToHex :: [CValue] -> String
byteSeqToHex ((getSValW -> Just (32, c)) : r)
  = (intToDigit $ fromIntegral $ ((fromIntegral c :: Word32) `quot` 16) `rem` 16)
    : (intToDigit $ fromIntegral $ (fromIntegral c :: Word32) `rem`  16)
    : byteSeqToHex r
byteSeqToHex ((getSValW -> Just (w,_c)) : _r)
  = error $ "internal: byteSeqToHex unexpected width " ++ show w
byteSeqToHex (CArray _ : _) = error "internal: byteSeqToHex CArray"
byteSeqToHex (CBool _ : _) = error "internal: byteSeqToHex CBool"
byteSeqToHex (CRec{} : _) = error "internal: byteSeqToHex CRec"
byteSeqToHex [] = []
byteSeqToHex _ = error "internal: byteSeqToHex bad value"

intToHex :: CValue -> String
intToHex (getSValW -> Just (32, c)) =
  let r = showHex (fromIntegral c :: Word32) ""
   in replicate (8 - length r) '0' ++ r
intToHex (getSValW -> Just (64, c)) =
  let r = showHex (fromIntegral c :: Word64) ""
   in replicate (16 - length r) '0' ++ r
intToHex _ = error $ "internal: Undefined intToHex for type"

intSeqToHex :: [CValue] -> String
intSeqToHex = foldl (\s c -> intToHex c ++ s) []

intSeqToBoolSeq :: [CValue] -> [Bool]
intSeqToBoolSeq = hexToBoolSeq . intSeqToHex

bitblastFlags :: SimulationFlags
bitblastFlags = defaultSimFlags { alwaysBitBlastBranchTerms = True }

satFlags :: SimulationFlags
satFlags = defaultSimFlags { satAtBranches = True }
