{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : lerkok
-}

module Tests.PrettyPrint (prettyPrintTests) where

import System.FilePath
import Test.HUnit hiding (Test, path)
import Test.Framework
import Test.Framework.Providers.HUnit

import qualified Verinf.SBV.Model as SBV
import qualified Verinf.SBV.Parser as SBV
import Verinf.Symbolic

data TestMode = CREATE          -- create the gold files
              | CHECK           -- test against the gold files

-- set the following to CREATE/CHECK accordingly
mode :: TestMode
mode = CHECK

prettyPrintTests :: Test
prettyPrintTests = testGroup "PrettyPrint" $
      [ testCase ("depth test "   ++ show i) (testDepth i j) 
            | (i, j) <- zip [0..] [0 .. 5]            ]
  ++  [ testCase ("line test "    ++ show i) (testLine  i j)
            | (i, j) <- zip [0..] [10, 80, 100, 1000] ]
  ++  [ testCase ("literal test " ++ show i) (testLiteral i j)
            | (i, j) <- zip [0..] [False, True]       ]
  ++  [ testCase ("mixed test "   ++ show i) (mixedTest i j)   
            | (i, j) <- zip [0..] mixedValues         ]
  where mixedValues = [(d, l, b) | d <- depths, l <- lengths, b <- [False, True]]
        depths      = [0 .. 5]
        lengths     = [5, 10, 20, 50, 100, 500]

-- location of the support files; relative to top-level JavaVerifier directory
testDir :: FilePath
testDir = "test" </> "src" </> "support" </> "ppSupport"

testAction :: Int -> String -> PPConfig -> Assertion
testAction i what cfg = do res <- do
                             oc <- mkOpCache
                             let path = testDir </> "ppTest.sbv"
                             pgm <- SBV.loadSBV path
                             (_,node) <- SBV.parseSBV oc (\_ _ -> Nothing) "ppTest" pgm
                             return $ prettyTermWith cfg node
                           let file = testDir </> ("pp" ++ what ++ "." ++ show i ++ ".gold")
                           case mode of
                              CREATE -> writeFile file res
                              CHECK  -> do gold <- readFile file
                                           res @?= gold

testDepth :: Int -> Int -> Assertion
testDepth i j = testAction i "Depth" (defaultPPConfig { ppMinSharingDepth = Just j })

testLine :: Int -> Int -> Assertion
testLine i j = testAction i "Line" (defaultPPConfig { ppLineLength = j })

testLiteral :: Int -> Bool -> Assertion
testLiteral i b = testAction i "Literal" (defaultPPConfig { ppShowConstTypes = b })

mixedTest :: Int -> (Int, Int, Bool) -> Assertion
mixedTest i (d, l, b) = testAction i "Mixed" (defaultPPConfig { ppMinSharingDepth = Just d, ppLineLength = l, ppShowConstTypes = b })

_ignore_nouse :: a
_ignore_nouse = undefined CREATE
