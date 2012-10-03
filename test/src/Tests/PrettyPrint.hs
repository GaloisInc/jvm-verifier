{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : lerkok
-}

module Tests.PrettyPrint (prettyPrintTests) where

import System.FilePath
import Test.QuickCheck
import Test.QuickCheck.Monadic as QC

import qualified Verinf.SBV.Model as SBV
import qualified Verinf.SBV.Parser as SBV
import Verinf.Symbolic

data TestMode = CREATE          -- create the gold files
              | CHECK           -- test against the gold files

-- set the following to CREATE/CHECK accordingly
mode :: TestMode
mode = CHECK

-- location of the support files; relative to top-level JavaVerifier directory
testDir :: FilePath
testDir = "test" </> "src" </> "support" </> "ppSupport"

testAction :: Int -> String -> PPConfig -> PropertyM IO ()
testAction i what cfg = do res <- run $ do
                             oc <- mkOpCache
                             let path = testDir </> "ppTest.sbv"
                             pgm <- SBV.loadSBV path
                             (_,node) <- SBV.parseSBV oc (\_ _ -> Nothing) "ppTest" pgm
                             return $ prettyTermWith cfg node
                           let file = testDir </> ("pp" ++ what ++ "." ++ show i ++ ".gold")
                           case mode of
                              CREATE -> do run $ writeFile file res
                                           QC.assert True
                              CHECK  -> do gold <- run $ readFile file
                                           QC.assert (res == gold)

testDepth :: Int -> Int -> PropertyM IO ()
testDepth i j = testAction i "Depth" (defaultPPConfig { ppMinSharingDepth = Just j })

testLine :: Int -> Int -> PropertyM IO ()
testLine i j = testAction i "Line" (defaultPPConfig { ppLineLength = j })

testLiteral :: Int -> Bool -> PropertyM IO ()
testLiteral i b = testAction i "Literal" (defaultPPConfig { ppShowConstTypes = b })

mixedTest :: Int -> (Int, Int, Bool) -> PropertyM IO ()
mixedTest i (d, l, b) = testAction i "Mixed" (defaultPPConfig { ppMinSharingDepth = Just d, ppLineLength = l, ppShowConstTypes = b })

prettyPrintTests :: [(Args, Property)]
prettyPrintTests =
      [ (stdArgs{ maxSuccess = 1 }, label ("depth test "    ++ show i)  $ monadicIO (testDepth i j))   | (i, j) <- zip [0..] [0 .. 5]            ]
  ++  [ (stdArgs{ maxSuccess = 1 }, label ("line test "     ++ show i)  $ monadicIO (testLine  i j))   | (i, j) <- zip [0..] [10, 80, 100, 1000] ]
  ++  [ (stdArgs{ maxSuccess = 1 }, label ("literal test "  ++ show i)  $ monadicIO (testLiteral i j)) | (i, j) <- zip [0..] [False, True]       ]
  ++  [ (stdArgs{ maxSuccess = 1 }, label ("mixed test "    ++ show i)  $ monadicIO (mixedTest i j))   | (i, j) <- zip [0..] mixedValues         ]
  where mixedValues = [(d, l, b) | d <- depths, l <- lengths, b <- [False, True]]
        depths      = [0 .. 5]
        lengths     = [5, 10, 20, 50, 100, 500]

_ignore_nouse :: a
_ignore_nouse = undefined CREATE
