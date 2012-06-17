{-# LANGUAGE Rank2Types #-}
module Main where

import Control.Monad
import System.Environment(getArgs)
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

import Tests.Arrays
import Tests.ExampleErrorSession
import Tests.ExampleExceptionSession
import Tests.ExpectedErrors
import Tests.PathStateMerges
import Tests.PrimOps
import Tests.RC564
import Tests.SBVParser
import Tests.SHA384
import Tests.JAPI
import Tests.PrettyPrint
import Tests.RuleChecker

main :: IO ()
main = do tags <- getArgs
          runTests (\(t, _) -> if null tags then True else t `elem` tags)

runTests :: (forall a. ((String, a) -> Bool)) -> IO ()
runTests chooseTest = do
  results <- fmap concat $ forM qcProps $ \(desc, props) -> do
               putStrLn $ "=== " ++ desc ++ " ==="
               mapM (uncurry quickCheckWithResult) props
  when (noOfTests /= noOfSelected) $ putStrLn $ "Selected " ++ show noOfSelected ++ " of " ++ show noOfTests ++ " test categories."
  if all isSuccess results
    then putStrLn "All tests successful."     >> exitWith ExitSuccess
    else putStrLn "One or more tests failed." >> exitWith (ExitFailure 1)

  where
    noOfTests    = length allTests
    noOfSelected = length qcProps
    qcProps = filter chooseTest allTests
    allTests =
      [ ("Error propagation in IO.Session: via IO exception",   exampleExceptionSessionTests)
      , ("Error propagation in IO.Session: via ErrorT",         exampleErrorSessionTests)
      , ("SBVParser",                                           sbvParserTests)
      , ("PrimOps",                                             primOpTests)
      , ("Arrays",                                              arrayTests)
      , ("Path state merging",                                  psmsTests)
      , ("Expected Errors",                                     expErrTests)
      , ("RC5-64",                                              rc564Tests)
      , ("SHA-384",                                             sha384Tests)
      , ("Java API",                                            japiTests)
      , ("PrettyPrint",                                         prettyPrintTests)
      , ("RuleChecker",                                         ruleCheckerTests)
      ]
