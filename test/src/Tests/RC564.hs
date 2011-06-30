{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE CPP #-}

module Tests.RC564 (rc564Tests) where

import Control.Monad
import qualified Data.Vector as V
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic

import JavaParser
import Symbolic
import Simulation hiding (run)
import Tests.Common
import Utils

rc564Tests :: [(Args, Property)]
rc564Tests =
  [ -- dag-based eval only for RC564 on two random 128b inputs
    ( stdArgs{ maxSuccess = 10 }
    , label "RC5-64 random keys/messages" $ monadicIO $
        -- run $ putStrLn "Running RC5-64 test..."
        forAllM (bytes 16) $ \key ->
          forAllM (bytes 16) $ \inp -> do
            evalDagRC564 key inp
    )
  ]

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests rc564Tests

--------------------------------------------------------------------------------
-- RC-564

getGoldenRC564 :: String -> String -> IO String
getGoldenRC564 key inp =
  readProcess "java"
              [ "-classpath"
#ifdef mingw32_HOST_OS
              , "user/bcprov-jdk16-145.jar;jdk1.6/classes.jar;test/src/support"
#else
              , "user/bcprov-jdk16-145.jar:jdk1.6/classes.jar:test/src/support"
#endif
              , "TestRC564"
              , key
              , inp
              ]
              ""

-- TODO: add evalAigRC564 etc.
evalDagRC564 :: String -> String -> PropertyM IO ()
evalDagRC564 key input = do
  cb     <- commonCB
  golden <- run $ getGoldenRC564 key input
--   run $ putStrLn $ "Key    : " ++ key
--   run $ putStrLn $ "Input  : " ++ input
--   run $ putStrLn $ "Golden : " ++ golden
  rslt <- run $ runSymbolic $ do
    keyVars <- replicateM 16 freshByte
    inpVars <- replicateM 16 freshByte
    outVars <- runSimulator cb $ runRC564 keyVars inpVars
    let inpValues = V.map constInt
                  $ V.fromList
                  $ hexToByteSeq key ++ hexToByteSeq input
    outCns <- symbolicEval inpValues $ do
      mapM evalNode outVars
    return $ byteSeqToHex outCns
  assert $ rslt == golden
--   run $ putStrLn $ "Result : " ++ rslt

_makeAigerRC564 :: String -> IO ()
_makeAigerRC564 filepath = do
  cb <- commonLoadCB
  putStrLn "Simulating RC564"
  runSymbolic $ do
    keyVars <- replicateM 16 freshByte
    inpVars <- replicateM 16 freshByte
    outValues <- runSimulator cb $ runRC564 keyVars inpVars
    liftIO $ putStrLn "Creating RC564 aiger..."
    outLits <- mapM getVarLit outValues
    liftAigMonad $ do
      liftIO $ putStrLn $ "Writing RC564 aiger to '" ++ filepath ++ "'"
      writeAiger filepath $ concat (map (take 8 . toLsbf_lit) outLits)

runRC564 :: AigOps sym =>
            [MonadTerm sym] -> [MonadTerm sym] -> Simulator sym [MonadTerm sym]
runRC564 keyVars inpVars = do
  let byteArrayType = ArrayType ByteType
  keyArray <- newIntArray byteArrayType keyVars
  inpArray <- newIntArray byteArrayType inpVars
  outArray <- newMultiArray byteArrayType [mkCInt (Wx 32) 16]
  [(pd,Terminated)] <- runStaticMethod "TestRC564"
                                       "rc564_encrypt"
                                       "([B[B[B)V"
                                       [ RValue keyArray
                                       , RValue inpArray
                                       , RValue outArray
                                       ]
  getIntArray pd outArray
