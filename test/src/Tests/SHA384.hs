{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE CPP #-}

module Tests.SHA384 (sha384Tests) where

import Control.Applicative
import Control.Monad
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.Vector as V
import JavaParser.Common
import Simulation hiding (run)
import Tests.Common
import Utils

import Verinf.Symbolic

sha384Tests :: [(Args, Property)]
sha384Tests =
  [
  --- dag-based eval for SHA384 on a message of (random r in [1..256]) bytes
    ( stdArgs { maxSuccess = 10 }
    , label "SHA-384 digests of random messages (dag evaluation)" $ monadicIO $
        -- run $ putStrLn "Running SHA-384 test..."
        forAllM (choose (1,256)) $ \numBytes ->
          forAllM (bytes numBytes) $ \msg ->
            evalDagSHA384 msg
    )
  , ( stdArgs { maxSuccess = 2 }
    , label "SHA-384 digests of random messages (aig evaluation)" $ monadicIO $
        -- run $ putStrLn "Running SHA-384 test..."
        forAllM (choose (1,256)) $ \numBytes ->
          forAllM (bytes numBytes) $ \msg ->
            evalAigSHA384 msg
    )
  ]

getGoldenSHA384 :: String -> IO String
getGoldenSHA384 msg =
  readProcess "java"
              [ "-classpath"
#ifdef mingw32_HOST_OS
              , "user/bcprov-jdk16-145.jar;jdk1.6/classes.jar;test/src/support"
#else
              , "user/bcprov-jdk16-145.jar:jdk1.6/classes.jar:test/src/support"
#endif
              , "TestSHA384"
              , msg
              ]
              ""

evalDagSHA384 :: String -> PropertyM IO ()
evalDagSHA384 msg = do
  cb <- commonCB
  golden <- run $ getGoldenSHA384 msg
  -- run $ putStrLn $ "Message        : " ++ msg
  -- run $ putStrLn $ "SHA-384 Golden : " ++ golden
  rslt <- run $ runSymbolic $ do
    msgVars <- replicateM (length msg `div` 2) freshByte
    outVars <- runSimulator cb $ runSHA384 msgVars
    let inputValues = V.map constInt $ V.fromList (hexToByteSeq msg)
    outCns <- symbolicEval inputValues $ do
      mapM evalNode outVars
    return $ byteSeqToHex outCns
  assert $ rslt == golden
  -- run $ putStrLn $ "SHA-384 Result : " ++ rslt

evalAigSHA384 :: String -> PropertyM IO ()
evalAigSHA384 msg = do
  cb <- commonCB
  golden <- run $ getGoldenSHA384 msg
--   run $ putStrLn $ "golden sha = " ++ golden
  rslt <- run $ runSymbolic $ do
    let msgLen = length msg `div` 2
    msgVars <- replicateM msgLen freshByte
    outVars <- runSimulator cb $ runSHA384 msgVars
    outLits <- concat <$> mapM getVarLitLsbf outVars
    -- | Word-level inputs
    let cinps = map constInt $ hexToByteSeq msg
    -- | Boolean inputs to evalAig
    let binps = concatMap (take 8 . intToBoolSeq) cinps
    be <- getBitEngine
    r <- evalAig be binps outLits
    let rs = [ constInt . head . hexToIntSeq . boolSeqToHex
               $ take 32 (drop (32*k) r)
             | k <- [0..47]
             ]
    -- Perform evaluation and blasting of every literal
    -- (expensive, but good for debugging failed runs)
--     evalAndBlast (map constInt $ hexToByteSeq msg) binps
--     liftIO $ mapM_ (putStrLn . show) rs
--     liftIO $ putStrLn $ "evalAigSHA384: rs = " ++ byteSeqToHex rs
    return $ byteSeqToHex rs
  assert $ rslt == golden

runSHA384 :: AigOps sym => [MonadTerm sym] -> Simulator sym [MonadTerm sym]
runSHA384 msgVars = do
  msgArray <- newIntArray intArrayTy msgVars
  outArray <- newMultiArray (ArrayType ByteType) [mkCInt (Wx 32) 48]
  [(pd, Terminated)] <- runStaticMethod "TestSHA384"
                                        "sha384_digest"
                                        "([B[B)V"
                                        [ RValue msgArray
                                        , RValue outArray
                                        ]
  getIntArray pd outArray

_ignore_nouse :: a
_ignore_nouse = undefined main evalAigSHA384

main :: IO ()
main = runTests sha384Tests