{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

module Tests.SHA384 (sha384Tests) where

import Control.Applicative
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import System.Process

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Tests.Common

main :: IO ()
main = defaultMain [sha384Tests]

sha384Tests :: Test
sha384Tests = testGroup "SHA384" $
  [
  --- dag-based eval for SHA384 on a message of (random r in [1..256]) bytes
    testPropertyN 10 "SHA-384 digests of random messages (dag evaluation)" $
        -- run $ putStrLn "Running SHA-384 test..."
        forAllM (choose (1,256)) $ \numBytes ->
          forAllM (bytes numBytes) $ \msg ->
            evalDagSHA384 msg
  , testPropertyN 2 "SHA-384 digests of random messages (aig evaluation)" $
        -- run $ putStrLn "Running SHA-384 test..."
        forAllM (choose (1,256)) $ \numBytes ->
          forAllM (bytes numBytes) $ \msg ->
            evalAigSHA384 msg
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
  cb <- run commonLoadCB
  golden <- run $ getGoldenSHA384 msg
  -- run $ putStrLn $ "Message        : " ++ msg
  -- run $ putStrLn $ "SHA-384 Golden : " ++ golden
  oc <- run mkOpCache
  rslt <- run $ withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms 
    msgVars <- replicateM (length msg `div` 2) $ freshByte sbe
    outVars <- runDefSimulator cb sbe $ runSHA384 msgVars
    let inputValues = V.map constInt $ V.fromList (hexToByteSeq msg)
    evalFn <- concreteEvalFn inputValues
    byteSeqToHex <$> mapM evalFn outVars
  assert $ rslt == golden
  -- run $ putStrLn $ "SHA-384 Result : " ++ rslt

evalAigSHA384 :: String -> PropertyM IO ()
evalAigSHA384 msg = do
  cb <- run commonLoadCB
  golden <- run $ getGoldenSHA384 msg
--   run $ putStrLn $ "golden sha = " ++ golden
  oc <- run mkOpCache
  rslt <- run $ withSymbolicMonadState oc $ \sms -> do
    let msgLen = length msg `div` 2
    let sbe = symbolicBackend sms 
    let be = smsBitEngine sms
    msgVars <- replicateM msgLen $ freshByte sbe
    outVars <- runDefSimulator cb sbe $ runSHA384 msgVars
    outLits <- concat <$> mapM (fmap toLsbf_lit . getVarLit sbe) outVars
    -- | Word-level inputs
    let cinps = map constInt $ hexToByteSeq msg
    -- | Boolean inputs to evalAig
    let binps = concatMap (take 8 . intToBoolSeq) cinps
    r <- beEvalAigV be (SV.fromList binps) (SV.fromList outLits)
    let rs = [ constInt . head . hexToIntSeq . boolSeqToHex
                 $ SV.toList $ SV.slice (32*k) 32 r
             | k <- [0..47]
             ]
    return $ byteSeqToHex rs
  assert $ rslt == golden

runSHA384 :: MonadSim sbe m => [SBETerm sbe] -> Simulator sbe m [SBETerm sbe]
runSHA384 msgVars = do
  msgArray <- newIntArray intArrayTy msgVars
  l <- withSBE $ \sbe -> termInt sbe 48
  outArray <- newMultiArray (ArrayType ByteType) [l]
  _ <- runStaticMethod "TestSHA384"
                       "sha384_digest"
                       "([B[B)V"
                       [ RValue msgArray
                       , RValue outArray
                       ]
  getIntArray outArray

_ignore_nouse :: a
_ignore_nouse = undefined main evalAigSHA384

