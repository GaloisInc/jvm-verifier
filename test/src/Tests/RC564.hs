{- |
Module           : $Header$
Description      :
License          : BSD3
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

module Tests.RC564 (rc564Tests, runRC564) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import qualified Data.Vector as V

import Test.Tasty
import Test.QuickCheck.Monadic

import Tests.Common

rc564Tests :: TestTree
rc564Tests = testGroup "RC564" $
  [ -- dag-based eval only for RC564 on two random 128b inputs
    testPropertyN 10 "RC5-64 random keys/messages" $
        -- run $ putStrLn "Running RC5-64 test..."
        forAllM (bytes 16) $ \key ->
          forAllM (bytes 16) $ \inp -> do
            evalDagRC564 key inp
  ]

-- _ignore_nouse :: a
-- _ignore_nouse = undefined main

-- main :: IO ()
-- main = defaultMain [rc564Tests]

--------------------------------------------------------------------------------
-- RC-564

getGoldenRC564 :: String -> String -> IO String
getGoldenRC564 key inp = runJava ["TestRC564" , key , inp]

-- TODO: add evalAigRC564 etc.
evalDagRC564 :: String -> String -> PropertyM IO ()
evalDagRC564 key input = do
  cb <- run commonLoadCB
  oc <- run mkOpCache
  golden <- run $ getGoldenRC564 key input
--   run $ putStrLn $ "Key    : " ++ key
--   run $ putStrLn $ "Input  : " ++ input
--   run $ putStrLn $ "Golden : " ++ golden
  rslt <- run $ withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms 
    keyVars <- replicateM 16 $ freshByte sbe
    inpVars <- replicateM 16 $ freshByte sbe
    outVars <- runDefSimulator cb sbe $ runRC564 keyVars inpVars
    let inpValues = V.map constInt
                  $ V.fromList
                  $ hexToByteSeq key ++ hexToByteSeq input
    evalFn <- concreteEvalFn inpValues
    byteSeqToHex <$> mapM evalFn outVars
  assert $ rslt == golden
--   run $ putStrLn $ "Result : " ++ rslt

{-
_makeAigerRC564 :: String -> IO ()
_makeAigerRC564 filepath = do
  cb <- commonLoadCB
  oc <- mkOpCache
  putStrLn "Simulating RC564"
  withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms
    let be = smsBitEngine sms
    keyVars <- replicateM 16 $ freshByte sbe
    inpVars <- replicateM 16 $ freshByte sbe
    outValues <- runDefSimulator cb sbe $ runRC564 keyVars inpVars
    putStrLn "Creating RC564 aiger..."
    putStrLn $ "Writing RC564 aiger to '" ++ filepath ++ "'"
    writeAiger be filepath outValues
-}

runRC564 :: MonadSim sbe m =>
            [SBETerm sbe] -> [SBETerm sbe] -> Simulator sbe m [SBETerm sbe]
runRC564 keyVars inpVars = do
  let byteArrayType = ArrayType ByteType
  keyArray <- newIntArray byteArrayType keyVars
  inpArray <- newIntArray byteArrayType inpVars
  l16 <- withSBE $ \sbe -> termInt sbe 16
  outArray <- newMultiArray byteArrayType [l16]
  _ <- runStaticMethod (mkClassName "TestRC564")
                       "rc564_encrypt"
                       "([B[B[B)V"
                       [ RValue keyArray
                       , RValue inpArray
                       , RValue outArray
                       ]
  getIntArray outArray
