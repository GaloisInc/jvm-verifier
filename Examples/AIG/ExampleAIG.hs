{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE TypeFamilies #-}

module Main
where
import Control.Applicative ((<$>))
import Control.Monad (forM, replicateM)
import Control.Monad.Trans
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import System.CPUTime
import Text.Printf

import Execution
import JavaParser.Common
import JavaParser
import Simulation
import Tests.Common
import Utils
import Verinf.Symbolic

-- Helper methods {{{1
time :: t -> IO t
time v = do
    start <- getCPUTime
    end   <- v `seq` getCPUTime
    let diff = (fromIntegral (end - start)) / (10^(12::Int))
    _ <- printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

-- Test inputs {{{1
testKey, testInput, zeroBlock :: String
testKey = "95A8EE8E89979B9EFDCBC6EB9797528D"
testInput = "4EC137A426DABF8AA0BEB8BC0C2B89D6"
zeroBlock = replicate 32 '0'

-- Cryptol Java Port {{{1
runCryptolJava :: String -> [DagTerm] -> [DagTerm] -> Simulator SymbolicMonad [DagTerm]
runCryptolJava name key input = do
  setVerbosity 0
  -- Create array for key.
  keyArray <- newIntArray intArrayTy key
  -- Initialize in with variables
  inArray <- newIntArray intArrayTy input
  -- Create out buffer
  outArray <- newMultiArray intArrayTy [mkCInt 32 4]
  -- Invoke static method
  liftIO $ putStrLn "runCryptolJava: Running static method"
  rs <- runStaticMethod name
                        "run" "([I[I[I)V"
                        [ RValue keyArray
                        , RValue inArray
                        , RValue outArray
                        ]
  let [(pd, Terminated)] = withoutExceptions rs
  -- Get (path-specific!) result
  forM [0..3] $ \i -> do
    IValue iValue <- getArrayValue pd outArray (mkCInt 32 i)
    return iValue

evalCryptolJava :: String -> String -> String -> IO String
evalCryptolJava name key input = do
  oc <- mkOpCache
  cb <- commonLoadCB
  withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms
    outVars <- runSimulator sbe defaultSimFlags cb $ do
      setVerbosity 0
      let tint      = mkCInt (Wx 32) . fromIntegral
          keyVars   = map tint $ hexToIntSeq key
          inputVars = map tint $ hexToIntSeq input
      runCryptolJava name keyVars inputVars
    evalFn <- concreteEvalFn V.empty
    intSeqToHex <$> mapM evalFn outVars

evalCryptolJavaWord :: String -> String -> String -> IO String
evalCryptolJavaWord name key input = do
  oc <- mkOpCache
  cb <- commonLoadCB
  withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms 
    keyVars   <- replicateM 4 $ freshInt sbe
    inputVars <- replicateM 4 $ freshInt sbe
    outVars <- runDefSimulator sbe cb $ do
      setVerbosity 0
      runCryptolJava name keyVars inputVars
    let inp = V.map constInt $ V.fromList $ hexToIntSeq key ++ hexToIntSeq input
    evalFn <- concreteEvalFn inp
    intSeqToHex <$> mapM evalFn outVars

-- Levent's Cryptol C Port {{{1
runCryptolC :: [DagTerm] -> [DagTerm] -> Simulator SymbolicMonad [DagTerm]
runCryptolC key input = do
  -- Create array for key.
  keyArray <- newIntArray intArrayTy $ reverse key
  -- Create array for input
  inArray <- newIntArray intArrayTy $ reverse input
  -- Invoke static encrypt method.
  liftIO $ putStrLn "runCryptolC: Running static method"
  [(pd, ReturnVal (RValue outArray))] <-
    withoutExceptions
      <$> runStaticMethod "AES128Encrypt"
                          "encrypt" "([I[I)[I"
                          [RValue keyArray, RValue inArray]

  -- Get (path-specific!) result
  rslt <-
    forM [3,2..0] $ \i -> do
      IValue iValue <- getArrayValue pd outArray (mkCInt 32 i)
      return iValue
  liftIO $ putStrLn "runCryptolC: completed"
  return rslt

type RunIO sym a = sym a -> IO a

makeCryptolAiger :: String
                    -> Codebase
                    -> ([DagTerm] -> [DagTerm] -> Simulator SymbolicMonad [DagTerm])
                    -> IO ()
makeCryptolAiger filepath cb simFn = do
  oc <- mkOpCache
  withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms 
    let be  = smsBitEngine sms
    keyVars   <- replicateM 4 $ freshInt sbe
    inputVars <- replicateM 4 $ freshInt sbe
    outVars   <- runDefSimulator sbe cb (setVerbosity 0 >> simFn keyVars inputVars)
    outLits   <- mapM (getVarLit sbe) outVars
    writeAiger be filepath (concat (map toLsbf_lit outLits))

evalCryptolC :: String -> String -> IO String
evalCryptolC key input = do
  oc <- mkOpCache
  cb <- commonLoadCB
  let tint      = mkCInt (Wx 32) . fromIntegral
      keyVars   = map tint $ hexToIntSeq key
      inputVars = map tint $ hexToIntSeq input
  withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms
    outVars <- runDefSimulator sbe cb $ do
      setVerbosity 0
      runCryptolC keyVars inputVars
    evalFn <- concreteEvalFn V.empty
    intSeqToHex <$> mapM evalFn outVars

-- Bouncy Castle {{{1

data CipherType = AES | RC5

runBouncyCastle :: CipherType
                -> String
                -> [DagTerm]
                -> [DagTerm]
                -> Simulator SymbolicMonad [DagTerm]
runBouncyCastle ct name key input = do
  let engineClassName = "org/bouncycastle/crypto/engines/" ++ name
      paramClassName  = "org/bouncycastle/crypto/params/" ++
                          case ct of
                            AES -> "KeyParameter"
                            RC5 -> "RC5Parameters"
      byteArrayType   = ArrayType ByteType
      init1           = makeMethodKey "<init>" $ case ct of AES -> "([B)V"; RC5 -> "([BI)V"
      rc5Rounds       = IValue (mkCInt 32 1)
--  setVerbosity 4
  ---- Create array for key.
  keyArray <- newIntArray byteArrayType key
  ---- Create cipher params object: KeyParameter for AES, RC5Parameters for RC5
  let initParams = case ct of
                     AES -> [RValue keyArray]
                     RC5 -> [RValue keyArray, rc5Rounds]
  cipherParam <- newObject paramClassName
  invokeInstanceMethod paramClassName init1 cipherParam initParams
  run_
  setInstanceFieldValue cipherParam
                        (FieldId paramClassName "key" byteArrayType)
                        (RValue keyArray)
  -- There's an additional rounds field for RC5Parameters, so set it
  case ct of
    RC5 -> do
      setInstanceFieldValue cipherParam
                            (FieldId paramClassName "rounds" IntType)
                            rc5Rounds
    _ -> return ()
  -- Instantiate & initialize the engine (e.g., AES, RC5, etc) class
  engine <- newObject engineClassName
  invokeInstanceMethod engineClassName
                       (makeMethodKey "init" "(ZLorg/bouncycastle/crypto/CipherParameters;)V")
                       engine
                       [ IValue (mkCInt (Wx 32) 1), RValue cipherParam ]
  run_
  -- Initialize in with variables
  inArray <- newIntArray byteArrayType input
  -- Create out buffer
  outArray <- newMultiArray byteArrayType [mkCInt (Wx 32) 16]
  -- Run processBlock
  invokeInstanceMethod engineClassName
                       (makeMethodKey "processBlock" "([BI[BI)I")
                       engine
                       [ RValue inArray
                       , IValue (mkCInt (Wx 32) 0)
                       , RValue outArray
                       , IValue (mkCInt (Wx 32) 0)]
  rs2 <- run
  let [(pd,_)] = withoutExceptions rs2
  -- Get (path-specific!) result
  forM [0..3] $ \i -> do
    IValue iValue <- getArrayValue pd outArray (mkCInt (Wx 32) i)
    return iValue

makeBouncyCastleAiger :: CipherType -> String -> Codebase -> IO ()
makeBouncyCastleAiger ct name cb = do
  oc <- mkOpCache
  withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms
    let be = smsBitEngine sms
    liftIO $ do
      revKey   <- replicateM 16 $ freshByte sbe
      revInput <- replicateM 16 $ freshByte sbe
      output <- runDefSimulator sbe cb $ do
        setVerbosity 0
        runBouncyCastle ct name (reverse revKey) (reverse revInput)
      outputLits <- mapM (getVarLit sbe) $ reverse output
      putStrLn $ "makeBouncyCastleAiger: Creating " ++ name ++ ".aig"
      writeAiger be (name ++ ".aig") $
        concatMap (SV.toList . SV.take 8 . toLsbfV) outputLits

evalBouncyCastle :: CipherType -> String -> String -> String -> IO String
evalBouncyCastle ct name key input = do
  oc <- mkOpCache
  cb <- commonLoadCB
  withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms
    keyVars   <- replicateM 16 $ freshByte sbe
    inputVars <- replicateM 16 $ freshByte sbe
    outVars <- runDefSimulator sbe cb $ do
      setVerbosity 0
      runBouncyCastle ct name keyVars inputVars
    let inp = V.map constInt $ V.fromList $ hexToByteSeq key ++ hexToByteSeq input
    evalFn <- concreteEvalFn inp
    intSeqToHex <$> mapM evalFn outVars

writeBouncyCastleAiger :: String -> IO ()
writeBouncyCastleAiger name = do
  cb <- commonLoadCB
  let aigerName = name ++ ".aig"
  _ <- printf "Generating %s\n" aigerName
  makeBouncyCastleAiger AES name cb
  printf "Finished writing %s\n" aigerName

-- {{{1 Create all AIGs

createAigers :: IO ()
createAigers = do
  cb <- loadCodebase commonJars ("user":commonClassPaths)
  makeCryptolAiger "cryptolJava.aig" cb     $ runCryptolJava "AESCryptol"
  makeCryptolAiger "cryptolNoTables.aig" cb $ runCryptolJava "AESCryptolNoTables"
  makeCryptolAiger "cryptolC.aig" cb runCryptolC

  -- [JS <2011-07-06 Wed>] TODO FIXME BUG : At some point, the simulator started
  -- crashing on this example (and likely downstream examples as well), but we
  -- missed it because this ExampleAIG code is not tested as a part of the test
  -- framework.  Crashes as far back in the repo as 264670cd, so it's been
  -- around for a while and is decidely NOT part of the June/July 2011 repo
  -- reorganization/refactoring.  Captured here for future reference.

  makeBouncyCastleAiger AES "AESEngine" cb
  makeBouncyCastleAiger AES "AESLightEngine" cb
  makeBouncyCastleAiger AES "AESFastEngine" cb

main :: IO ()
main = createAigers
