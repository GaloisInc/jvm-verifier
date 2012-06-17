{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

module Tests.Arrays (arrayTests) where

import Control.Applicative
import Control.Monad
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

import JavaParser.Common

import Simulation hiding (run)
import Tests.Common
import Utils
import Utils.Simulation

import Verinf.Symbolic

arrayTests :: [(Args, Property)]
arrayTests =
  [
    test 10 False sa1 "Read concarray @ symidx"
  , test 10 False sa2 "Write symelem @ symidx to concarray"
  , test 1  False sa3 "Write symelem @ concidx to concarray"
  , test 1  False sa4 "Write symelem @ concidx to 2-dim symarray"
  ]

-- | Read concrete int array at symbolic indices
sa1 :: TrivialProp
sa1 cb =
  forAllM (int32s 128) $ \arrayElems ->
  runTest $ do
    sbe <- getBackend
    idx <- liftIO $ IValue <$> freshInt sbe
    outVar <- runDefSymSim cb $ do
      let tint = mkCInt 32 . fromIntegral
      inpArr <- newIntArray intArrayTy $ map tint arrayElems
      Right rslt <- snd . takeIntRslt . withoutExceptions
                    <$> runStaticMethod "Arrays" "index" "(I[I)I"
                          [idx, RValue inpArr]
      return rslt
    be <- getBitEngine
    liftIO $ do
      outIntLit <- toLsbf_lit <$> getVarLit sbe outVar
      let getAt = fmap boolSeqToInt32
                . (\inp -> evalAig be inp outIntLit)
                . intToBoolSeq
                . constInt
      ((:[]) . (== arrayElems))
        <$> mapM getAt [0..(fromIntegral $ length arrayElems - 1)]

-- | Concrete int array update w/ symbolic index & symbolic value
sa2 :: TrivialProp
sa2 cb =
  forAllM (int32s 128 `suchThat` (not . elem 42))            $ \arrayElems ->
  forAllM (choose (0, fromIntegral (length arrayElems - 1))) $ \overwriteIdx ->
  runTest $ do
    let n = length arrayElems
    sbe <- getBackend
    [idx, val]  <- liftIO $ replicateM 2 $ IValue <$> freshInt sbe
    rslt <- runDefSymSim cb $ do
      let tint = mkCInt 32 . fromIntegral
      arr <- newIntArray intArrayTy $ map tint arrayElems
      [(pd, Terminated)] <- withoutExceptions
                            <$> runStaticMethod "Arrays" "update" "(II[I)V"
                                  [idx, val, RValue arr]
      getIntArray pd arr

    -- Overwrite a random index with 42 and check it
    be <- getBitEngine
    liftIO $ do
      rsltLits <- concatMap toLsbf_lit <$> mapM (getVarLit sbe) rslt
      ((:[]) . elem 42)
        <$> outsToInts32 n
        <$> evalAig be (evalAigArgs32 [overwriteIdx, 42]) rsltLits

-- | Symbolic array update w/ concrete index and symbolic value
sa3 :: TrivialProp
sa3 cb =
  runTest $ do
    sbe <- getBackend
    let n    = 3
        fill = 99
    val     <- liftIO $ IValue <$> freshInt sbe
    symVals <- liftIO $ replicateM n $ freshInt sbe
    rslt <- runDefSymSim cb $ do
      arr <- newIntArray intArrayTy symVals
      [(pd, Terminated)] <-
        withoutExceptions
        <$> runStaticMethod "Arrays" "update" "(II[I)V"
              [IValue (mkCInt 32 $ fromIntegral n - 1), val, RValue arr]
      getIntArray pd arr
    be <- getBitEngine
    -- Overwrite the last index with 42 and check it
    liftIO $ do
      rsltLits <- concatMap toLsbf_lit <$> mapM (getVarLit sbe) rslt
      ((:[]) . (==) (replicate (n-1) fill ++ [42]))
        <$> outsToInts32 n
        <$> evalAig be (evalAigArgs32 (42 : replicate n fill)) rsltLits

-- | Symbolic 2-dim array update w/ concrete index and value
sa4 :: TrivialProp
sa4 cb =
  runTest $ do
    sbe <- getBackend
    let n        = 3 ; nI = fromIntegral n
        m        = 4
        numElems = n * m
        fill     = 99
    symVals <- liftIO $ replicateM n $ replicateM m $ freshInt sbe
    rslt <- runDefSymSim cb $ do
      let tint = mkCInt 32
      inners <- mapM (newIntArray intArrayTy) symVals
      twodim <- newMultiArray (ArrayType intArrayTy) [tint nI]
      forM_ (map (tint . fromIntegral) [0..nI] `zip` map RValue inners) $
        uncurry (setArrayValue twodim)
      [(pd, Terminated)] <-
        withoutExceptions
        <$> runStaticMethod "Arrays" "update" "(III[[I)V"
              (map (IValue . tint) [0, 0, 42] ++ [RValue twodim])
      concat <$> (mapM (getIntArray pd) =<< getRefArray pd twodim)

    -- Overwrite the first index with 42 and check it
    be <- getBitEngine
    liftIO $ do
      rsltLits <- concatMap toLsbf_lit <$> mapM (getVarLit sbe) rslt
      ((:[]) . (==) (42 : replicate (numElems - 1) fill))
        <$> outsToInts32 (fromIntegral numElems)
        <$> evalAig be (evalAigArgs32 (replicate numElems fill)) rsltLits


--------------------------------------------------------------------------------
-- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests arrayTests
