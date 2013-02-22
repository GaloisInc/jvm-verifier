{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

module Tests.Arrays (arrayTests) where

import Control.Applicative
import Control.Monad
import Data.Int
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

import Tests.Common

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
    runWithSymbolicMonadState $ \sms -> do
      let sbe = symbolicBackend sms
      let be = smsBitEngine sms
      idx <- IValue <$> freshInt sbe
      outVar <- runDefSimulator cb sbe $ do
        let tint = mkCInt 32 . fromIntegral
        inpArr <- newIntArray intArrayTy $ map tint arrayElems
        [(_, Just (IValue rslt))] <-
          runStaticMethod "Arrays" "index" "(I[I)I" [idx, RValue inpArr]
        return rslt
      outIntLit <- toLsbf_lit <$> getVarLit sbe outVar
      let getAt = fmap (boolSeqToValue :: [Bool] -> Int32)
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
  runWithSymbolicMonadState $ \sms -> do
    let sbe = symbolicBackend sms
    let be = smsBitEngine sms
    [idx, val]  <- replicateM 2 $ IValue <$> freshInt sbe
    rslt <- runDefSimulator cb sbe $ do
      let tint = mkCInt 32 . fromIntegral
      arr <- newIntArray intArrayTy $ map tint arrayElems
      [(pd, Nothing)] <- runStaticMethod "Arrays" "update" "(II[I)V"
                           [idx, val, RValue arr]
      getIntArray arr
      -- Overwrite a random index with 42 and check it
    rsltLits <- concatMap toLsbf_lit <$> mapM (getVarLit sbe) rslt
    ((:[]) . elem 42)
      <$> (map (boolSeqToValue :: [Bool] -> Int32) . splitN 32)
      <$> evalAig be (evalAigArgs32 [overwriteIdx, 42]) rsltLits

-- | Symbolic array update w/ concrete index and symbolic value
sa3 :: TrivialProp
sa3 cb =
  runWithSymbolicMonadState $ \sms -> do
    let sbe = symbolicBackend sms
    let be = smsBitEngine sms
    let n    = 3
        fill = 99
    val     <- IValue <$> freshInt sbe
    symVals <- replicateM n $ freshInt sbe
    rslt <- runDefSimulator cb sbe $ do
      arr <- newIntArray intArrayTy symVals
      [(pd, Nothing)] <-
        runStaticMethod "Arrays" "update" "(II[I)V"
          [IValue (mkCInt 32 $ fromIntegral n - 1), val, RValue arr]
      getIntArray arr
    -- Overwrite the last index with 42 and check it
    rsltLits <- concatMap toLsbf_lit <$> mapM (getVarLit sbe) rslt
    ((:[]) . (==) (replicate (n-1) fill ++ [42]))
        <$> (map boolSeqToValue . splitN 32)
        <$> evalAig be (evalAigArgs32 (42 : replicate n fill)) rsltLits

-- | Symbolic 2-dim array update w/ concrete index and value
sa4 :: TrivialProp
sa4 cb =
  runWithSymbolicMonadState $ \sms -> do 
    let sbe = symbolicBackend sms
    let be = smsBitEngine sms
    let n        = 3 ; nI = fromIntegral n
        m        = 4
        numElems = n * m
        fill     = 99
    symVals <- replicateM n $ replicateM m $ freshInt sbe
    rslt <- runDefSimulator cb sbe $ do
      let tint = mkCInt 32
      inners <- mapM (newIntArray intArrayTy) symVals
      twodim <- newMultiArray (ArrayType intArrayTy) [tint nI]
      forM_ (map (tint . fromIntegral) [0..nI] `zip` map RValue inners) $
        uncurry (setArrayValue twodim)
      [(pd, Nothing)] <-
        runStaticMethod "Arrays" "update" "(III[[I)V"
          (map (IValue . tint) [0, 0, 42] ++ [RValue twodim])
      concat <$> (mapM getIntArray =<< getRefArray twodim)
    -- Overwrite the first index with 42 and check it
    rsltLits <- concatMap toLsbf_lit <$> mapM (getVarLit sbe) rslt
    ((:[]) . (==) ((42 :: Int32) : replicate (numElems - 1) (fromIntegral fill)))
        <$> (map boolSeqToValue . splitN 32)
        <$> evalAig be (concatMap intToBoolSeq $ replicate numElems (mkCInt 32 fill)) rsltLits

--------------------------------------------------------------------------------
-- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests arrayTests
