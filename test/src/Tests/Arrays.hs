{- |
Module           : $Header$
Description      :
License          : BSD3
Stability        : provisional
Point-of-contact : atomb
-}
{-# LANGUAGE CPP #-}

module Tests.Arrays (arrayTests) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import Data.Int

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.QuickCheck.Monadic

import Tests.Common


arrayTests :: Codebase -> TestTree
arrayTests cb = testGroup "Arrays" $
  [ testPropertyN 20 "Read concarray @ symidx" $ sa1 cb
  , testPropertyN 20 "Write symelem @ symidx to concarray" $ sa2 cb
  , testCase "Write symelem @ concidx to concarray" $ sa3 cb
  , testCase "Write symelem @ concidx to 2-dim symarray" $ sa4 cb
  ]


-- | Read concrete int array at symbolic indices
sa1 :: TrivialProp
sa1 cb =
  forAllM (int32s 128) $ \arrayElems ->
    mkPropWithSMS $ \sms -> do
      let sbe = symbolicBackend sms
      idx <- IValue <$> freshInt sbe
      outVar <- runDefSimulator cb sbe $ do
        let tint = mkCInt 32 . fromIntegral
        inpArr <- newIntArray intArrayTy $ map tint arrayElems
        [(_, Just (IValue rslt))] <-
          runStaticMethod "Arrays" "index" "(I[I)I" [idx, RValue inpArr]
        return rslt
      let getAt x = do
               x' <- termInt sbe x
               z <- evalAigIntegral sbe id [x'] outVar
               maybe (assertFailure "result is not a concrete integer" >> return 0) return $ asInt sbe z

      ((:[]) . (== arrayElems))
        <$> mapM getAt [0..(fromIntegral $ length arrayElems - 1)]

-- | Concrete int array update w/ symbolic index & symbolic value
sa2 :: TrivialProp
sa2 cb =
  forAllM (int32s 128 `suchThat` (not . elem 42))            $ \arrayElems ->
  forAllM (choose (0, fromIntegral (length arrayElems - 1))) $ \overwriteIdx ->
  mkPropWithSMS $ \sms -> do
    let sbe = symbolicBackend sms
    [idx, val]  <- replicateM 2 $ IValue <$> freshInt sbe
    rslt <- runDefSimulator cb sbe $ do
      let tint = mkCInt 32 . fromIntegral
      arr <- newIntArray intArrayTy $ map tint arrayElems
      [(_, Nothing)] <- runStaticMethod "Arrays" "update" "(II[I)V"
                          [idx, val, RValue arr]
      getIntArray arr

    -- Overwrite a random index with 42 and check it
    oidx <- termInt sbe overwriteIdx
    oval <- termInt sbe 42
    arr <- evalAigArray sbe 32 [oidx,oval] rslt
    arr' <- mapM (maybe (assertFailure "result is not a concrete integer" >> return 0) return . asInt sbe) arr
    return [elem 42 arr']


-- | Symbolic array update w/ concrete index and symbolic value
sa3 :: Codebase -> Assertion
sa3 cb =
  mkAssertionWithSMS $ \sms -> do
    let sbe = symbolicBackend sms
    let n    = 3
        fill = 99
    val     <- IValue <$> freshInt sbe
    symVals <- replicateM n $ freshInt sbe
    rslt <- runDefSimulator cb sbe $ do
      arr <- newIntArray intArrayTy symVals
      [(_, Nothing)] <-
        runStaticMethod "Arrays" "update" "(II[I)V"
          [IValue (mkCInt 32 $ fromIntegral n - 1), val, RValue arr]
      getIntArray arr

    -- Overwrite the last index with 42 and check it
    xs <- mapM (termInt sbe) (42 : replicate n fill)
    arr <- evalAigArray sbe 32 xs rslt
    arr' <- mapM (maybe (assertFailure "result is not a concrete integer" >> return 0) return . asInt sbe) arr

    (@=?) (replicate (n-1) fill ++ [42]) arr'


-- | Symbolic 2-dim array update w/ concrete index and value
sa4 :: Codebase -> Assertion
sa4 cb =
  mkAssertionWithSMS $ \sms -> do
    let sbe = symbolicBackend sms
    let n        = 3 ; nI = fromIntegral n
        m        = 4
        numElems = n * m
        fill     = 99 :: Int32
    symVals <- replicateM n $ replicateM m $ freshInt sbe
    rslt <- runDefSimulator cb sbe $ do
      let tint = mkCInt 32
      inners <- mapM (newIntArray intArrayTy) symVals
      twodim <- newMultiArray (ArrayType intArrayTy) [tint nI]
      forM_ (map (tint . fromIntegral) [0..nI] `zip` map RValue inners) $
        uncurry (setArrayValue twodim)
      [(_, Nothing)] <-
        runStaticMethod "Arrays" "update" "(III[[I)V"
          (map (IValue . tint) [0, 0, 42] ++ [RValue twodim])
      concat <$> (mapM getIntArray =<< getRefArray twodim)

    xs <- mapM (termInt sbe) (42 : replicate (numElems - 1) fill)
    arr <- evalAigArray sbe 32 xs rslt
    arr' <- mapM (maybe (assertFailure "result is not a concrete integer" >> return 0) return . asInt sbe) arr
    (@=?) ((42 :: Int32) : replicate (numElems - 1) fill) arr'
