{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : atomb
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

module Tests.PrimOps (primOpTests) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Int

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic as QC

import Tests.Common

primOpTests :: Codebase -> TestTree
primOpTests cb = testGroup "PrimOps" $
   [ -- 32b tests over all symbolic backends, as configured below
     testCase "32b int and" $ t1 cb
   , testCase "32b int add" $ t2 cb
   , testCase "byte array sum" $ t3 cb
   , testCase "32b int sub" $ t4 cb
   , testCase "data-dependent branch (simple)" $ t12a cb
   , testCase "data-dependent branch (nested)" $ t12b cb
   , testCase "data-dependent branch (loop)" $ t12c cb
   , testProperty "32b quotRem: dag and aig eval" $ monadicIO $ qr32
     -- 64b tests over all symbolic backends, as configured below
   , testCase "64b int and" $ t7 cb
   , testCase "64b int add" $ t8 cb
   , testCase "64b int array sum" $ t9 cb
   , testProperty "64b quotRem: dag and aig eval" $ monadicIO $ qr64
   , testProperty "string instantiation & simple string ops" $ monadicIO $ ct2 cb
   , testCase "32b int array out parameter" $ t13 cb
   , testCase "superclass field assignment from subclass method" $ ct1 cb
   , testCase "concrete double add" $ fp1 cb
   , testCase "concrete double sub" $ fp2 cb
   , testCase "concrete double mul" $ fp3 cb
   , testCase "concrete double div" $ fp4 cb
   , testCase "concrete float add" $ fp5 cb
   , testCase "concrete float sub" $ fp6 cb
   , testCase "concrete float mul" $ fp7 cb
   , testCase "concrete float div" $ fp8 cb
   ]
  where
    qr32 =
      chkQuotRem
        ("Trivial", "int_f5", "(II)I") -- 32b quot java impl
        ("Trivial", "int_f6", "(II)I") -- 32b rem java impl
        (evalBinOp32 cb)
    qr64 =
      chkQuotRem
        ("Trivial", "long_f5", "(JJ)J") -- 64b quot java impl
        ("Trivial", "long_f6", "(JJ)J") -- 64b rem java impl
        (evalBinOp64 cb)
--------------------------------------------------------------------------------
-- "trivial" tests

mkBinOpTest32 :: Codebase
              -> MethodSpec
              -> (Int32 -> Int32 -> Int32)
              -> [(Int32, Int32)]
              -> Assertion
mkBinOpTest32 cb ms gnd inps = do
  forM_ inps $ \(x,y) -> do
    dagEval <- evalBinOp32 cb ms x y
    dagEval @?= mkCInt 32 (fromIntegral (x `gnd` y))

mkBinOpTest64 :: Codebase
              -> MethodSpec
              -> (Int64 -> Int64 -> Int64)
              -> [(Int64, Int64)]
              -> Assertion
mkBinOpTest64 cb ms gnd inps = do
  forM_ inps $ \(x,y) -> do
    dagEval <- evalBinOp64 cb ms x y
    dagEval @?= mkCInt 64 (fromIntegral (x `gnd` y))

t1 :: TrivialCase
t1 cb = mkBinOpTest32 cb ("Trivial", "bool_f1", "(ZZ)Z") (.&.) [(0,0), (0,1), (1,0), (1,1)]

t2 :: TrivialCase
t2 cb = mkBinOpTest32 cb ("Trivial", "int_f2", "(II)I") (+) [(0,0), (0,1), (1,0), (1,1), (8192,8192)]

t3 :: TrivialCase
t3 cb =  
  mkSymAssertion $ \sbe -> do
    syms <- replicateM 4 $ freshByte sbe
    [(_,Just (IValue rsltTerm))] <- runDefSimulator cb sbe $ do
      arr <- newIntArray (ArrayType ByteType) syms
      runStaticMethod "Trivial" "byte_array_f3" "([B)B" [RValue arr]
    let inps = [[4,4,4,4], [1,2,3,4], [63,-42,1,1]]
    forM_ inps $ \inp -> do
      evalFn <- concreteEvalFn (V.map constInt (V.fromList inp))
      rslt   <- evalFn rsltTerm
      constInt (sum inp) @=? rslt

t4 ::TrivialCase
t4 cb = mkBinOpTest32 cb ("Trivial", "int_f4", "(II)I") (-)
  [(0,0), (0,1), (7,2), (-16, -5), (1 `shiftL` 31,1)]

t7 :: TrivialCase
t7 cb = mkBinOpTest64 cb ("Trivial", "long_f1", "(JJ)J") (.&.)
  [(0,0), (0,1), (1,0), (1,1)]

t8 :: TrivialCase
t8 cb = mkBinOpTest64 cb ("Trivial", "long_f2", "(JJ)J") (+)
  [(0,0), (0,1), (1,0), (1,1), (8192,8192)]

t9 :: TrivialCase
t9 cb =
  mkSymAssertion $ \sbe -> do
    syms <- replicateM 4 $ freshLong sbe
    [(_,Just (LValue rsltTerm))] <- runDefSimulator cb sbe $ do
      arr <- newLongArray syms
      runStaticMethod "Trivial" "long_array_f3" "([J)J" [RValue arr]
    let inps = [[1,1,1,1], [42,99,99,42], [39203,2033991,2930,2305843009213693951]]
    forM_ inps $ \inp -> do
      evalFn <- concreteEvalFn (V.map constLong (V.fromList inp))
      rslt <- evalFn rsltTerm
      constLong (sum inp) @=? rslt

t12a :: TrivialCase
t12a cb =
  mkSymAssertion $ \sbe -> do
    sym <- freshInt sbe
    [(_,Just (IValue rsltTerm))] <- runDefSimulator cb sbe $ do
      runStaticMethod "Trivial" "fork_f1" "(Z)I" [IValue sym]
    let inps = [[0],[1]]
    forM_ inps $ \[inp] -> do
      evalFn <- concreteEvalFn (V.fromList [constInt inp])
      rslt <- evalFn rsltTerm
      constInt inp @=? rslt

t12cmn
  :: MethodSpec
  -> (Int32 -> Int32 -> CValue -> Assertion)
  -> TrivialCase
t12cmn (cNm, mNm, sig) chk cb = do
  mkSymAssertion $ \sbe -> do
    syms <- replicateM 2 $ IValue <$> freshInt sbe
    [(_,Just (IValue rslt))] <- runDefSimulator cb sbe $ do
      runStaticMethod cNm mNm sig syms
    forM_ [[0,0],[1,0],[0,1],[1,1]] $ \[b0,b1] -> do
      evalFn <- concreteEvalFn (V.map constInt (V.fromList [b0, b1]))
      rsltVal <- evalFn rslt
      chk b0 b1 rsltVal

t12b :: TrivialCase
t12b cb = t12cmn ("Trivial", "fork_f2", "(ZZ)I") chk cb
  where chk b0 b1 rslt = constInt (b0 .|. (b1 `shiftL` 1)) @=? rslt

t12c :: TrivialCase
t12c cb = t12cmn ("Trivial", "fork_loop_f2", "(ZZ)I") chk cb
  where chk b0 b1 rslt = constInt ((b0 .|. (b1 `shiftL` 1)) * 2) @=? rslt

t13 :: TrivialCase
t13 cb =
  mkAssertionWithSMS $ \sms -> do
    let sbe = symbolicBackend sms
        be = smsBitEngine sms
    let cInputs = [constInt 16, constInt 4]
        expect  = map constInt [4, 64, 4, 8]
        n       = length expect
    ins <- replicateM 2 $ IValue <$> freshInt sbe
    outVars <- runDefSimulator cb sbe $ do
      outArr <- newMultiArray (ArrayType IntType) [mkCInt 32 4]
      _  <- runStaticMethod "Trivial" "out_array" "(II[I)V" (ins ++ [RValue outArr])
      getIntArray outArr
    -- DAG eval
    evalFn <- concreteEvalFn (V.fromList cInputs)
    outVals <- mapM evalFn outVars

    -- AIG eval
    outLits <- fmap (map flattenLitResult) $ mapM (smsBitBlastFn sms) outVars
    r <- beEvalAigV be (SV.fromList $ concatMap intToBoolSeq cInputs)
                       (SV.concat outLits)
    let rs = [ constInt . head . hexToIntSeq . boolSeqToHex
               $ SV.toList $ (SV.slice (32*k) 32 r)
             | k <- [0..(n-1)] ]
    [outVals, rs] @?= [expect, expect]


-- NB: This won't symbolically terminate yet.
_t14 :: TrivialCase
_t14 cb = mkSymAssertion $ \sbe -> do
    a <- freshInt sbe
    [(_,Just (IValue _))] <- 
       runDefSimulator cb sbe $
         runStaticMethod "Trivial" "loop1" "(I)I" [IValue a]
    return ()
    -- putStrLn . render $ 
    --   "t14: rs =" <+> integer (p^.pathName) <+> "=>" <+> prettyTermD sbe x
    -- return [True]

--------------------------------------------------------------------------------
-- Class tests

-- | Check assignment in a virtual subclass method to a protected superclass
-- field
ct1 :: TrivialCase
ct1 cb = mkSymAssertion $ \sbe -> do
  outVars <- runDefSimulator cb sbe $ do
    outArr <- newMultiArray (ArrayType IntType) [mkCInt 32 2]
    _ <- runStaticMethod "IVTDriver" "go" "([I)V" [RValue outArr]
    getIntArray outArr
  evalFn <- concreteEvalFn V.empty
  outVals <- mapM evalFn outVars
  [constInt 42, constInt 42] @=? outVals

-- | Ensure that refFromString produces a usable string reference
ct2 :: TrivialProp
ct2 cb =
  forAllM (listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z']) $ \str -> do
  mkSymProp $ \sbe -> do
    inp <- termInt sbe (fromIntegral (length str))
    [(_, Just (IValue outVar))] <- runDefSimulator cb sbe $ do
      s <- refFromString str
      runStaticMethod "Trivial" "stringCheck" "(Ljava/lang/String;I)Z"
                      [RValue s, IValue inp]
    evalFn <- concreteEvalFn V.empty
    outVal <- evalFn outVar
    return [outVal == mkCInt 32 1]

--------------------------------------------------------------------------------
-- floating point tests

testDoubleBin :: Codebase
              -> (Double -> Double -> Double)
              -> String
              -> String
              -> Assertion
testDoubleBin cb op method ty = do
  let a = 1.0
      b = 2.0
  mkSymAssertion $ \sbe -> do
    [(_,Just (DValue r))] <-
      runDefSimulator cb sbe $
        runStaticMethod "Trivial" method ty [DValue a, DValue b]
    r @?= op a b

testFloatBin :: Codebase
             -> (Float -> Float -> Float)
             -> String
             -> String
             -> Assertion
testFloatBin cb op method ty = do
  let a = 1.0
      b = 2.0
  mkSymAssertion $ \sbe -> do
    [(_,Just (FValue r))] <- 
      runDefSimulator cb sbe $ do
        runStaticMethod "Trivial" method ty [FValue a, FValue b]
    r @?= op a b

fp1, fp2, fp3, fp4, fp5, fp6, fp7, fp8 :: TrivialCase
fp1 cb = testDoubleBin cb (\a b -> a + b + 3.0) "double_f1" "(DD)D"
fp2 cb = testDoubleBin cb (-) "double_f2" "(DD)D"
fp3 cb = testDoubleBin cb (*) "double_f3" "(DD)D"
fp4 cb = testDoubleBin cb (/) "double_f4" "(DD)D"
fp5 cb = testFloatBin  cb (\a b -> a + b + 3.0) "float_f1"  "(FF)F"
fp6 cb = testFloatBin  cb (-) "float_f2"  "(FF)F"
fp7 cb = testFloatBin  cb (*) "float_f3"  "(FF)F"
fp8 cb = testFloatBin  cb (/) "float_f4"  "(FF)F"

--------------------------------------------------------------------------------
-- quotRem tests

type MethodSpec  = (String, String, String)

chkQuotRem :: (Arbitrary a, Bounded a, Num a, Ord a, Show a)
           => MethodSpec
           -> MethodSpec
           -> (MethodSpec -> a -> a -> IO CValue)
           -> PropertyM IO ()
chkQuotRem quotSpec remSpec eval = do
  forAllM arbitrary $ \d -> do
  forAllM (arbitrary `suchThat` (\v -> v /= 0)) $ \v -> do
    dagQuot <- run $ eval quotSpec d v
    dagRem   <- run $ eval remSpec d v
    let Just dq = fromIntegral <$> getSVal dagQuot
        Just dr = fromIntegral <$> getSVal dagRem
        sameSign     = (d >= 0 && v >= 0) || (d < 0 && v < 0)
        report s = s ++ ": "
                   ++ "d = " ++ show d ++ ", v = " ++ show v
                   ++ ", dq = " ++ show dq ++ ", dr = " ++ show dr
                   ++ ", dq * v + dr = " ++ show (dq * v + dr)
    assertMsg (d == dq * v + dr) $
      report "FAIL (aig eval): d == q * v + r violated"
    -- misc checks for alignment w/ JVM semantics
    if (d == minBound && v == (-1))
     then QC.assert $ dq == d
     else when (abs d >= abs v) $ QC.assert $ if sameSign then dq >= 0 else dq < 0
    assertMsg (abs dr <= abs v)
            $ report "FAIL: Absolute value check failed"
    -- run $ putStrLn $ report " PASS (dag & aig)"

--------------------------------------------------------------------------------
-- Misc utility functions

-- Yields dag eval and aiger eval for the provided binop
evalBinOp32 :: Codebase
            -> MethodSpec
            -> Int32
            -> Int32
            -> IO CValue
evalBinOp32 cb (classNm, methodNm, sig) x y = do
  oc <- mkOpCache          
  withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms
    a <- IValue <$> freshInt sbe
    b <- IValue <$> freshInt sbe
    [(_,Just (IValue rslt))] <- runDefSimulator cb sbe $ do
      setVerbosity verb
      runStaticMethod classNm methodNm sig [a, b]
    let args = V.map (mkCInt 32 . toInteger) (V.fromList [x, y])
    evalFn <- concreteEvalFn args
    evalFn rslt

-- Yields dag eval eval for the provided binop
evalBinOp64 :: Codebase
            -> MethodSpec
            -> Int64
            -> Int64
            -> IO CValue
evalBinOp64 cb (classNm, methodNm, sig) x y = do
  oc <- mkOpCache          
  withSymbolicMonadState oc $ \sms -> do
    let sbe = symbolicBackend sms
    a <- LValue <$> freshLong sbe
    b <- LValue <$> freshLong sbe
    [(_,Just (LValue rslt))] <- runDefSimulator cb sbe $ do
      setVerbosity verb
      runStaticMethod classNm methodNm sig [a, b]
    let args = V.map (mkCInt 64 . toInteger) (V.fromList [x, y])
    evalFn <- concreteEvalFn args
    evalFn rslt

--------------------------------------------------------------------------------
-- Scratch

-- _ignore_nouse :: a
-- _ignore_nouse = undefined main

--main :: IO ()
--main = do cb <- commonLoadCB
--          defaultMain [primOpTests cb]

