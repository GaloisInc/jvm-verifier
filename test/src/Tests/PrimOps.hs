{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.PrimOps (primOpTests) where

import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Int
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

import JavaParser (Type(..))
import Simulation hiding (run)
import Tests.Common
import Utils

import Verinf.Symbolic

verb :: Int
verb = 0

-- NB: This whole module could probably use a rewrite; with the move to type
-- families and attempted support for multiple symbolic backends, it's become
-- somewhat of a poster child for refactoring gone wrong. -- js 06 Jan 2011

primOpTests :: [(Args, Property)]
primOpTests =
   [ -- 32b tests over all symbolic backends, as configured below
     test1 t1                  "32b int and"
   , test1 t2                  "32b int add"
   , test1 t3                  "byte array sum"
   , test1 t4                  "32b int sub"
   , test1 t12a                "data-dependent branch (simple)"
   , test1 t12b                "data-dependent branch (nested)"
   , test1 t12c                "data-dependent branch (loop)"
   , test 10 False qr32        "32b quotRem: dag and aig eval"
     -- 64b tests over all symbolic backends, as configured below
   , test1 t7                  "64b int and"
   , test1 t8                  "64b int add"
   , test1 t9                  "64b int array sum"
   , test 10 False qr64        "64b quotRem: dag and aig eval"
   , test 10 False ct2                 "string instantiation & simple string ops"
   , test1 t13                         "32b int array out parameter"
   , test1 ct1                         "superclass field assignment from subclass method"
   , test1 fp1                         "concrete double add"
   , test1 fp2                         "concrete double sub"
   , test1 fp3                         "concrete double mul"
   , test1 fp4                         "concrete double div"
   , test1 fp5                         "concrete float add"
   , test1 fp6                         "concrete float sub"
   , test1 fp7                         "concrete float mul"
   , test1 fp8                         "concrete float div"
   ]
  where
    qr32 cb =
      chkQuotRem
        ("Trivial", "int_f5", "(II)I") -- 32b quot java impl
        ("Trivial", "int_f6", "(II)I") -- 32b rem java impl
        (evalBinOp32 cb)
    qr64 cb =
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
                 -> PropertyM IO ()
mkBinOpTest32 cb ms gnd inps = do
  forM_ inps $ \(x,y) -> do
    dagEval <- run $ evalBinOp32 cb ms x y
    assert (dagEval == mkCInt 32 (fromIntegral (x `gnd` y)))

mkBinOpTest64 ::Codebase
              -> MethodSpec
              -> (Int64 -> Int64 -> Int64)
              -> [(Int64, Int64)]
              -> PropertyM IO ()
mkBinOpTest64 cb ms gnd inps = do
  forM_ inps $ \(x,y) -> do
    dagEval <- run $ evalBinOp64 cb ms x y
    assert (dagEval == mkCInt 64 (fromIntegral (x `gnd` y)))

t1 :: TrivialProp
t1 cb = mkBinOpTest32 cb ("Trivial", "bool_f1", "(ZZ)Z") (.&.) [(0,0), (0,1), (1,0), (1,1)]

t2 :: TrivialProp
t2 cb = mkBinOpTest32 cb ("Trivial", "int_f2", "(II)I") (+) [(0,0), (0,1), (1,0), (1,1), (8192,8192)]

t3 :: TrivialProp
t3 cb =  
  runSymTest $ \sbe -> do
    syms <- replicateM 4 $ freshByte sbe
    [(_,ReturnVal (IValue rsltTerm))] <- runDefSimulator sbe cb $ do
      setVerbosity verb
      arr <- newIntArray (ArrayType ByteType) syms
      runStaticMethod "Trivial" "byte_array_f3" "([B)B" [RValue arr]
    let inps = [[4,4,4,4], [1,2,3,4], [63,-42,1,1]]
    forM inps $ \inp -> do
      evalFn <- concreteEvalFn (V.map constInt (V.fromList inp))
      rslt   <- evalFn rsltTerm
      return $ constInt (sum inp) == rslt

t4 ::TrivialProp
t4 cb = mkBinOpTest32 cb ("Trivial", "int_f4", "(II)I") (-)
  [(0,0), (0,1), (7,2), (-16, -5), (1 `shiftL` 31,1)]

t7 :: TrivialProp
t7 cb = mkBinOpTest64 cb ("Trivial", "long_f1", "(JJ)J") (.&.)
  [(0,0), (0,1), (1,0), (1,1)]

t8 :: TrivialProp
t8 cb = mkBinOpTest64 cb ("Trivial", "long_f2", "(JJ)J") (+)
  [(0,0), (0,1), (1,0), (1,1), (8192,8192)]

t9 :: TrivialProp
t9 cb =
  runSymTest $ \sbe -> do
    syms <- replicateM 4 $ freshLong sbe
    [(_,ReturnVal (LValue rsltTerm))] <- runDefSimulator sbe cb $ do
      setVerbosity verb
      arr <- newLongArray syms
      runStaticMethod "Trivial" "long_array_f3" "([J)J" [RValue arr]
    let inps = [[1,1,1,1], [42,99,99,42], [39203,2033991,2930,2305843009213693951]]
    forM inps $ \inp -> do
      evalFn <- concreteEvalFn (V.map constLong (V.fromList inp))
      rslt <- evalFn rsltTerm
      return $ constLong (sum inp) == rslt

t12a :: TrivialProp
t12a cb =
  runSymTest $ \sbe -> do
    sym <- freshInt sbe
    [(_,ReturnVal (IValue rsltTerm))] <- runDefSimulator sbe cb $ do
      setVerbosity verb
      runStaticMethod "Trivial" "fork_f1" "(Z)I" [IValue sym]
    let inps = [[0],[1]]
    forM inps $ \[inp] -> do
      evalFn <- concreteEvalFn (V.fromList [constInt inp])
      rslt <- evalFn rsltTerm
      return $ constInt inp == rslt

t12cmn
  :: MethodSpec
  -> (Int32 -> Int32 -> CValue -> Bool)
  -> TrivialProp
t12cmn (cNm, mNm, sig) chk cb = do
  runSymTest $ \sbe -> do
    syms <- replicateM 2 $ IValue <$> freshInt sbe
    [(_,ReturnVal (IValue rslt))] <- runDefSimulator sbe cb $ do
      setVerbosity verb
      runStaticMethod cNm mNm sig syms
    forM [[0,0],[1,0],[0,1],[1,1]] $ \[b0,b1] -> do
      evalFn <- concreteEvalFn (V.map constInt (V.fromList [b0, b1]))
      rsltVal <- evalFn rslt
      return $ chk b0 b1 rsltVal

t12b :: TrivialProp
t12b cb = t12cmn ("Trivial", "fork_f2", "(ZZ)I") chk cb
  where chk b0 b1 rslt = constInt (b0 .|. (b1 `shiftL` 1)) == rslt

t12c :: TrivialProp
t12c cb = t12cmn ("Trivial", "fork_loop_f2", "(ZZ)I") chk cb
  where chk b0 b1 rslt = constInt ((b0 .|. (b1 `shiftL` 1)) * 2) == rslt

t13 :: TrivialProp
t13 cb =
  runWithSymbolicMonadState $ \sms -> do
    let sbe = symbolicBackend sms
        be = smsBitEngine sms
    let n       = 4
        cInputs = [constInt 16, constInt 4]
        expect  = map constInt [4, 64, 4, 8]
    ins <- replicateM 2 $ IValue <$> freshInt sbe
    outVars <- runDefSimulator sbe cb $ do
      setVerbosity verb
      outArr <- newMultiArray (ArrayType IntType) [mkCInt (Wx 32) 4]
      [(pd, Terminated)] <-
        withoutExceptions <$>
          runStaticMethod "Trivial" "out_array" "(II[I)V"
                          (ins ++ [RValue outArr])
      getIntArray pd outArr
    -- DAG eval
    evalFn <- concreteEvalFn (V.fromList cInputs)
    outVals <- mapM evalFn outVars
    -- AIG eval
    outLits <- mapM (getVarLit sbe) outVars
    r <- beEvalAigV be (SV.fromList $ concatMap intToBoolSeq cInputs)
                       (SV.fromList $ concatMap toLsbf_lit outLits)
    let rs = [ constInt . head . hexToIntSeq . boolSeqToHex
               $ SV.toList $ (SV.slice (32*k) 32 r)
             | k <- [0..(n-1)] ]
    return [outVals == expect && rs == expect]

-- NB: This won't symbolically terminate yet.
_t14 :: TrivialProp
_t14 cb = runSymTest $ \sbe -> do
    a <- freshInt sbe
    [(p,ReturnVal (IValue x))] <- 
       runDefSimulator sbe cb $ do
        setVerbosity verb
        runStaticMethod "Trivial" "loop1" "(I)I" [IValue a]
    putStrLn $ "t14: rs = " ++ show p ++ " => " ++ prettyTerm x
    return [True]

--------------------------------------------------------------------------------
-- Class tests

-- | Check assignment in a virtual subclass method to a protected superclass
-- field
ct1 :: TrivialProp
ct1 cb = runSymTest $ \sbe -> do
  outVars <- runDefSimulator sbe cb $ do
    outArr <- newMultiArray (ArrayType IntType) [mkCInt 32 2]
    [(pd, Terminated)] <-
      runStaticMethod "IVTDriver" "go" "([I)V" [RValue outArr]
    getIntArray pd outArr
  evalFn <- concreteEvalFn V.empty
  outVals <- mapM evalFn outVars
  return $ [[constInt 42, constInt 42] == outVals]

-- | Ensure that refFromString produces a usable string reference
ct2 :: TrivialProp
ct2 cb =
  forAllM (listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z']) $ \str -> do
  runSymTest $ \sbe -> do
    inp <- termInt sbe (fromIntegral (length str))
    [(_, ReturnVal (IValue outVar))] <- runDefSimulator sbe cb $ do
      s <- refFromString str
      runStaticMethod "Trivial" "stringCheck" "(Ljava/lang/String;I)Z"
                      [RValue s, IValue inp]
    evalFn <- concreteEvalFn V.empty
    outVal <- evalFn outVar
    return [boolFromConst outVal]

--------------------------------------------------------------------------------
-- floating point tests

testDoubleBin :: Codebase
              -> (Double -> Double -> Double)
              -> String
              -> String
              -> PropertyM IO ()
testDoubleBin cb op method ty = do
  let a = 1.0
      b = 2.0
  runSymTest $ \sbe -> do
    [(_,ReturnVal (DValue r))] <-
      runDefSimulator sbe cb $
        runStaticMethod "Trivial" method ty [DValue a, DValue b]
    return [r == op a b]

testFloatBin :: Codebase
             -> (Float -> Float -> Float)
             -> String
             -> String
             -> PropertyM IO ()
testFloatBin cb op method ty = do
  let a = 1.0
      b = 2.0
  runSymTest $ \sbe -> do
    [(_,ReturnVal (FValue r))] <- 
      runDefSimulator sbe cb $ do
        runStaticMethod "Trivial" method ty [FValue a, FValue b]
    return [r == op a b]

fp1, fp2, fp3, fp4, fp5, fp6, fp7, fp8 :: TrivialProp
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
     then assert $ dq == d
     else when (abs d >= abs v) $ assert $ if sameSign then dq >= 0 else dq < 0
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
    [(_,ReturnVal (IValue rslt))] <- runDefSimulator sbe cb $ do
      setVerbosity verb
      withoutExceptions <$>
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
    [(_,ReturnVal (LValue rslt))] <- runDefSimulator sbe cb $ do
      setVerbosity verb
      withoutExceptions <$>
        runStaticMethod classNm methodNm sig [a, b]
    let args = V.map (mkCInt 64 . toInteger) (V.fromList [x, y])
    evalFn <- concreteEvalFn args
    evalFn rslt

_containsExc :: Monad m => [FinalResult (MonadTerm m)] -> String -> m ()
_containsExc frs s = flip CE.assert (return ()) $
  any (\fr -> case fr of
                Exc (JavaException (Ref _ (ClassType s')) _) -> s == s'
                _ -> False
      )
      frs

--------------------------------------------------------------------------------
-- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests primOpTests

