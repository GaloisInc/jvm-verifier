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
import Control.Monad.Trans
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Int
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Monadic

import JavaParser (Type(..))
import Simulation hiding (run, newLongArray)
import qualified Simulation
import Tests.Common
import Utils
import Utils.Simulation

import Verinf.Symbolic

type TestInput = [CValue]

type Test sym a =
     Codebase
  -> Maybe String                                               -- ^ aig filename to write, if any
  -> MethodSpec                                                 -- ^ method under test
  -> sym [MonadTerm sym]                                        -- ^ symbolic term creator
  -> ([MonadTerm sym] -> Simulator sym [Value (MonadTerm sym)]) -- ^ function argument creator
  -> (TestInput -> [Bool])                                      -- ^ concrete input => aig input
  -> (TestInput -> CValue -> Bool)                              -- ^ correct dag eval predicate
  -> (TestInput -> [Bool] -> Bool)                              -- ^ correct aig eval predicate
  -> [TestInput]                                                -- ^ concrete inputs
  -> PropertyM IO ()

type RunIO sym c = sym (CValue, c) -> IO (CValue, c)
type RunTest sym = sym [Bool] -> PropertyM IO ()

verb :: Int
verb = 0

-- A "symbolic backend testable" typeclass alias to clean up signatures a bit
class (AigOps sym, TermDagMonad sym) => SBETestable sym
instance SBETestable SymbolicMonad

-- NB: This whole module could probably use a rewrite; with the move to type
-- families and attempted support for multiple symbolic backends, it's become
-- somewhat of a poster child for refactoring gone wrong. -- js 06 Jan 2011

type RunSymTest a = (SymbolicMonad a -> IO a, SymbolicMonad [Bool] -> PropertyM IO ())

primOpTests :: [(Args, Property)]
primOpTests =
  -- 32b tests over all symbolic backends, as configured below
  (runBEs $ \(rio, rt) ->
    [
      test1 (t1 rio)            "32b int and"
    , test1 (t2 rio)            "32b int add"
    , test1 (t3 rt)             "byte array sum"
    , test1 (t4 rio)            "32b int sub"
    , test1 (t12a rt)           "data-dependent branch (simple)"
    , test1 (t12b rt)           "data-dependent branch (nested)"
    , test1 (t12c rt)           "data-dependent branch (loop)"
    , test 10 False (qr32 rio)  "32b quotRem: dag and aig eval"
    ]
  )
  ++
  -- 64b tests over all symbolic backends, as configured below
  (runBEs $ \(rio, rt) ->
    [
      test1 (t7 rio)            "64b int and"
    , test1 (t8 rio)            "64b int add"
    , test1 (t9 rt)             "64b int array sum"
    , test 10 False (qr64 rio ) "64b quotRem: dag and aig eval"
    ]
  )
  ++
  [
    test 10 False ct2                 "string instantiation & simple string ops"
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
    -- NB: REVISIT: The poorly named "runTest" and "runSymbolic" functions are
    -- what ground to the default symbolic backend; we'll want better names when
    -- we support multiple symbolic backends (runSymbolicWordRep,
    -- runSymbolicBitBlast or somesuch, perhaps, etc.)
    runBEs :: forall a b. (RunSymTest a -> [b]) -> [b]
    runBEs = (`concatMap` symbolicBackendRunFns)
    --
    symbolicBackendRunFns :: [RunSymTest a]
    symbolicBackendRunFns = [(\m -> flip runSymbolic m =<< mkOpCache, runTest)]
    --
    qr32 runIO cb =
      chkQuotRem
        cb
        ("Trivial", "int_f5", "(II)I") -- 32b quot java impl
        ("Trivial", "int_f6", "(II)I") -- 32b rem java impl
        (evalInt32BinOp runIO "qr32" Nothing)
    qr64 runIO cb =
      chkQuotRem
        cb
        ("Trivial", "long_f5", "(JJ)J") -- 64b quot java impl
        ("Trivial", "long_f6", "(JJ)J") -- 64b rem java impl
        (evalInt64BinOp runIO "qr64" Nothing)

--------------------------------------------------------------------------------
-- "trivial" tests

mkBinOpTest :: Eq a =>
               Codebase
            -> MethodSpec
            -> (a -> a -> a)
            -> EvalBinOp a
            -> (a -> CValue)
            -> [(a, a)]
            -> PropertyM IO ()
mkBinOpTest cb ms gnd eval ctor inps = runTest $ do
  forM inps $ \(i1,i2) -> do
    (dagEval, aigEval) <- liftIO $ eval cb ms i1 i2
    return (dagEval == ctor (i1 `gnd` i2) && aigEval == (i1 `gnd` i2))

mkTest ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym
  -> String
  -> TakeSingleton (MonadTerm sym) (MonadTerm sym)
  -> Test sym a
mkTest runTest' _lbl getRslt cb maigNm (cNm, mNm, sig)
       mkSyms mkArgs toAigInps prDag prAig inps =
  runTest' $ do
    syms       <- mkSyms
    Right rslt <- runSimulator cb $ do
       setVerbosity verb
       snd . getRslt . withoutExceptions
         <$> (runStaticMethod cNm mNm sig =<< mkArgs syms)
    -- dbugM $ "mkTest: rslt = " ++ show rslt
    outIntLit <- toLsbfV <$> getVarLit rslt
    chks0 <- forM inps $ \inp -> do
       evalFn <- mkConcreteEval (V.fromList inp)
       return $ prDag inp (evalFn rslt)
    be <- getBitEngine
    liftIO $ do
      case maigNm of
        Nothing -> return ()
        Just nm -> beWriteAigerV be nm outIntLit
      chks1 <- liftIO $ forM inps $ \inp ->
        prAig inp . SV.toList
          <$> beEvalAigV be (SV.fromList (toAigInps inp)) outIntLit
      return (chks0 ++ chks1)

mkIntTest ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym -> String -> Test sym Int32
mkIntTest runTest' s = mkTest runTest' s takeIntRslt

mkLongTest ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym -> String -> Test sym Int64
mkLongTest runTest' s = mkTest runTest' s takeLongRslt

t1 ::
  ( AigOps sym
  , MonadTerm sym ~ Node
  , TermDagMonad sym
  )
  => RunIO sym Int32 -> TrivialProp
t1 runIO cb = mkBinOpTest cb ("Trivial", "bool_f1", "(ZZ)Z") (.&.)
  (evalInt32BinOp runIO "t1" $ Just "t1.aig")
  constInt
  [(0,0), (0,1), (1,0), (1,1)]

t2 ::
  ( AigOps sym
  , MonadTerm sym ~ Node
  , TermDagMonad sym
  )
  => RunIO sym Int32 -> TrivialProp
t2 runIO cb = mkBinOpTest cb ("Trivial", "int_f2", "(II)I") (+)
  (evalInt32BinOp runIO "t2" $ Just "t2.aig")
  constInt
  [(0,0), (0,1), (1,0), (1,1), (8192,8192)]

t3 ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym -> TrivialProp
t3 rt cb = mkIntTest rt "t3" cb (Just "t3.aig") ("Trivial", "byte_array_f3", "([B)B")
  (replicateM 4 freshByte)
  (singletonArg RValue . newIntArray (ArrayType ByteType))
  (concatMap (take 8 . intToBoolSeq))
  (chk intFromConst)
  (chk boolSeqToInt32)
  (map intInputs [[4,4,4,4], [1,2,3,4], [63,-42,1,1]])
  where
    chk :: (t -> Int32) -> [CValue] -> t -> Bool
    chk cvt inp rslt = sum (map intFromConst inp) == cvt rslt

t4 ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunIO sym Int32
  -> TrivialProp
t4 runIO cb = mkBinOpTest cb ("Trivial", "int_f4", "(II)I") (-)
  (evalInt32BinOp runIO "t4" $ Just "t4.aig")
  constInt
  [(0,0), (0,1), (7,2), (-16, -5), (1 `shiftL` 31,1)]

t7 ::
  ( AigOps sym
  , MonadTerm sym ~ Node
  , TermDagMonad sym
  )
  => RunIO sym Int64
  -> TrivialProp
t7 runIO cb = mkBinOpTest cb ("Trivial", "long_f1", "(JJ)J") (.&.)
  (evalInt64BinOp runIO "t7" $ Just "t7.aig")
  constLong
  [(0,0), (0,1), (1,0), (1,1)]

t8 ::
  ( AigOps sym
  , MonadTerm sym ~ Node
  , TermDagMonad sym
  )
  => RunIO sym Int64
  -> TrivialProp
t8 runIO cb = mkBinOpTest cb ("Trivial", "long_f2", "(JJ)J") (+)
  (evalInt64BinOp runIO "t8" $ Just "t8.aig")
  constLong
  [(0,0), (0,1), (1,0), (1,1), (8192,8192)]

t9 ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym -> TrivialProp
t9 rt cb = mkLongTest rt "t9" cb (Just "t9.aig") ("Trivial", "long_array_f3", "([J)J")
  (replicateM 4 freshLong)
  (singletonArg RValue . newLongArray)
  (concatMap intToBoolSeq)
  (chk longFromConst)
  (chk boolSeqToInt64)
  (map longInputs [[1,1,1,1], [42,99,99,42], [39203,2033991,2930,2305843009213693951]])
  where
    chk :: (t -> Int64) -> [CValue] -> t -> Bool
    chk cvt inp rslt = sum (map longFromConst inp) == cvt rslt

t12a ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym -> TrivialProp
t12a rt cb = mkIntTest rt "t12a" cb Nothing ("Trivial", "fork_f1", "(Z)I")
  (replicateM 1 freshInt)
  (return . map IValue)
  (concatMap intToBoolSeq)
  (\(~[inp]) rslt -> intFromConst inp == intFromConst rslt)
  (\(~[inp]) rslt -> intFromConst inp == boolSeqToInt32 rslt)
  (map intInputs [[0],[1]])

t12cmn ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym
  -> String
  -> MethodSpec
  -> (Int32 -> Int32 -> Int32 -> Bool)
  -> TrivialProp
t12cmn rt lbl ms chk cb = mkIntTest rt lbl cb Nothing ms
  (replicateM 2 freshInt)
  (return . map IValue)
  (concatMap intToBoolSeq)
  (\(~[b0,b1]) rslt -> chk (intFromConst b0) (intFromConst b1) (intFromConst rslt))
  (\(~[b0,b1]) rslt -> chk (intFromConst b0) (intFromConst b1) (boolSeqToInt32 rslt))
  (map intInputs [[0,0],[1,0],[0,1],[1,1]])

t12b ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym -> TrivialProp
t12b rt cb = t12cmn rt "t12b" ("Trivial", "fork_f2", "(ZZ)I") chk cb
  where chk b0 b1 rslt = b0 .|. (b1 `shiftL` 1) == rslt

t12c ::
  ( SBETestable sym
  , MonadTerm sym ~ Node
  )
  => RunTest sym -> TrivialProp
t12c rt cb = t12cmn rt "t12c" ("Trivial", "fork_loop_f2", "(ZZ)I") chk cb
  where chk b0 b1 rslt = (b0 .|. (b1 `shiftL` 1)) * 2 == rslt

t13 :: TrivialProp
t13 cb = runTest $ do
  let n       = 4
      cInputs = [constInt 16, constInt 4]
      expect  = map constInt [4, 64, 4, 8]
  ins <- map IValue <$> replicateM 2 freshInt
  outVars <- runSimulator cb $ do
    setVerbosity verb
    outArr <- newMultiArray (ArrayType IntType) [mkCInt (Wx 32) 4]
    [(pd, Terminated)] <-
      withoutExceptions
        <$> (do q <- runStaticMethod "Trivial" "out_array" "(II[I)V"
                       (ins ++ [RValue outArr])
                -- dbugM $ "t13 q = " ++ show q
                return q
            )
    getIntArray pd outArr

  -- DAG eval
  evalFn <- mkConcreteEval (V.fromList cInputs)
  -- AIG eval
  outLits <- mapM getVarLit outVars
  be <- getBitEngine
  r <- liftIO $ beEvalAigV be
                           (SV.fromList $ concatMap intToBoolSeq cInputs)
                           (SV.fromList $ concatMap toLsbf_lit outLits)
  let rs = [ constInt . head . hexToIntSeq . boolSeqToHex
             $ SV.toList $ (SV.slice (32*k) 32 r)
           | k <- [0..(n-1)] ]
  return [map evalFn outVars == expect && rs == expect]

-- NB: This won't symbolically terminate yet.
_t14 :: TrivialProp
_t14 cb = runTest $ do
  a <- freshInt
  Right _rslt <- runSimulator cb $ do
    setVerbosity verb
    rs <- runStaticMethod "Trivial" "loop1" "(I)I" [IValue a]
    liftIO $ putStrLn $ "t14: rs = " ++
               unlines (map (\(p,r) -> show p ++ " => " ++ ppFinalResult r) rs)
    CE.assert (length rs == 1) $ return ()
    return (snd . takeIntRslt $ rs)
  return [True]

--------------------------------------------------------------------------------
-- Class tests

-- | Check assignment in a virtual subclass method to a protected superclass
-- field
ct1 :: TrivialProp
ct1 cb = runTest $ do
  outVars <- runSimulator cb $ do
    outArr <- newMultiArray (ArrayType IntType) [mkCInt (Wx 32) 2]
    (pd, Left Terminated) <-
      takeSingleRslt <$> runStaticMethod "IVTDriver"
                                         "go"
                                         "([I)V"
                                         [RValue outArr]
    getIntArray pd outArr
  evalFn <- mkConcreteEval V.empty
  return $ [[constInt 42, constInt 42] == map evalFn outVars]

-- | Ensure that refFromString produces a usable string reference
ct2 :: TrivialProp
ct2 cb =
  forAllM (listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z']) $ \str -> do
  runTest $ do
    outVar <- runSimulator cb $ do
      s <- refFromString str
      (\(~(Right r)) -> r) . snd . takeIntRslt . withoutExceptions
        <$> runStaticMethod "Trivial" "stringCheck" "(Ljava/lang/String;I)Z"
              [RValue s, IValue (mkCInt (Wx 32) . fromIntegral . length $ str)]
    evalFn <- mkConcreteEval V.empty
    return [boolFromConst (evalFn outVar)]

--------------------------------------------------------------------------------
-- floating point tests

testDoubleBin :: Codebase
              -> (Double -> Double -> Double)
              -> String
              -> String
              -> PropertyM IO ()
testDoubleBin cb op method ty =
  let a = 1.0
      b = 2.0 in
  runTest $ runSimulator cb $ do
    r <- snd <$> takeDoubleRslt <$>
         runStaticMethod "Trivial" method ty [DValue a, DValue b]
    return [r == Right (op a b)]

testFloatBin :: Codebase
             -> (Float -> Float -> Float)
             -> String
             -> String
             -> PropertyM IO ()
testFloatBin cb op method ty =
  let a = 1.0
      b = 2.0 in
  runTest $ runSimulator cb $ do
    r <- snd <$> takeFloatRslt <$>
         runStaticMethod "Trivial" method ty [FValue a, FValue b]
    return [r == Right (op a b)]

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
type EvalBinOp a = Codebase -> MethodSpec -> a -> a -> IO (CValue, a)

ssiToNum :: Num a => CValue -> a
ssiToNum (getSVal -> Just c) = fromIntegral c
ssiToNum _ = error $ "internal: Value type is not a num"

chkQuotRem :: (Arbitrary a, Bounded a, Integral a, Num a, Ord a) =>
              Codebase -> MethodSpec -> MethodSpec -> EvalBinOp a -> PropertyM IO ()
chkQuotRem cb quotSpec remSpec eval = do
  forAllM arbitrary $ \d -> do
  forAllM (arbitrary `suchThat` (\v -> v /= 0)) $ \v -> do
    (dagQuot, aigQuot) <- run $ eval cb quotSpec d v
    (dagRem, aigRem)   <- run $ eval cb remSpec d v
    let dq           = ssiToNum dagQuot
        dr           = ssiToNum dagRem
        sameSign     = (d >= 0 && v >= 0) || (d < 0 && v < 0)
        report s = s ++ ": "
                   ++ "d = " ++ show d ++ ", v = " ++ show v
                   ++ ", dq = " ++ show dq ++ ", dr = " ++ show dr
                   ++ ", dq * v + dr = " ++ show (dq * v + dr)
    assertMsg (dq == aigQuot && dr == aigRem) $
      report "FAIL: dag/aig eval result mismatch"
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

-- Yields dag eval and aiger eval for the provided integer 32b binop
evalInt32BinOp ::
  ( AigOps sym
  , MonadTerm sym ~ Node
  , TermDagMonad sym
  )
  => RunIO sym Int32
  -> String
  -> Maybe String
  -> EvalBinOp Int32
evalInt32BinOp runIO lbl maigNm cb =
  evalBinOp runIO lbl cb (Wx 32) maigNm IValue
            getIntegral freshInt (head . hexToIntSeq)
  where getIntegral (IValue v) = v
        getIntegral _          = error "evalInt32BinOp: invalid Value type"

evalInt64BinOp ::
  ( AigOps sym
  , MonadTerm sym ~ Node
  , TermDagMonad sym
  )
  => RunIO sym Int64
  -> String
  -> Maybe String
  -> EvalBinOp Int64
evalInt64BinOp runIO lbl maigNm cb =
  evalBinOp runIO lbl cb (Wx 64) maigNm LValue
            getIntegral freshLong (head . hexToLongSeq)
  where getIntegral (LValue v) = v
        getIntegral _          = error "evalInt64BinOp: invalid Value type"

-- Yields dag eval and aiger eval for the provided binop
evalBinOp ::
  ( AigOps sym
  , Integral a
  , MonadTerm sym ~ Node
  , TermDagMonad sym
  , WordMonad sym
  )
  => (sym (CValue, c) -> IO (CValue, c))
  -> String
  -> Codebase
  -> BitWidth
  -> Maybe String -- aig filename
  -> (b -> Value (MonadTerm sym))
  -> (Value (MonadTerm sym) -> MonadTerm sym)
  -> sym b
  -> ([Char] -> c)
  -> MethodSpec
  -> (a -> a -> IO (CValue, c))
evalBinOp runIO _lbl cb w maigNm mkValue getSymIntegralFromValue newSymVar
          hexToIntegral (classNm, methodNm, sig) x y = do
  runIO $ do
    a <- newSymVar
    b <- newSymVar
    Right val <- runSimulator cb $ do
      setVerbosity verb
      snd . takeSingleRslt . withoutExceptions
            <$> runStaticMethod classNm methodNm sig [mkValue a, mkValue b]
    let rslt = getSymIntegralFromValue val
    outIntLit <- toLsbf_lit <$> getVarLit rslt
    evalFn <- mkConcreteEval (V.map (mkCInt w . fromIntegral) $ V.fromList [x, y])
    let inputs = concatMap (intToBoolSeq . mkCInt w . fromIntegral) [x, y]
    be <- getBitEngine
    liftIO $ do
      aigResult <- beEvalAigV be (SV.fromList inputs) (SV.fromList outIntLit)
      case maigNm of
        Nothing -> return ()
        Just nm -> writeAiger be nm outIntLit
      return ( evalFn rslt
             , -- aiger eval
               hexToIntegral
               $ (\hs -> CE.assert (length hs == numBits w `div` 4) hs)
               $ boolSeqToHex
               $ SV.toList
               $ aigResult
             )

singletonArg :: Functor f => (a -> b) -> f a -> f [b]
singletonArg ctor f = (:[]) . ctor <$> f

intInputs :: [Int32] -> TestInput
intInputs = map constInt

longInputs :: [Int64] -> TestInput
longInputs = map constLong

newLongArray :: WordMonad sym => [MonadTerm sym] -> Simulator sym Ref
newLongArray = Simulation.newLongArray (ArrayType LongType)

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

