{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

module Tests.PathStateMerges(psmsTests) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Test.QuickCheck

import JavaParser
import Simulation hiding (run)
import Symbolic
import Tests.Common
import Utils
import Utils.CatchMIO
import Utils.Simulation

-- import Debug.Trace

dummyCN :: String
dummyCN = "PathStateMerges$Dummy"

psmsTests :: [(Args, Property)]
psmsTests =
  [
    (`test1` "ddb1") $ \cb -> runTest $ do
      outs <- runSimulator cb $ do
        b      <- IValue <$> liftSymbolic freshInt
        let zero = mkCInt (Wx 32) 0
        inpArr <- newIntArray intArrayTy $ [zero, zero]
        rs <- withoutExceptions
              <$> runStaticMethod "PathStateMerges" "ddb1" "(Z[I)V"
                    [b, RValue inpArr]
--         dbugM $ "rs = " ++ show rs
        when (length rs /= 1) $ error "psmsTests.ddb1: failed path state merge"
        getIntArray (fst . head $ rs) inpArr
      (:[]) . (==) [[99,2000],[42,1000]] . map (map (intFromConst . fromJust . termConst))
        <$> mapM (\x -> evalAigArray (Wx 32) [constInt x] outs) [0,1]
    `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.ddb1")
  ,
     (`test1` "ddb2") $ \cb -> runTest $ do
      out <- runSimulator cb $ do
        b <- IValue <$> liftSymbolic freshInt
        rs <- withoutExceptions
              <$> runStaticMethod "PathStateMerges" "ddb2" "(Z)I" [b]
        when (length rs /= 1) $ error "psmsTests.ddb2: failed path state merge"
        let [(_, ReturnVal (IValue r))] = rs
        return r
      (:[]) . (==) [99,42] . map (intFromConst . fromJust . termConst)
        <$> mapM (\x -> evalAigIntegral id [constInt x] out) [0,1]
    `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.ddb2")
  ,
    (`test1` "ddb3") $ \cb -> runTest $ do
      _refs <- runSimulator cb $ do
        b   <- IValue <$> liftSymbolic freshInt
        arr <- newMultiArray (ArrayType (ClassType dummyCN)) [mkCInt (Wx 32) 1]
        rs  <- withoutExceptions
               <$> runStaticMethod "PathStateMerges" "ddb3" ("(Z[L" ++ dummyCN ++ ";)V")
                     [b, RValue arr]
        when (length rs /= 1) $ error "psmsTests.ddb3: failed path state merge"
        getRefArray (fst . head $ rs) arr
      return [True]
    `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.ddb3")
  ,
    (`test1` "ddb4") $ \cb -> runTest $ do
      out <- runSimulator cb $ do
        b   <- IValue <$> liftSymbolic freshInt
        d   <- createInstance dummyCN $ Just [(IntType, IValue $ mkCInt (Wx 32) 0)]
        rs  <- withoutExceptions
               <$> runStaticMethod "PathStateMerges" "ddb4"
                     ("(ZL" ++ dummyCN ++ ";)V") [b, RValue d]
        when (length rs /= 1) $ error "psmsTests.ddb4: failed path state merge"
        let [(pd,Terminated)] = rs
        unIValue <$> getInstanceFieldValue pd d (FieldId dummyCN "m_x" IntType)
      (:[]) . (==) [99,42] . map (intFromConst . fromJust . termConst)
        <$> mapM (\x -> evalAigIntegral id [constInt x] out) [0,1]
    `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.ddb4")
  ,
    (`test1` "ddb5") $ \cb -> runTest $ do
      out <- runSimulator cb $ do
        b   <- IValue <$> liftSymbolic freshInt
        d   <- createInstance dummyCN $ Just [(IntType, IValue $ mkCInt (Wx 32) 0)]
        rs  <- withoutExceptions
               <$> runStaticMethod "PathStateMerges" "ddb5"
                     ("(ZL" ++ dummyCN ++ ";)V") [b, RValue d]
        when (length rs /= 1) $ error "psmsTests.ddb5: failed path state merge"
        let [(pd,Terminated)] = rs
        unLValue <$> getStaticFieldValue pd (FieldId dummyCN "m_st" LongType)
      (:[]) . (==) [7,42] . map (longFromConst . fromJust . termConst)
        <$> mapM (\x -> evalAigIntegral id [constInt x] out) [0,1]
    `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.ddb5")
  ,
     (`test1` "ddb6") $ \cb -> runTest $ do
      out <- runSimulator cb $ do
        b <- IValue <$> liftSymbolic freshInt
        rs <- withoutExceptions
              <$> runStaticMethod "PathStateMerges" "ddb6" "(Z)I" [b]
        when (length rs /= 1) $ error "psmsTests.ddb6: failed path state merge"
        let [(_, ReturnVal (IValue r))] = rs
        return r
      (:[]) . (==) [99,42] . map (intFromConst . fromJust . termConst)
        <$> mapM (\x -> evalAigIntegral id [constInt x] out) [0,1]
    `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.ddb6")
  ,
     (`test1` "ddb7") $ \cb -> runTest $ do
      out <- runSimulator cb $ do
        [b1, b2] <- map IValue <$> replicateM 2 (liftSymbolic freshInt)
        rs <- withoutExceptions
              <$> runStaticMethod "PathStateMerges" "ddb7" "(ZZ)I" [b1,b2]
        when (length rs /= 1) $ error "psmsTests.ddb7: failed path state merge"
        let [(_, ReturnVal (IValue r))] = rs
        return r

      (:[]) . (==) (map (2*) [2000,99,1000,42]) . map (intFromConst . fromJust . termConst)
        <$> mapM (\(b1,b2) -> evalAigIntegral id (map constInt [b1,b2]) out)
              [(0,0), (0,1), (1,0), (1,1)]
    `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.ddb7")
  ,
     (`test1` "mul3") $ \cb -> runTest $ do
      out <- runSimulator cb $ do
        [a, b] <- map IValue <$> replicateM 2 (liftSymbolic freshInt)
        rs <- withoutExceptions
              <$> runStaticMethod "PathStateMerges" "mul3" "(III)I"
                    [a, b, IValue (mkCInt (Wx 32) 33)]
        when (length rs /= 1) $ error "psmsTests.mul3: failed path state merge"
        let [(_, ReturnVal (IValue r))] = rs
        return r
      (:[]) . (==) [4, 20, 4158] . map (intFromConst . fromJust . termConst)
        <$> mapM (\(x,y) -> evalAigIntegral id [constInt x, constInt y] out)
              [(2,2), (4,5), (42,99)]
    `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.mul3")
  ,
     (`test1` "mul2") $ \cb -> runTest $ do
      out <- runSimulator' SimulationFlags{ alwaysBitBlastBranchTerms = True} cb $ do
        [a, b] <- map IValue <$> replicateM 2 (liftSymbolic freshInt)
        rs <- withoutExceptions
              <$> runStaticMethod "PathStateMerges" "mul2" "(II)I" [a, b]
        when (length rs /= 1) $ error "psmsTests.mul2: failed path state merge"
        let [(_, ReturnVal (IValue r))] = rs
        return r
      (:[]) . (==) [4, 20, 4158, 77137830] . map (intFromConst . fromJust . termConst)
        <$> mapM (\(x,y) -> evalAigIntegral id [constInt x, constInt y] out)
              [(2,2), (4,5), (42,99), (2310, 33393)]
    `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.mul2")
--   ,
-- -- As of <2010-01-19 Wed>, ddb8 does not symbolically terminate, and it is not expected to.
--      (`test1` "ddb8") $ \cb -> runTest $ do
--       out <- runSimulator cb $ do
--         x <- IValue <$> liftSymbolic freshInt
--         rs <- withoutExceptions <$> runStaticMethod "PathStateMerges" "ddb8" "(I)I" [x]
--         when (length rs /= 1) $ error "psmsTests.ddb8: failed path state merge"
--         let [(_, ReturnVal (IValue r))] = rs
--         return r
--       liftIO $ putStrLn $ "out = " ++ show out
--       return [True]
--     `catchMIO` simExcHndlr ("Unexpected error caught: psmsTests.ddb8")
  ]

--------------------------------------------------------------------------------
-- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests psmsTests
