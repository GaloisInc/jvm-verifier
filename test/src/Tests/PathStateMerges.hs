{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

module Tests.PathStateMerges(psmsTests) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe
import Prelude hiding (catch)
import Test.QuickCheck

import JavaParser
import Simulation hiding (run)
import Tests.Common
import Utils
import Utils.Simulation

import Verinf.Utils.CatchMIO
import Verinf.Symbolic

-- import Debug.Trace

dummyCN :: String
dummyCN = "PathStateMerges$Dummy"

psmsTests :: [(Args, Property)]
psmsTests =
  [ (`test1` "ddb1") $ \cb -> runSymTest $ \sbe -> do
      b    <- IValue <$> freshInt sbe
      outs <- runDefSimulator sbe cb $ do
        let zero = mkCInt 32 0
        inpArr <- newIntArray intArrayTy $ [zero, zero]
        rs <- withoutExceptions
                <$> runStaticMethod "PathStateMerges" "ddb1" "(Z[I)V"
                      [b, RValue inpArr]
        when (length rs /= 1) $ error "psmsTests.ddb1: failed path state merge"
        getIntArray (fst . head $ rs) inpArr
      let fn x = do
            map (intFromConst . fromJust . termConst)
              <$> evalAigArray sbe 32 [constInt x] outs
      (\x -> [[[99,2000],[42,1000]] == x]) <$> mapM fn [0,1]
  , (`test1` "ddb2") $ \cb -> runSymTest $ \sbe -> do
      b <- IValue <$> freshInt sbe
      out <- runDefSimulator sbe cb $ do
        rs <- withoutExceptions
                <$> runStaticMethod "PathStateMerges" "ddb2" "(Z)I" [b]
        when (length rs /= 1) $ error "psmsTests.ddb2: failed path state merge"
        let [(_, ReturnVal (IValue r))] = rs
        return r
      (:[]) . (==) [99,42] . map (intFromConst . fromJust . termConst)
         <$> mapM (\x -> evalAigIntegral sbe id [constInt x] out) [0,1]
  , (`test1` "ddb3") $ \cb -> runSymTest $ \sbe -> do
      b     <- IValue <$> freshInt sbe
      _refs <- runDefSimulator sbe cb $ do
        arr <- newMultiArray (ArrayType (ClassType dummyCN)) [mkCInt 32 1]
        rs  <- withoutExceptions
                <$> runStaticMethod "PathStateMerges" "ddb3" ("(Z[L" ++ dummyCN ++ ";)V")
                      [b, RValue arr]
        when (length rs /= 1) $ error "psmsTests.ddb3: failed path state merge"
        getRefArray (fst . head $ rs) arr
      return [True]
  , (`test1` "ddb4") $ \cb -> runSymTest $ \sbe -> do
      b   <- IValue <$> freshInt sbe
      out <- runDefSimulator sbe cb $ do
        d   <- createInstance dummyCN $ Just [(IntType, IValue $ mkCInt 32 0)]
        rs  <- withoutExceptions
                 <$> runStaticMethod "PathStateMerges" "ddb4"
                       ("(ZL" ++ dummyCN ++ ";)V") [b, RValue d]
        when (length rs /= 1) $ error "psmsTests.ddb4: failed path state merge"
        let [(pd,Terminated)] = rs
        unIValue <$> getInstanceFieldValue pd d (FieldId dummyCN "m_x" IntType)
      (:[]) . (==) [99,42] . map (intFromConst . fromJust . termConst)
        <$> mapM (\x -> evalAigIntegral sbe id [constInt x] out) [0,1]
  , (`test1` "ddb5") $ \cb -> runTest $ do
      sbe <- getBackend
      liftIO $ do
        b   <- IValue <$> freshInt sbe
        out <- runDefSimulator sbe cb $ do
          d   <- createInstance dummyCN $ Just [(IntType, IValue $ mkCInt 32 0)]
          rs  <- withoutExceptions
                 <$> runStaticMethod "PathStateMerges" "ddb5"
                       ("(ZL" ++ dummyCN ++ ";)V") [b, RValue d]
          when (length rs /= 1) $ error "psmsTests.ddb5: failed path state merge"
          let [(pd,Terminated)] = rs
          unLValue <$> getStaticFieldValue pd (FieldId dummyCN "m_st" LongType)
        (:[]) . (==) [7,42] . map (longFromConst . fromJust . termConst)
          <$> mapM (\x -> evalAigIntegral sbe id [constInt x] out) [0,1]
  , (`test1` "ddb6") $ \cb -> runTest $ do
      sbe <- getBackend 
      b <- liftIO $ IValue <$> freshInt sbe
      liftIO $ do
        out <- runDefSimulator sbe cb $ do
          rs <- withoutExceptions
                  <$> runStaticMethod "PathStateMerges" "ddb6" "(Z)I" [b]
          when (length rs /= 1) $ error "psmsTests.ddb6: failed path state merge"
          let [(_, ReturnVal (IValue r))] = rs
          return r
        (:[]) . (==) [99,42] . map (intFromConst . fromJust . termConst)
          <$> mapM (\x -> evalAigIntegral sbe id [constInt x] out) [0,1]
  , (`test1` "ddb7") $ \cb -> runSymTest $ \sbe -> do
      [b1, b2] <- replicateM 2 $ IValue <$> freshInt sbe
      out <- runDefSimulator sbe cb $ do
        rs <- withoutExceptions
                <$> runStaticMethod "PathStateMerges" "ddb7" "(ZZ)I" [b1,b2]
        when (length rs /= 1) $ error "psmsTests.ddb7: failed path state merge"
        let [(_, ReturnVal (IValue r))] = rs
        return r
      (:[]) . (==) (map (2*) [2000,99,1000,42]) . map (intFromConst . fromJust . termConst)
        <$> mapM (\(b1,b2) -> evalAigIntegral sbe id (map constInt [b1,b2]) out)
                 [(0,0), (0,1), (1,0), (1,1)]
  , (`test1` "mul3") $ \cb -> runSymTest $ \sbe -> do
      [a, b] <- replicateM 2 $ IValue <$> freshInt sbe
      out <- runDefSimulator sbe cb $ do
        rs <- withoutExceptions
                 <$> runStaticMethod "PathStateMerges" "mul3" "(III)I"
                       [a, b, IValue (mkCInt 32 33)]
        when (length rs /= 1) $ error "psmsTests.mul3: failed path state merge"
        let [(_, ReturnVal (IValue r))] = rs
        return r
      (:[]) . (==) [4, 20, 4158] . map (intFromConst . fromJust . termConst)
        <$> mapM (\(x,y) -> evalAigIntegral sbe id [constInt x, constInt y] out)
                 [(2,2), (4,5), (42,99)]
  , (`test1` "mul2") $ \cb -> runSymTest $ \sbe -> do
      [a, b] <- replicateM 2 $ IValue <$> freshInt sbe
      out <- runSimulator sbe SimulationFlags{ alwaysBitBlastBranchTerms = True} cb $ do
        rs <- withoutExceptions
                 <$> runStaticMethod "PathStateMerges" "mul2" "(II)I" [a, b]
        when (length rs /= 1) $ error "psmsTests.mul2: failed path state merge"
        let [(_, ReturnVal (IValue r))] = rs
        return r
      (:[]) . (==) [4, 20, 4158, 77137830] . map (intFromConst . fromJust . termConst)
        <$> mapM (\(x,y) -> evalAigIntegral sbe id [constInt x, constInt y] out)
                 [(2,2), (4,5), (42,99), (2310, 33393)]
  ]

--------------------------------------------------------------------------------
-- Scratch

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests psmsTests
