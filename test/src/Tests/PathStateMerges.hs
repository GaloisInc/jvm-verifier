{- |
Module           : $Header$
Description      :
License          : BSD3
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}
{-# LANGUAGE CPP #-}

module Tests.PathStateMerges (psmsTests, mul2WithFlags) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import Data.Maybe
import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Tests.Common


dummyCN :: String
dummyCN = "PathStateMerges$Dummy"

psmsTests :: Codebase -> TestTree
psmsTests cb = testGroup "PathMerges" $
  [ testCase "ddb1" . mkSymAssertion $ \sbe -> do
      b    <- IValue <$> freshInt sbe
      outs <- runDefSimulator cb sbe $ do
        let zero = mkCInt 32 0
        inpArr <- newIntArray intArrayTy $ [zero, zero]
        rs <- runStaticMethod "PathStateMerges" "ddb1" "(Z[I)V"
                  [b, RValue inpArr]
        when (length rs /= 1) $ error "psmsTests.ddb1: failed path state merge"
        getIntArray inpArr
      let fn x = do
            map (fromJust . asInt sbe)
              <$> evalAigArray sbe 32 [mkCInt 32 x] outs
      ([[99,2000],[42,1000]] @=?) =<< mapM fn [0,1]
  , testCase "ddb2" . mkSymAssertion $ \sbe -> do
      b <- IValue <$> freshInt sbe
      out <- runDefSimulator cb sbe $ do
        rs <- runStaticMethod "PathStateMerges" "ddb2" "(Z)I" [b]
        when (length rs /= 1) $ error "psmsTests.ddb2: failed path state merge"
        let [(_, Just (IValue r))] = rs
        return r
      res <- forM [0,1] $ \x ->
        asInt sbe <$> evalAigIntegral sbe id [mkCInt 32 x] out
      map Just [99,42] @=? res
  , testCase "ddb3" . mkSymAssertion $ \sbe -> do
      b     <- IValue <$> freshInt sbe
      void $ runDefSimulator cb sbe $ do
        arr <- newMultiArray (ArrayType (ClassType dummyCN)) [mkCInt 32 1]
        rs  <- runStaticMethod "PathStateMerges" "ddb3" ("(Z[L" ++ dummyCN ++ ";)V")
                               [b, RValue arr]
        when (length rs /= 1) $ error "psmsTests.ddb3: failed path state merge"
        getRefArray arr
  , testCase "ddb4" . mkSymAssertion $ \sbe -> do
      b   <- IValue <$> freshInt sbe
      IValue out <- runDefSimulator cb sbe $ do
        d   <- createInstance dummyCN $ Just [(IntType, IValue $ mkCInt 32 0)]
        rs  <- runStaticMethod "PathStateMerges" "ddb4"
                   ("(ZL" ++ dummyCN ++ ";)V") [b, RValue d]
        when (length rs /= 1) $ error "psmsTests.ddb4: failed path state merge"
        getInstanceFieldValue d (FieldId dummyCN "m_x" IntType)
      (@=?) [99,42] . map (fromJust . asInt sbe)
        =<< mapM (\x -> evalAigIntegral sbe id [mkCInt 32 x] out) [0,1]
  , testCase "ddb5" . mkSymAssertion $ \sbe -> do
      b   <- IValue <$> freshInt sbe
      LValue out <- runDefSimulator cb sbe $ do
        d   <- createInstance dummyCN $ Just [(IntType, IValue $ mkCInt 32 0)]
        rs  <- runStaticMethod "PathStateMerges" "ddb5"
                  ("(ZL" ++ dummyCN ++ ";)V") [b, RValue d]
        when (length rs /= 1) $ error "psmsTests.ddb5: failed path state merge"
        getStaticFieldValue (FieldId dummyCN "m_st" LongType)
      (@=?) [7,42] . map (fromJust . asLong sbe)
        =<< mapM (\x -> evalAigIntegral sbe id [mkCInt 32 x] out) [0,1]
  , testCase "ddb6" . mkSymAssertion $ \sbe -> do
      b <- IValue <$> freshInt sbe
      rs <- runDefSimulator cb sbe $
              runStaticMethod "PathStateMerges" "ddb6" "(Z)I" [b]
      when (length rs /= 1) $ error "psmsTests.ddb6: failed path state merge"
      let [(_, Just (IValue out))] = rs
      forM_ [(0,99),(1,42)] $ \(x,e) -> do
        r <- evalAigIntegral sbe id [mkCInt 32 x] out
        Just e @=? asInt sbe r
  , testCase "ddb7" . mkSymAssertion $ \sbe -> do
      [b1, b2] <- replicateM 2 $ IValue <$> freshInt sbe
      out <- runDefSimulator cb sbe $ do
        rs <- runStaticMethod "PathStateMerges" "ddb7" "(ZZ)I" [b1,b2]
        when (length rs /= 1) $ error "psmsTests.ddb7: failed path state merge"
        let [(_, Just (IValue r))] = rs
        return r
      (@=?) (map (2*) [2000,99,1000,42]) . map (fromJust . asInt sbe)
        =<< mapM (\(b1',b2') -> evalAigIntegral sbe id (map (mkCInt 32) [b1',b2']) out)
                 [(0,0), (0,1), (1,0), (1,1)]
  , testCase "mul3" . mkSymAssertion $ \sbe -> do
      [a, b] <- replicateM 2 $ IValue <$> freshInt sbe
      rs <- runDefSimulator cb sbe $
              runStaticMethod "PathStateMerges" "mul3" "(III)I"
                              [a, b, IValue (mkCInt 32 33)]
      when (length rs /= 1) $ error "psmsTests.mul3: failed path state merge"
      let [(_, Just (IValue out))] = rs
      (@=?) [4, 20, 4158] . map (fromJust . asInt sbe)
        =<< mapM (\(x,y) -> evalAigIntegral sbe id [mkCInt 32 x, mkCInt 32 y] out)
                 [(2,2), (4,5), (42,99)]
  , testCase "mul2-blast" . mul2WithFlags cb $ bitblastFlags
  , testCase "mul2-sat" . mul2WithFlags cb $ satFlags
  ]

mul2WithFlags :: Codebase -> SimulationFlags -> Assertion
mul2WithFlags cb flags = mkSymAssertion $ \sbe -> do
  [a, b] <- replicateM 2 $ IValue <$> freshInt sbe
  rs <- runSimulator cb sbe defaultSEH (Just flags) $
          runStaticMethod "PathStateMerges" "mul2" "(II)I" [a, b]
  assertEqual "failed path state merge" 1 (length rs)
  let [(_, Just (IValue out))] = rs
  (@=?) [4, 20, 4158, 77137830] . map (fromJust . asInt sbe)
    =<< mapM (\(x,y) -> evalAigIntegral sbe id [mkCInt 32 x, mkCInt 32 y] out)
             [(2,2), (4,5), (42,99), (2310, 33393)]

--------------------------------------------------------------------------------
-- Scratch

-- _ignore_nouse :: a
-- _ignore_nouse = undefined main

--main :: IO ()
--main = do cb <- commonLoadCB
--          defaultMain [psmsTests cb]
