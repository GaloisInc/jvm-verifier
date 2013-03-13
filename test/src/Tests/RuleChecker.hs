{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : lerkok
-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.RuleChecker (ruleCheckerTests) where


import qualified Control.Exception as E
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Vector as V

import Verinf.Symbolic

main = defaultMain [ruleCheckerTests]

ruleCheckerTests :: Test
ruleCheckerTests = testGroup "RuleChecker" $
      [testCase ("goodRule" ++ show (i :: Int)) (testGoodRule r) 
           | (i, r) <- zip [0 ..] goodRules]
   ++ [testCase ("badRule"  ++ show (i :: Int)) (testBadRule  r)
           | (i, r) <- zip [0 ..] badRules]
  where testGoodRule (gold, r) = ruleName r @?= gold
        testBadRule  (gold, r)  = do
                let mark = "recovered from expected failure test error, this must not a valid rule name!"
                    rn   = ruleName r
                res <- (r `seq` rn `seq` return rn) `E.catch` (\(_ :: E.ErrorCall) -> return mark)
                assert (res /= gold)
        goodRules = [ goodRule1
                    , goodRule2
                    , goodRule3
                    , goodRule4
                    , goodRule5
                    , goodRule6
                    ]
        badRules  = [ badRule1
                    , badRule2
                    , badRule3
                    , badRule4
                    , badRule5
                    , badRule6
                    ]

type R = Rule

-- good rules
goodRule1 :: (String, R)
goodRule1 = (nm, mkRule nm ((a # b) # c) (a # (b # c)))
  where a = mkVar "a" (SymInt (varWidth "a"))
        b = mkVar "b" (SymInt (varWidth "b"))
        c = mkVar "c" (SymInt (varWidth "c"))
        nm = "#-assoc"

goodRule2 :: (String, R)
goodRule2 = (nm, mkRule nm (mkCBool True &&& x) x)
  where x  = mkVar "x" SymBool
        nm = "and-elim-1"

goodRule3 :: (String, R)
goodRule3 = (nm, mkRule nm (x &&& mkCBool True) x)
  where x  = mkVar "x" SymBool
        nm = "and-elim-2"

goodRule4 :: (String, R)
goodRule4 = (nm, mkRule nm (mkCBool True ||| x) (mkCBool True))
  where x  = mkVar "x" SymBool
        nm = "or-elim-1"

goodRule5 :: (String, R)
goodRule5 = (nm, mkRule nm (x ||| mkCBool True) (mkCBool True))
  where x  = mkVar "x" SymBool
        nm = "or-elim-2"

goodRule6 :: (String, R)
goodRule6 = (nm, mkRule nm (ushr x (mkCInt 9 0)) x)
  where x    = mkVar "x" $ SymInt (constantWidth 384)
        nm   = "shift-by-0:[389]-[9]"

-- bad rules

-- check that left hand side is not a simple variable
badRule1 :: (String, R)
badRule1 = (nm, mkRule nm x (mkCBool True &&& x))
  where x  = mkVar "x" SymBool
        nm = "bad-and-elim-1"

-- check the case when both sides yield different types
badRule2 :: (String, R)
badRule2 = (nm, mkRule nm ((a # b) # c) ((a # b) # (a # c)))
  where a = mkVar "a" (SymInt (varWidth "a"))
        b = mkVar "b" (SymInt (varWidth "b"))
        c = mkVar "c" (SymInt (varWidth "c"))
        nm = "#-bad-assoc-type"

-- check that the right hand side cannot have extra variables
badRule3 :: (String, R)
badRule3 = (nm, mkRule nm (a # b) (a # c))
  where a = mkVar "a" (SymInt (varWidth "a"))
        b = mkVar "b" (SymInt (varWidth "b"))
        c = mkVar "c" (SymInt (varWidth "b"))
        nm = "#-bad-assoc-extra-vars"

-- check that operator arg gounts match (fully saturated, first order)
badRule4 :: (String, R)
badRule4 = (nm, mkRule nm (appTerm bAndOp [a, b, c])
                          (appTerm bAndOp [b, c]))
  where a = mkVar "a" SymBool
        b = mkVar "b" SymBool
        c = mkVar "c" SymBool
        nm = "#-bad-and-wrong-count"

-- check that argument types match
badRule5 :: (String, R)
badRule5 = (nm, mkRule nm (a # b) (a # b))
  where a  = mkVar "a" (SymInt wx)
        wx = varWidth "a"
        b  = mkVar "b" SymBool
        nm = "#-assoc-bad-argument type"

-- check bad operator defn with non-variable type
badRule6 :: (String, R)
badRule6 = (nm, mkRule nm (mkAnd (mkCBool True) a) a)
  where a  = mkVar "x" SymBool
        nm = "and-elim-1"
        mkAnd x y = appTerm bAndOpBotched [x, y]
        bAndOpBotched =
          groundOp (bAndOpDef { opDefArgTypes = V.replicate 2 badType })
        badType = SymInt $ addWidth (varWidth "x") (varWidth "y")
