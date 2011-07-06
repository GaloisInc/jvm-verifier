{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : matthews
-}

{-# LANGUAGE CPP #-}
module Tests.Session (sessionTests)
  where

import qualified Control.Exception as CE

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Verinf.Symbolic
import Verinf.IO.Session
import Verinf.Utils.Assert

sessionTests :: [(Args, Property)]
sessionTests =
  [ ( stdArgs{ maxSuccess = 1 }
    , label "testSymbolicSession" $ monadicIO $ run testSymbolicSession
    )
  ]

-- Testing Symbolic.makeSession and some SymbolicMonad operations within
--  the session.
testSymbolicSession :: IO ()
testSymbolicSession = do
  -- Create a new session for performing SymbolicMonad and AigMonad computations
  sess <- makeSession
  let sym = toIO sess
  let aig = sym . liftAigMonad
  -- Test that SymbolicMonad errors are properly converted to IO exceptions.
  v <- sym $ freshUninterpretedVar (SymInt (constantWidth 32))
  res <- sym ((getVarLit v :: SymbolicMonad LitVector) >> return "this string won't be returned")
           `CE.catch` (return . symExtErrMsg)
  assertIO (res == "Cannot bitblast uninterpreted input variable")
           "Tests.Session.testSymbolicSession: unexpected res value"
  -- Construct an unsatisfiable Symbolic term
  l1 <- aig makeInputLit
  l2 <- aig makeInputLit
  x  <- sym $ freshVar SymBool $ litToLitVector l1      -- x
  y  <- sym $ freshVar SymBool $ litToLitVector l2      -- y
  t1 <- sym $ applyOp (groundOp bNotOpDef) [x]     -- !x
  t2 <- sym $ applyOp (groundOp bOrOpDef)  [t1,y]  -- !x \/ y
  t3 <- sym $ applyOp (groundOp bNotOpDef) [y]     -- !y
  t4 <- sym $ applyOp (groundOp bAndOpDef) [x,t3]  -- x /\ !y
  t6 <- sym $ applyOp (groundOp bXorOpDef) [t2,t4] -- xor (!x \/ y) (x /\ !y)
  t7 <- sym $ applyOp (groundOp bNotOpDef) [t6]    -- !(xor (!x \/ y) (x /\ !y))
  -- Obtain the Lit corresponding to the unsatisfiable formula
  litvec_t7 <- sym $ getVarLit t7
  let bits_t7 = toLsbf_lit litvec_t7
  assertIO (length bits_t7 == 1) "length bits_t7 == 1"
  let [res_lit] = bits_t7
#if defined(UseABC)
  -- check that the Lit is unsatisfiable using ABC
  res_sat <- aig $ checkSat res_lit
  assertIO (res_sat == UnSat) "res_sat == Unsat"
#else
  -- ABC not available; test for all four variable valuations
  resTrueTrue <- aig $ evalAig [True,True] [res_lit]
  assertIO (length resTrueTrue == 1)
           "Tests.Session.testSymbolicSession: (length resTrueTrue == 1) failed"
  assertIO (not (head resTrueTrue))
           "Tests.Session.testSymbolicSession: (not (head resTrueTrue)) failed"
  resTrueFalse <- aig $ evalAig [True,False] [res_lit]
  assertIO (length resTrueFalse == 1)
           "Tests.Session.testSymbolicSession: (length resTrueFalse == 1) failed"
  assertIO (not (head resTrueFalse)) "(not (head resTrueFalse))"
  resFalseTrue <- aig $ evalAig [False,True] [res_lit]
  assertIO (length resFalseTrue == 1)
           "Tests.Session.testSymbolicSession: (length resFalseTrue == 1) failed"
  assertIO (not (head resFalseTrue))
           "Tests.Session.testSymbolicSession: (not (head resFalseTrue)) failed"
  resFalseFalse <- aig $ evalAig [False,False] [res_lit]
  assertIO (length resFalseFalse == 1)
           "Tests.Session.testSymbolicSession: (length resFalseFalse == 1) failed"
  assertIO (not (head resFalseFalse))
           "Tests.Session.testSymbolicSession: (not (head resFalseFalse)) failed"
#endif
  -- Close the session
  finishIO sess

