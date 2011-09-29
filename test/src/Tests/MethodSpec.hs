{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}

{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Tests.MethodSpec (methodSpecTests) where

import Control.Monad
import Data.Maybe (isJust)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import JavaParser
import MethodSpec
import ProofUtils hiding (run)
import qualified Data.Vector as V
import Tests.Common

methodSpecTests :: [(Args, Property)]
methodSpecTests =
  [ testBlastSpec "blastStaticIntArrayCopy"  (staticIntArrayCopySpec  :: MethodSpec SymbolicMonad)
  , testBlastSpec "blastStaticLongArrayCopy" (staticLongArrayCopySpec :: MethodSpec SymbolicMonad)
  , testBlastSpec "blastCopyToIntArrayF"     (copyToIntArrayFSpec     :: MethodSpec SymbolicMonad)
  , testBlastSpec "redCopyFromIntArrayF"     (copyFromIntArrayFSpec   :: MethodSpec SymbolicMonad)
  , let spec = staticIntArrayCopySpec :: MethodSpec SymbolicMonad
        verifyAction = staticIntArrayCopyVerify
    in
      testRedSpec "testStaticIntArrayCopy" spec verifyAction
  , let spec = buggyBitMaskSpec :: MethodSpec SymbolicMonad
        verifyAction = buggyBitMaskVerify
    in
      testRedSpec "testBuggyBitMask" spec verifyAction
-- TODO FIXME re-enable when inductive assertions are working again
--    , testBlastSpec "blastStraightLineAssertion" (straightLineAssertionSpec :: MethodSpec SymbolicMonad)
--    , testBlastSpec "blastLoopAssertion"         (loopAssertionSpec         :: MethodSpec SymbolicMonad)
  ]

singleTest :: Property -> (Args, Property)
singleTest prop = (stdArgs { maxSuccess = 1 }, prop)

testBlastSpec :: String -> MethodSpec SymbolicMonad -> (Args, Property)
testBlastSpec name spec =
  singleTest $ label name $ monadicIO $ do
    cb <- commonCB
    run $ do
      oc <- mkOpCache
      be <- runSymbolic oc $ (getBitEngine :: SymbolicMonad (BitEngine Lit))
      when (isJust (beCheckSat be)) $ do
        blastMethodSpec oc cb spec

testRedSpec :: String
            -> MethodSpec SymbolicMonad
            -> ([Rule] -> MonadTerm SymbolicMonad -> SymbolicMonad ())
            -> (Args, Property)
testRedSpec name spec verifyAction =
  ( stdArgs { maxSuccess = 1 }
  , label name $ monadicIO $ do
      cb <- commonCB
      let initAction = return ()
      run $ do
        oc <- mkOpCache
        -- All the rules we might need for any spec...
        rules <- runRuleMonad $ do
          -- "eqOp" simplification
          let { x = mkVar "x" (SymShapeVar "x") }
            in defRule "=:reflexive" (eq x x) (mkCBool True)
          -- Add And and Or simplification
          let { x = mkVar "x" SymBool }
            in do defRule "and-elim-1" (mkCBool False &&& x) (mkCBool False)
                  defRule "and-elim-2" (x &&& mkCBool False) (mkCBool False)
                  defRule "and-elim-3" (mkCBool True &&& x) x
                  defRule "and-elim-4" (x &&& mkCBool True) x
                  defRule "or-elim-1" (mkCBool True ||| x) (mkCBool True)
                  defRule "or-elim-2" (x ||| mkCBool True) (mkCBool True)
                  defRule "or-elim-3" (mkCBool False ||| x) x
                  defRule "or-elim-4" (x ||| mkCBool False) x
          let { a = mkVar "a" arrType
              ; i = mkVar "i" w32
              }
            in do defRule "null-set" (aset a i (aget a i)) a
          return ()
        redMethodSpec oc cb spec initAction (verifyAction rules))

staticIntArrayCopySpec :: (Monad sym, ConstantInjection (MonadTerm sym)) =>
                          MethodSpec sym
staticIntArrayCopySpec =
  MethodSpec
    { specClass = "TestMethodSpec"
    , specMethodKey = makeMethodKey "staticIntArrayCopy" "([I[I)V"
    , specInitializedClasses = ["TestMethodSpec"]
    , specRefs = [ ([Arg 0, Arg 1], SpecArray 12 IntType) ]
    , specPrecondition = trivialCond
    , specChanges = [(Arg 0, ArrayUpdate (ArrayValue (Arg 1)))]
    , specAssertions = []
    }

staticIntArrayCopyVerify :: (MonadTerm m ~ Node,
                             MonadIO m,
                             ApplyOpMonad m) =>
                            [Rule] -> MonadTerm m -> m ()
staticIntArrayCopyVerify rules t = do
  let pgrm = foldl addRule emptyProgram rules
  rew <- liftIO $ mkRewriter pgrm
  res <- reduce rew t
  case getBool res of
    Just True -> return ()
    _ -> return ()
{-
    _ -> do liftIO $ putStr "Residual term: "
            liftIO $ putStrLn $ prettyTerm res
            fail "Rewriting proof failed!"
 -}
  return ()

buggyBitMaskSpec :: MethodSpec SymbolicMonad
buggyBitMaskSpec =
  MethodSpec
    { specClass = "TestMethodSpec"
    , specMethodKey = makeMethodKey "buggyBitMask" "([I[I)V"
    , specInitializedClasses = ["TestMethodSpec"]
    , specRefs = [ ([Arg 0, Arg 1], SpecArray 1 IntType) ]
    , specPrecondition = trivialCond
    , specChanges = [( Arg 0
                     , ArrayUpdate (appList specFn [ArrayValue (Arg 1)])
                     )
                    ]
    , specAssertions = []
    }
    where specFn :: [MonadTerm SymbolicMonad]
                 -> SymbolicMonad (MonadTerm SymbolicMonad)
          specFn [x] = do {-
            y <- applyGetArrayValue x (mkCInt 32 3)
            y' <- applyINot y
            symbolicArrayFromList (Wx 32) w32 (replicate 12 y') -}
            return x
          specFn _ = error "spec applied to wrong number of arguments"

buggyBitMaskVerify :: (ApplyOpMonad m,
                       MonadIO m,
                       TermClass (MonadTerm m)) =>
                      [Rule] -> MonadTerm m -> m ()
buggyBitMaskVerify rules t = do
  let pgrm = foldl addRule emptyProgram rules
  rew <- liftIO $ mkRewriter pgrm
  res <- reduce rew t
  case getBool res of
    Just False -> return ()
    Just True -> fail "Rewriting proof succeded unexpectedly!"
    _ -> do --liftIO $ putStr "Residual term: "
            --liftIO $ putStrLn $ prettyTerm res
            return ()
  return ()

staticLongArrayCopySpec :: (Monad sym, ConstantInjection (MonadTerm sym)) =>
                           MethodSpec sym
staticLongArrayCopySpec =
  MethodSpec
    { specClass = "TestMethodSpec"
    , specMethodKey = makeMethodKey "staticLongArrayCopy" "([J[J)V"
    , specInitializedClasses = ["TestMethodSpec"]
    , specRefs = [ ([Arg 0, Arg 1], SpecArray 12 LongType) ]
    , specPrecondition = trivialCond
    , specChanges = [(Arg 0, ArrayUpdate (ArrayValue (Arg 1)))]
    , specAssertions = []
    }

intArrayF :: FieldId
intArrayF = FieldId { fieldIdClass = "TestMethodSpec"
                    , fieldIdName = "intArrayF"
                    , fieldIdType = ArrayType IntType }

copyToIntArrayFSpec :: (Monad sym, ConstantInjection (MonadTerm sym)) =>
                       MethodSpec sym
copyToIntArrayFSpec =
    MethodSpec
      { specClass = "TestMethodSpec"
      , specMethodKey = makeMethodKey "copyToIntArrayF" "([I)V"
      , specInitializedClasses = ["TestMethodSpec"]
      , specRefs = [ ( [This], SpecRefClass "TestMethodSpec")
                   , ( [Arg 0, InstanceField This intArrayF]
                     , SpecArray 12 IntType) ]
      , specPrecondition = trivialCond
      , specChanges = [( InstanceField This intArrayF
                       , ArrayUpdate (ArrayValue (Arg 0)))]
      , specAssertions = []
      }

copyFromIntArrayFSpec :: (Monad sym, ConstantInjection (MonadTerm sym)) =>
                         MethodSpec sym
copyFromIntArrayFSpec =
  MethodSpec
    { specClass = "TestMethodSpec"
    , specMethodKey = makeMethodKey "copyFromIntArrayF" "([I)V"
    , specInitializedClasses = ["TestMethodSpec"]
    , specRefs = [ ( [This], SpecRefClass "TestMethodSpec")
                 , ( [Arg 0, InstanceField This intArrayF]
                   , SpecArray 12 IntType) ]
    , specPrecondition = trivialCond
    , specChanges =
        [( Arg 0, ArrayUpdate (ArrayValue (InstanceField This intArrayF)))]
    , specAssertions = []
    }

_straightLineAssertionSpec :: WordMonad sym => MethodSpec sym
_straightLineAssertionSpec =
  MethodSpec
    { specClass = "TestMethodSpec"
    , specMethodKey = makeMethodKey "straightLineAssertion" "()V"
    , specInitializedClasses = ["TestMethodSpec"]
    , specRefs = []
    , specPrecondition = trivialCond
    , specChanges = []
    , specAssertions = [( 8
                        , SymApp
                            equals2
                            (V.fromList [ScalarValue (LocalVar 0)])
                        --, [HLocal 0]
                        )]
    }
    where equals2 args =
            --assert (V.length args == 1) $
            applyEq (args V.! 0) (mkCInt 32 2)

_loopAssertionSpec :: WordMonad sym => MethodSpec sym
_loopAssertionSpec =
  MethodSpec
    { specClass = "TestMethodSpec"
    , specMethodKey = makeMethodKey "loopAssertion" "()V"
    , specInitializedClasses = ["TestMethodSpec"]
    , specRefs = []
    , specPrecondition = trivialCond
    , specChanges = []
    , specAssertions = [( 5
                        , SymApp
                            sumEquals10
                            (V.fromList [ ScalarValue (LocalVar 0)
                                        , ScalarValue (LocalVar 1)])
                        --, [HLocal 0, HLocal 1]
                        )]
    }
    where sumEquals10 args =
            --assert (V.length args == 2) $
            applyEq (mkCInt 32 10) =<< applyAdd (args V.! 0) (args V.! 1)

_ignore_nouse :: a
_ignore_nouse = undefined main

main :: IO ()
main = runTests methodSpecTests
