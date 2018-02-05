{- |
Module           : $Header$
Description      :
License          : BSD3
Stability        : provisional
Point-of-contact : acfoltzer

Since we don't yet support @invokedynamic@, this module only tests to
make sure that the presence of lambda in a class does not preclude us
from analyzing other code in the class.
-}

module Tests.Lambda (lambdaTests) where

import Control.Monad
import Data.Int
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Tests.Common

lambdaTests :: Codebase -> TestTree
lambdaTests cb = testGroup "Lambda" $
  [ testCase "add_vanilla" $
      mkBinOpTest32 cb ("LambdaCalc", "add_vanilla", "(II)I") (+)
        [(0,0), (0,1), (1,0), (1,1), (8192,8192)]
{- this one turns out to use invokedynamic as well, so it's not very illuminating yet
  , testCase "f_methodreference" $
      mkBinOpTest32 cb ("MethodReferenceTest", "f", "(II)I") (+)
        [(0,0), (0,1), (1,0), (1,1), (8192,8192)]
-}
  ]

type MethodSpec  = (String, String, String)

mkBinOpTest32 :: Codebase
              -> MethodSpec
              -> (Int32 -> Int32 -> Int32)
              -> [(Int32, Int32)]
              -> Assertion
mkBinOpTest32 cb ms gnd inps = do
  forM_ inps $ \(x,y) -> do
    dagEval <- evalBinOp32 cb ms x y
    dagEval @?= mkCInt 32 (fromIntegral (x `gnd` y))

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
      runStaticMethod (mkClassName classNm) methodNm sig [a, b]
    let args = V.map (mkCInt 32 . toInteger) (V.fromList [x, y])
    evalFn <- concreteEvalFn args
    evalFn rslt
