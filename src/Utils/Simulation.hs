{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

module Utils.Simulation where

import Control.Arrow (second)

import Verinf.Symbolic

import Execution
import Simulation

withValidResult :: Show term => Either (FinalResult term) a -> (a -> m b) -> m b
withValidResult (Left (Exc ref)) _ = error $ "wvr: obtained exception: ref = " ++ show ref
withValidResult (Left _) _         = error $ "internal: unexpected FinalResult"
withValidResult (Right rslt) f     = f rslt

type TakeSingleton term a =
  [(PathDescriptor, FinalResult term)]
  -> (PathDescriptor, Either (FinalResult term) a)

takeSingleRslt :: Show term => TakeSingleton term (Value term)
takeSingleRslt [(pd,fr)] = case fr of ReturnVal v -> (pd, Right v) ; x -> (pd, Left x)
takeSingleRslt frs       = error $ "takeSingleRslt expected singleton, got: " ++ show frs

withoutExceptions :: [(PathDescriptor, FinalResult term)]
                  -> [(PathDescriptor, FinalResult term)]
withoutExceptions = filter (\r -> case snd r of Exc{} -> False ; _ -> True)

takeIntRslt :: Show term => TakeSingleton term term
takeIntRslt  = takeRsltCommon $ \v ->
  case v of IValue x -> x ; _ -> error "Input not IValue-constructed"

takeLongRslt :: Show term => TakeSingleton term term
takeLongRslt = takeRsltCommon $ \v ->
  case v of LValue x -> x ; _ -> error "Input not LValue-constructed"

takeDoubleRslt :: TakeSingleton SymbolicTerm Double
takeDoubleRslt = takeRsltCommon $ \v ->
  case v of DValue x -> x ; _ -> error "Input not DValue-constructed"

takeFloatRslt :: TakeSingleton SymbolicTerm Float
takeFloatRslt = takeRsltCommon $ \v ->
  case v of FValue x -> x ; _ -> error "Input not DValue-constructed"

takeRefRslt :: Show term => TakeSingleton term Ref
takeRefRslt = takeRsltCommon $ \v ->
  case v of RValue x -> x; _ -> error "Input not RValue-constructed"

takeRsltCommon :: Show term => (Value term -> a) -> TakeSingleton term a
takeRsltCommon f = second (either Left (Right . f)) . takeSingleRslt

