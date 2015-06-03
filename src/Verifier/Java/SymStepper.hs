{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : stable
Point-of-contact : atomb
-}

{-# OPTIONS_GHC -Wall #-}

module Verifier.Java.SymStepper
  ( step
  ) where

import Execution.JavaSemantics
import qualified Execution.Stepper as Step
import Verifier.Java.SymTranslation

step :: (JavaSemantics m) => SymInstruction -> m ()
step (PushInvokeFrame s ty k pc) = return () -- TODO!
step (PushPostDominatorFrame pc) = return () -- TODO!
step (MergePostDominator pc) = return () -- TODO!
step (MergeReturn) = return () -- TODO!
step (PushPendingExecution c) = return () -- TODO!
step (SetCurrentBlock blockId) = return ()
step (OtherInsn i) = Step.step i
