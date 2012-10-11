{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : atomb
-}

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
step (SetCurrentPC pc) = return () -- TODO!
step (OtherInsn i) = Step.step i
