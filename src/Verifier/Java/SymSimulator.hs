{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : atomb
-}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

module Verifier.Java.SymSimulator where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.State

import qualified Data.Map as M

import Execution.JavaSemantics
import qualified Execution.Stepper as Step
import Verifier.Java.Codebase
import Verifier.Java.Simulator
import Verifier.Java.SymTranslation

step :: (AigOps sym) => SymInstruction -> Simulator sym ()

-- First cut for invoke instructions is just to copy the cases from Execution.Stepper
step (PushInvokeFrame InvInterface (ClassType iName) key _) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef <- rPop
  dynBind iName key objectRef $ \iName' -> do
    gotoNextInstruction
    invokeInstanceMethod iName' key objectRef (reverse reverseArgs)
step (PushInvokeFrame InvSpecial (ClassType methodClass) key _) = do
  currentClassName <- getCurrentClassName
  reverseArgs      <- replicateM (length (methodKeyParameterTypes key)) popValue
  cb               <- getCodebase
  currentClass     <- liftIO $ lookupClass cb currentClassName
  objectRef        <- rPop
  objectRefAsCC    <- coerceRef objectRef $ ClassType (className currentClass)
  b                <- liftIO $ isStrictSuper cb methodClass currentClass
  let args          = reverse reverseArgs
      call cl       = do gotoNextInstruction
                         invokeInstanceMethod cl key objectRef args
  if classHasSuperAttribute currentClass && b && methodKeyName key /= "<init>"
    then do
      dynBind' methodClass key objectRef $ \cl ->
        objectRefAsCC `superHasType` cl |-> call cl
    else
      forkM (isNull objectRef)
            (createAndThrow "java/lang/NullPointerException")
            (call methodClass)
step (PushInvokeFrame InvSpecial (ArrayType _methodType) key _) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef <- rPop
  forkM (isNull objectRef)
        (createAndThrow "java/lang/NullPointerException")
        (do gotoNextInstruction
            invokeInstanceMethod "java/lang/Object" key objectRef $ reverse reverseArgs)
step (PushInvokeFrame InvSpecial _ _ _) = error "internal: unexpected Invokespecial form"
step (PushInvokeFrame InvStatic (ClassType cName) key _) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  gotoNextInstruction
  invokeStaticMethod cName key (reverse reverseArgs)
step (PushInvokeFrame InvVirtual (ArrayType _methodType) key _) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef <- rPop
  forkM (isNull objectRef)
        (createAndThrow "java/lang/NullPointerException")
        (do gotoNextInstruction
            invokeInstanceMethod "java/lang/Object" key objectRef $ reverse reverseArgs)
step (PushInvokeFrame InvVirtual (ClassType cName) key _) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef   <- rPop
  dynBind cName key objectRef $ \cName' -> do
    gotoNextInstruction
    invokeInstanceMethod cName' key objectRef (reverse reverseArgs)
step (PushInvokeFrame _ _ _ _) = error "internal: unexpected PushInvokeFrame form"

-- These are generated for each postdom in the CFG, so to figure out
-- what to do, we need to identify all instructions which yield a
-- postdom. Should be all the forkM/choice calls, but the bodies of
-- those are an impedance mismatch
step (PushPostDominatorFrame pc) = return () -- TODO!

-- Mismatch here: the current architecture waits until hitting a
-- return frame to fold the merge over all of the completed
-- paths. However, if we want to mirror the LSS structure we need to
-- do that more on-demand.
step (MergePostDominator pc) = return () -- TODO!

-- We need to have a return value here in order to know what to do...
step (MergeReturn) = return () -- TODO!

-- This should probably be ~half of Simulator.onNewPath
step (PushPendingExecution c) = return () -- TODO!

-- This should probably be the other ~half of Simulator.onNewPath
step (SetCurrentBlock blockId) = return ()

-- TODO: check for control-flow-affecting instructions to make sure
-- we're never interpreting them
step (OtherInsn i) = Step.step i
