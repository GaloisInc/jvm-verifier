{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : atomb
-}

module Verifier.Java.SymStepper
  ( step
  , SymCond(..)
  , InvokeType(..)
  , SymInstruction(..)
  ) where

import Execution.JavaSemantics
import qualified Execution.Stepper as Step
import Verifier.Java.Codebase

data SymCond
  -- | @HasConstValue i@ holds if the top element on the stack
  -- corresponds to the constant @i@.
  = HasConstValue Integer
  -- | @NotConstValues is@ holds if the top element on the stack does
  -- not correspond to any of the constants in @is@.
  | NotConstValues [Integer]
  -- | @Null@ holds if the top element on the stack is null.
  | Null
  -- | @Null@ holds if the top element on the stack is non-null.
  | NonNull
  -- | @TrueSymCond@ always holds.
  | TrueSymCond

data InvokeType
  = InvInterface
  | InvSpecial
  | InvStatic
  | InvVirtual

data SymInstruction
  -- | @PushInvokeFrame s ty k pc@ pushes a invoke frame to the merge
  -- frame stack that will call @ty.k@. The calling method will resume
  -- execution at @pc@
  = PushInvokeFrame InvokeType Type MethodKey PC
  -- | @PushPostDominatorFrame pc@ pushes a new frame to the merge
  -- frame stack for a post-dominator at the given PC. This
  -- instruction is used when we jump into a block that has a
  -- different immediate post-dominator than its parent.
  | PushPostDominatorFrame PC
  -- | @MergePostDominator pc@ merges the current path state with the
  -- post-dominator return path under the given condition, and clears
  -- the current path state. N.B. The current state must be unchanged.
  -- However, the current block of the merged state must be the
  -- post-dominator block.
  | MergePostDominator PC 
  -- | @MergeReturn dc@ pops top call frame from path, merges (current
  -- path return value) with call frame, and clears current path.
  | MergeReturn
  -- | @PushPendingExecution c@ makes the current state a pending
  -- execution in the top-most merge frame with the additional path
  -- constraint @c@.
  | PushPendingExecution SymCond
  -- | Sets the block to the given location.
  | SetCurrentPC PC
  -- | Any other non-control-flow instruction. Stepped normally.
  | OtherInsn Instruction

step :: (JavaSemantics m) => SymInstruction -> m ()
step (PushInvokeFrame s ty k pc) = return () -- TODO!
step (PushPostDominatorFrame pc) = return () -- TODO!
step (MergePostDominator pc) = return () -- TODO!
step (MergeReturn) = return () -- TODO!
step (PushPendingExecution c) = return () -- TODO!
step (SetCurrentPC pc) = return () -- TODO!
step (OtherInsn i) = Step.step i
