{- |
Module           : $Header$
Description      : Translation of JVM instructions to symbolic form
Stability        : stable
Point-of-contact : atomb
-}

module Verifier.Java.SymTranslation
  ( liftInsn
  --, liftCFG
  , SymCond(..)
  , CmpType(..)
  , InvokeType(..)
  , SymInstruction(..)
  , SymBlock(..)
  ) where

import Prelude hiding (EQ, LT, GT)

import Language.JVM.CFG
import Language.JVM.Common

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
  -- | @Compare ty@ holds if the given comparison is true between the
  -- top element of the stack and the next-to-top element.
  | Compare CmpType

data CmpType
  = EQ
  | NE
  | LT
  | GE
  | GT
  | LE

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

data SymBlock = SymBlock {
    sbPC :: PC
  , sbInsns :: [(PC, SymInstruction)]
  }

--liftCFG :: CFG -> Map PC SymBlock

liftInsn :: CFG -> PC -> Instruction -> [SymInstruction]
liftInsn cfg npc i =
  case i of
    Areturn -> [OtherInsn i, MergeReturn]
    Dreturn -> [OtherInsn i, MergeReturn]
    Freturn -> [OtherInsn i, MergeReturn]
    Lreturn -> [OtherInsn i, MergeReturn]
    Ireturn -> [OtherInsn i, MergeReturn]
    Return -> [OtherInsn i, MergeReturn]
    Invokeinterface n k ->
      [PushInvokeFrame InvInterface (ClassType n) k npc]
    Invokespecial t k ->
      [PushInvokeFrame InvSpecial t k npc]
    Invokestatic n k ->
      [PushInvokeFrame InvStatic (ClassType n) k npc]
    Invokevirtual t k ->
      [PushInvokeFrame InvVirtual t k npc]
    Goto pc -> [SetCurrentPC pc]
    If_acmpeq pc -> br pc EQ
    If_acmpne pc -> br pc NE
    If_icmpeq pc -> br pc EQ
    If_icmpne pc -> br pc NE
    If_icmplt pc -> br pc LT
    If_icmpge pc -> br pc GE
    If_icmpgt pc -> br pc GT
    If_icmple pc -> br pc LE
    Ifeq pc -> cmpZero (If_icmpeq pc)
    Ifne pc -> cmpZero (If_icmpne pc)
    Iflt pc -> cmpZero (If_icmplt pc)
    Ifge pc -> cmpZero (If_icmpge pc)
    Ifgt pc -> cmpZero (If_icmpgt pc)
    Ifle pc -> cmpZero (If_icmple pc)
    Ifnonnull pc -> [PushPendingExecution NonNull]
    Ifnull pc -> [PushPendingExecution Null]
    {-
    Lookupswitch d cs ->
      (SetCurrentPC d, PushPendingExecution (NotConstValues (map fst cs))) :
      map (\(i, pc) -> (SetCurrentPC pc, PushPendingExecution (ConstValue i))) cs
    Tableswitch d l h cs ->
      (SetCurrentPC d, PushPendingExecution (NotConstValues is)) :
      map (\(i, pc) -> (SetCurrentPC pc, PushPendingExecution (ConstValue i))) (zip is cs)
        where is = [l..h]
    Jsr pc -> undefined
    Ret -> undefined
    -}
    _ -> [OtherInsn i]
    where cmpZero = (OtherInsn (Ldc (Integer 0)) :) . liftInsn cfg npc
          br pc cmp = [SetCurrentPC pc, PushPendingExecution (Compare cmp)]
