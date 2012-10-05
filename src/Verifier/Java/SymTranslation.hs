{- |
Module           : $Header$
Description      : Translation of JVM instructions to symbolic form
Stability        : stable
Point-of-contact : atomb
-}

module Verifier.Java.SymTranslation
  ( liftInsn
  --, liftCFG
  , SymBlock(..)
  ) where

import Language.JVM.CFG
import Language.JVM.Common
import Verifier.Java.SymStepper

data SymBlock = SymBlock {
    sbPC :: PC
  , sbInsns :: [(PC, SymInstruction)]
  }

--liftCFG :: CFG -> Map PC SymBlock

liftInsn :: PC -> Instruction -> [SymInstruction]
liftInsn npc i =
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
    -- TODO: unsure of the best way to encode conditional expressions,
    -- since they imply popping an element from the stack and then
    -- branching.
    {-
    If_acmpeq pc -> undefined
    If_acmpne pc -> undefined
    If_icmpeq pc -> undefined
    If_icmpne pc -> undefined
    If_icmplt pc -> undefined
    If_icmpge pc -> undefined
    If_icmpgt pc -> undefined
    If_icmple pc -> undefined
    -}
    Ifeq pc -> cmpZero (If_icmpeq pc)
    Ifne pc -> cmpZero (If_icmpne pc)
    Iflt pc -> cmpZero (If_icmplt pc)
    Ifge pc -> cmpZero (If_icmpge pc)
    Ifgt pc -> cmpZero (If_icmpgt pc)
    Ifle pc -> cmpZero (If_icmple pc)
    {-
    Ifnonnull pc -> undefined
    Ifnull pc -> undefined
    Lookupswitch d cs -> undefined
    Tableswitch d l h cs -> undefined
    Jsr pc -> undefined
    Ret -> undefined
    -}
    _ -> [OtherInsn i]
    where cmpZero = (OtherInsn (Ldc (Integer 0)) :) . liftInsn npc 
