{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the main data types for the AST interpreted
-- by the symbolic simulator. It enriches the JVM instruction set with
-- operations for handling symbolic execution paths and explicitly
-- calling and returning from methods.
module Data.JVM.Symbolic.AST where

import Prelude hiding (EQ, LT, GT)
import Text.PrettyPrint

import qualified Language.JVM.CFG    as CFG
import qualified Language.JVM.Common as J
import qualified Language.JVM.Parser as J

data BlockId = BlockId { blockId :: !CFG.BBId, blockN :: !Int }

ppBlockId :: BlockId -> Doc
ppBlockId (BlockId b n) = CFG.ppBBId b <> "." <> int n

-- | Different types of invocations
data InvokeType
  = InvInterface
  | InvSpecial
  | InvStatic
  | InvVirtual

ppInvokeType :: InvokeType -> Doc
ppInvokeType it = case it of
    InvInterface -> "interface"
    InvSpecial   -> "special"
    InvStatic    -> "static"
    InvVirtual   -> "virtual"

-- | A merge location is a block or @Nothing@ if the merge happens at a return.
type MergeLocation = Maybe BlockId

-- | Symbolic instructions
data SymStmt
  -- | @PushInvokeFrame it ty key pc@ pushes a invoke frame to the merge
  -- frame stack that will call @ty.k@. The calling method will resume
  -- execution at @pc@
  = PushInvokeFrame InvokeType J.Type J.MethodKey BlockId
  -- | @ReturnVoid@ pops top call frame from path, merges path states
  -- if appropriate, and clears current path.
  | ReturnVoid
  -- | @ReturnVal@ pops top call frame from path, adds the returned
  -- value to the stack of the previous call frame, merges path states
  -- if appropriate, and clears the current path.
  | ReturnVal
  -- | @PushPendingExecution bid c ml elseStmts@ arises when
  -- simulating a branch. The true branch at @bid@ is pushed onto the
  -- control stack, and the false branch (@elseStmts@) are
  -- executed. @ml@ is the control point where the branched paths
  -- should merge.
  | PushPendingExecution BlockId SymCond MergeLocation [SymStmt]
  -- | Sets the block to the given location. This may trigger a merge
  -- if the location is a @MergeLocation@.
  | SetCurrentBlock BlockId
  -- | A JVM instruction we don't support for symbolic simulation
  | BadInsn J.Instruction
  -- | Any other non-control-flow instruction. Stepped normally.
  | NormalInsn J.Instruction

ppSymStmt :: SymStmt -> Doc
ppSymStmt stmt = case stmt of
    PushInvokeFrame it ty key bid -> 
        "pushInvokeFrame" <+> ppInvokeType it
        <+> J.ppType ty <> "." <> J.ppMethodKey key
        <+> "returns to" <+> ppBlockId bid
    ReturnVoid ->
        "returnVoid"
    ReturnVal ->
        "returnVal"
    PushPendingExecution bid c ml elseStmts ->
        "pushPendingExecution" <+> ppBlockId bid <+> ppSymCond c
        <+> "merge" <+> maybe "return" ppBlockId ml
        $+$ vcat (map ppSymStmt elseStmts)
    SetCurrentBlock bid ->
        "setCurrentBlock" <+> ppBlockId bid
    BadInsn insn ->
        "badInsn" <+> J.ppInstruction insn
    NormalInsn insn ->
        J.ppInstruction insn

-- | Predicates in the context of the symbolic simulator
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

ppSymCond :: SymCond -> Doc
ppSymCond c = case c of 
    HasConstValue i   -> top <+> "==" <+> integer i
    NotConstValues is -> top <+> "not in" <+> ppIntList is 
    Null              -> top <+> "==" <+> null
    NonNull           -> top <+> "!=" <+> null
    TrueSymCond       -> "true"
    Compare cmp       -> top <+> ppCmpType cmp <+> "<second elt of stack>"
  where top = "<top of stack>"
        null = "null"
        ppIntList = brackets . commas . map integer

data CmpType
  = EQ
  | NE
  | LT
  | GE
  | GT
  | LE

ppCmpType :: CmpType -> Doc
ppCmpType cmp = case cmp of
    EQ -> "==" ; NE -> "!=" ; LT -> "<" ; GE -> ">=" ; GT -> ">" ; LE -> "<="

commas :: [Doc] -> Doc
commas = sep . punctuate comma