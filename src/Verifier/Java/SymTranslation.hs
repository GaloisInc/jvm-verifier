{- |
Module           : $Header$
Description      : Translation of JVM instructions to symbolic form
Stability        : experimental
Point-of-contact : atomb
-}

module Verifier.Java.SymTranslation
  (liftBB
  , liftCFG
  , SymCond(..)
  , CmpType(..)
  , InvokeType(..)
  , SymInstruction(..)
  , SymBlock(..)
  ) where

import Control.Monad.State
import qualified Data.List as L
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
    deriving (Show)

data CmpType
  = EQ
  | NE
  | LT
  | GE
  | GT
  | LE
    deriving (Show)

data InvokeType
  = InvInterface
  | InvSpecial
  | InvStatic
  | InvVirtual
    deriving (Show)

data SymInstruction
  -- | @PushInvokeFrame s ty k pc@ pushes a invoke frame to the merge
  -- frame stack that will call @ty.k@. The calling method will resume
  -- execution at @pc@
  = PushInvokeFrame InvokeType Type MethodKey BlockId
  -- | @PushPostDominatorFrame pc@ pushes a new frame to the merge
  -- frame stack for a post-dominator at the given PC. This
  -- instruction is used when we jump into a block that has a
  -- different immediate post-dominator than its parent.
  | PushPostDominatorFrame BlockId
  -- | @MergePostDominator pc@ merges the current path state with the
  -- post-dominator return path under the given condition, and clears
  -- the current path state. N.B. The current state must be unchanged.
  -- However, the current block of the merged state must be the
  -- post-dominator block.
  | MergePostDominator BlockId
  -- | @MergeReturn dc@ pops top call frame from path, merges (current
  -- path return value) with call frame, and clears current path.
  | MergeReturn
  -- | @PushPendingExecution c@ makes the current state a pending
  -- execution in the top-most merge frame with the additional path
  -- constraint @c@.
  | PushPendingExecution SymCond
  -- | Sets the block to the given location.
  | SetCurrentBlock BlockId
  -- | Any other non-control-flow instruction. Stepped normally.
  | OtherInsn Instruction
    deriving (Show)

data BlockId = BlockId { blockId :: !BBId, blockN :: !Int }
  deriving (Show)

data SymBlock = SymBlock {
    sbId :: BlockId
  , sbInsns :: [(Maybe PC, SymInstruction)]
  }

type SymTrans a = State [SymBlock] a

bbToBlockId :: BBId -> BlockId
bbToBlockId bb = BlockId bb 0

defineBlock :: BlockId -> [(Maybe PC, SymInstruction)] -> SymTrans ()
defineBlock bid insns = modify (SymBlock bid insns :)

liftCFG :: CFG -> [SymBlock]
liftCFG cfg = execState (mapM_ (liftBB cfg) (map bbId (allBBs cfg))) []

liftBB :: CFG -> BBId -> SymTrans ()
liftBB cfg bb = do
  let blk = bbToBlockId bb
      processInsns [] currId il = defineBlock currId (reverse il)
      processInsns ((pc,i):is) currId il =
        let blk' = case nextPC cfg pc of
                    Nothing -> error "no next PC"
                    Just pc' -> getBlock pc'
        in case i of
          Areturn -> ret pc i currId il
          Dreturn -> ret pc i currId il
          Freturn -> ret pc i currId il
          Lreturn -> ret pc i currId il
          Ireturn -> ret pc i currId il
          Return -> ret pc i currId il
          Invokeinterface n k ->
            defineBlock currId $ reverse
            (si (PushInvokeFrame InvInterface (ClassType n) k blk') : il)
          Invokespecial t k ->
            defineBlock currId $ reverse
            (si (PushInvokeFrame InvSpecial t k blk') : il)
          Invokestatic n k ->
            defineBlock currId $ reverse
            (si (PushInvokeFrame InvStatic (ClassType n) k blk') : il)
          Invokevirtual t k ->
            defineBlock currId $ reverse
            (si (PushInvokeFrame InvVirtual t k blk') : il)
          Goto tgt ->
            defineBlock currId $
            reverse il ++ brSymInstrs cfg currId (getBlock tgt)
          If_acmpeq tgt -> br blk' tgt (Compare EQ)
          If_acmpne tgt -> br blk' tgt (Compare NE)
          If_icmpeq tgt -> br blk' tgt (Compare EQ)
          If_icmpne tgt -> br blk' tgt (Compare NE)
          If_icmplt tgt -> br blk' tgt (Compare LT)
          If_icmpge tgt -> br blk' tgt (Compare GE)
          If_icmpgt tgt -> br blk' tgt (Compare GT)
          If_icmple tgt -> br blk' tgt (Compare LE)
          Ifeq tgt -> cmpZero pc (If_icmpeq tgt) currId is il
          Ifne tgt -> cmpZero pc (If_icmpne tgt) currId is il
          Iflt tgt -> cmpZero pc (If_icmplt tgt) currId is il
          Ifge tgt -> cmpZero pc (If_icmpge tgt) currId is il
          Ifgt tgt -> cmpZero pc (If_icmpgt tgt) currId is il
          Ifle tgt -> cmpZero pc (If_icmple tgt) currId is il
          Ifnonnull tgt -> br blk' tgt NonNull
          Ifnull tgt -> br blk' tgt Null
          {-
          Lookupswitch d cs ->
            (SetCurrentBlock d
            , PushPendingExecution (NotConstValues (map fst cs))) :
            map (\(i, pc) ->
                   ( SetCurrentBlock pc
                   , PushPendingExecution (ConstValue i))) cs
          Tableswitch d l h cs ->
            ( SetCurrentBlock d
            , PushPendingExecution (NotConstValues is)) :
            map (\(i, pc) ->
                   ( SetCurrentBlock pc
                   , PushPendingExecution (ConstValue i))) (zip is cs)
              where is = [l..h]
          Jsr pc -> undefined
          Ret -> undefined
          -}
          _ -> processInsns is currId ((Just pc, OtherInsn i) : il)
      getBlock pc = maybe
                    (error $ "No block for PC: " ++ show pc)
                    (bbToBlockId . bbId)
                    (bbByPC cfg pc)
      cmpZero pc i' currId is il =
        processInsns ((pc, i'):is) currId
          (si (OtherInsn (Ldc (Integer 0))) : il)
      ret pc i currId il =
        defineBlock currId $
        reverse (si MergeReturn : (Just pc, OtherInsn i) : il)
      br blk' tgt cmp = do
        let suspendBlockID = blk { blockN = blockN blk + 1 }
        -- Add pending execution for false branch, and execute true branch.
        defineBlock blk
          ([ si (SetCurrentBlock suspendBlockID)
           , si (PushPendingExecution cmp)
           ] ++ brSymInstrs cfg blk (getBlock tgt))
        -- Define block for suspended thread.
        defineBlock suspendBlockID (brSymInstrs cfg suspendBlockID blk')
  case bbById cfg bb of
    Just basicBlock -> processInsns (bbInsts basicBlock) blk []
    Nothing -> error $ "Block not found: " ++ show bb

-- Derived from respective code in LSS.
-- @brSymInstrs tgt@ returns the code for jumping to the target block.
-- Observations:
--  * A post-dominator of a post-dominator of the current block is itself
--    the post-dominator of the current block.
--    Consequently, branching to a post-dominator of the current block
--    cannot result in new nodes being needed.
-- Behavior:
--   For unconditional branches to tgt, do the following:
--     If tgt is the dominator
--       Set current block in state to branch target.
--       Merge this state with dominator state.
--       Clear current execution
--     Else
--       Set current block in state to branch target.
brSymInstrs :: CFG -> BlockId -> BlockId -> [(Maybe PC, SymInstruction)]
brSymInstrs cfg curr tgt =
  si (SetCurrentBlock tgt) :
    (if isImmediatePostDominator cfg (blockId curr) (blockId tgt)
     then [ si (MergePostDominator tgt) ]
     else map (\d -> (si (PushPostDominatorFrame d)))
              (newPostDominators cfg curr tgt))

newPostDominators :: CFG -> BlockId -> BlockId -> [BlockId]
newPostDominators cfg a b =
  map bbToBlockId $
  getPostDominators cfg (blockId a) L.\\
  getPostDominators cfg (blockId b)

si :: SymInstruction -> (Maybe PC, SymInstruction)
si i = (Nothing, i)
