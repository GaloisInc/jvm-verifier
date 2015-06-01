{- |
Module           : $Header$
Description      : Translation of JVM instructions to symbolic form
Stability        : experimental
Point-of-contact : atomb, acfoltzer
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Data.JVM.Symbolic.Translation
  ( liftBB
  , liftCFG
  , symBlockMap
  , SymCond(..)
  , CmpType(..)
  , InvokeType(..)
  , SymInsn(..)
  , SymBlock(..)
  , ppSymBlock
  , ppSymInsn
  , ppBlockId
  , SymTransWarning
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.RWS hiding ((<>))
import Data.Int
import qualified Data.Map as M
import Data.Maybe
import Prelude hiding (EQ, LT, GT)
import Text.PrettyPrint

import Data.JVM.Symbolic.AST

import Language.JVM.CFG    
import Language.JVM.Common 

data SymBlock = SymBlock {
    sbId :: BlockId
  , sbInsns :: [(Maybe PC, SymInsn)]
  }
  deriving (Eq)

instance Ord SymBlock where
  compare (SymBlock { sbId = x }) (SymBlock { sbId = y }) = compare x y

ppSymBlock :: SymBlock -> Doc
ppSymBlock SymBlock { sbId, sbInsns } =
    ppBlockId sbId <> colon $+$ nest 2 insns
  where insns = foldr ($+$) mempty 
                  [ maybe "%" ppPC pc <> colon <+> ppSymInsn insn 
                  | (pc, insn) <- sbInsns 
                  ]

type SymTransWarning = Doc

type SymTrans a = RWS () [SymTransWarning] [SymBlock] a

bbToBlockId :: BBId -> BlockId
bbToBlockId bb = BlockId bb 0

defineBlock :: BlockId -> [(Maybe PC, SymInsn)] -> SymTrans ()
defineBlock bid insns = modify (SymBlock bid insns :)

liftCFG :: CFG -> ([SymBlock], [SymTransWarning])
liftCFG cfg = (initBlock : bs, ws)
    where (bs, ws)     = execRWS (mapM_ (liftBB cfg) normalBlocks) () []
          normalBlocks = filter isNormal (map bbId (allBBs cfg))
          initBlock    = SymBlock {
                         sbId = bbToBlockId BBIdEntry
                       , sbInsns = [ si (SetCurrentBlock (BlockId (BBId 0) 0)) ]
                       }

symBlockMap :: [SymBlock] -> M.Map BlockId [SymInsn]
symBlockMap symblocks = M.fromList [ (bid, map snd insns) 
                                   | SymBlock bid insns <- symblocks
                                   ]

liftBB :: CFG -> BBId -> SymTrans ()
liftBB cfg bb = do
  let blk = bbToBlockId bb
      pd = case filter (isImmediatePostDominator cfg bb) 
                $ getPostDominators' cfg bb of
             [] -> Nothing
             [pdbb] -> Just $ bbToBlockId pdbb
             _ -> error . render $
                    "more than one immediate postdominator for" <+> ppBlockId blk
      processInsns [] currId [] = defineBlock currId []
      processInsns [] currId il@((mpc, _) : _) =
        defineBlock currId 
          (reverse il ++
           maybe [] (brSymInstrs cfg . bbToBlockId . BBId) (join $ nextPC cfg `fmap` mpc))
      processInsns ((pc,i):is) currId il =
        let blk' = fromMaybe (bbToBlockId BBIdExit) $ nextBlk pc
            blk'' = blk { blockN = blockN currId + 1 }
            blk''' = if null is then blk' else blk''
            warn msg = tell $ ["warning in" <+> ppBlockId currId <> colon <+> msg]
        in case i of
          Areturn -> retVal pc currId il
          Dreturn -> retVal pc currId il
          Freturn -> retVal pc currId il
          Lreturn -> retVal pc currId il
          Ireturn -> retVal pc currId il
          Return -> retVoid pc currId il
          Invokeinterface n k -> do

            defineBlock currId $ reverse
              (si (PushInvokeFrame InvInterface (ClassType n) k blk''') : il)
            processInsns is blk''' []
          Invokespecial t k -> do
            defineBlock currId $ reverse
              (si (PushInvokeFrame InvSpecial t k blk''') : il)
            processInsns is blk''' []
          Invokestatic n k -> do
            defineBlock currId $ reverse
              (si (PushInvokeFrame InvStatic (ClassType n) k blk''') : il)
            processInsns is blk''' []
          Invokevirtual t k -> do
            defineBlock currId $ reverse
              (si (PushInvokeFrame InvVirtual t k blk''') : il)
            processInsns is blk''' []
          Goto tgt ->
            defineBlock currId $
            reverse il ++ brSymInstrs cfg (getBlock tgt)
          If_acmpeq tgt -> br pc currId il (getBlock tgt) blk' (CompareRef EQ)
          If_acmpne tgt -> br pc currId il (getBlock tgt) blk' (CompareRef NE)
          If_icmpeq tgt -> br pc currId il (getBlock tgt) blk' (Compare EQ)
          If_icmpne tgt -> br pc currId il (getBlock tgt) blk' (Compare NE)
          If_icmplt tgt -> br pc currId il (getBlock tgt) blk' (Compare LT)
          If_icmpge tgt -> br pc currId il (getBlock tgt) blk' (Compare GE)
          If_icmpgt tgt -> br pc currId il (getBlock tgt) blk' (Compare GT)
          If_icmple tgt -> br pc currId il (getBlock tgt) blk' (Compare LE)
          Ifeq tgt -> cmpZero pc (If_icmpeq tgt) currId is il
          Ifne tgt -> cmpZero pc (If_icmpne tgt) currId is il
          Iflt tgt -> cmpZero pc (If_icmplt tgt) currId is il
          Ifge tgt -> cmpZero pc (If_icmpge tgt) currId is il
          Ifgt tgt -> cmpZero pc (If_icmpgt tgt) currId is il
          Ifle tgt -> cmpZero pc (If_icmple tgt) currId is il
          Ifnonnull tgt -> br pc currId il (getBlock tgt) blk' NonNull
          Ifnull tgt -> br pc currId il (getBlock tgt) blk' Null
          Lookupswitch d cs -> switch currId il d (map fst cs) cs
          Tableswitch d l h cs -> switch currId il d vs (zip vs cs)
            where vs = [l..h]
          Jsr _ -> do warn "jsr not supported"
                      processInsns is currId il
          Ret _ -> do warn "ret not supported"
                      processInsns is currId il
          _ -> processInsns is currId ((Just pc, NormalInsn i) : il)
      getBlock pc = maybe
                    (error $ "No block for PC: " ++ show pc)
                    (bbToBlockId . bbId)
                    (bbByPC cfg pc)
      nextBlk pc =
        case nextPC cfg pc of
          Nothing -> Just $ bbToBlockId BBIdExit
          Just pc' -> Just $ getBlock pc'
      cmpZero pc i' currId is il =
        processInsns ((pc, i'):is) currId
          (si (NormalInsn (Ldc (Integer 0))) : il)
      retVal pc currId il =
        defineBlock currId $ reverse ((Just pc, ReturnVal) : il)
      retVoid pc currId il =
        defineBlock currId $ reverse ((Just pc, ReturnVoid) : il)
      switch :: BlockId 
             -> [(Maybe PC, SymInsn)] 
             -> PC
             -> [Int32] 
             -> [(Int32, PC)]
             -> SymTrans ()
      switch currId il d is cs = do
        defineBlock currId $ reverse il ++ cases
        zipWithM_ defineBlock caseBlockIds (brSymInstrs cfg <$> targets)
          where targets = getBlock . snd <$> cs
                caseBlockIds = 
                  [ currId { blockN = n } 
                  | n <- [  blockN currId + 1 .. blockN currId + length cs]
                  ]
                cases = foldr mkCase (brSymInstrs cfg (getBlock d))
                         $ zip (fromIntegral <$> is) caseBlockIds
                mkCase (cv, bid) rest = 
                  [ si (PushPendingExecution bid (HasConstValue cv) pd rest) ]
      br pc currBlk il thenBlk elseBlk cmp = do
        let suspendBlk = currBlk { blockN = (blockN currBlk) + 1 }
        -- Add pending execution for true branch, and execute false branch.
        -- If we want to do it the other way, we can negate the condition
        defineBlock currBlk $ reverse $
          ((Just pc, (PushPendingExecution suspendBlk cmp pd
                                    [si (SetCurrentBlock elseBlk)]))
          : il)
        -- Define block for suspended thread.
        defineBlock suspendBlk (brSymInstrs cfg thenBlk)
  case bbById cfg bb of
    Just basicBlock -> processInsns (bbInsts basicBlock) blk []
    Nothing -> error $ "Block not found: " ++ show bb

-- simplified from previous sym translation, as we no longer need
-- explicit merge instructions
brSymInstrs :: CFG -> BlockId -> [(Maybe PC, SymInsn)]
brSymInstrs _cfg tgt = [si (SetCurrentBlock tgt)]

getPostDominators' :: CFG -> BBId -> [BBId]
getPostDominators' cfg = filter isNormal . getPostDominators cfg

isNormal :: BBId -> Bool
isNormal (BBId _) = True
isNormal _ = False

si :: SymInsn -> (Maybe PC, SymInsn)
si i = (Nothing, i)
