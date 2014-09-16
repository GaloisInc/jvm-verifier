{- |
Module           : $Header$
Description      : The (default) symbolic backend.  Defines operations over a term DAG.
Stability        : provisional
Point-of-contact : jhendrix, jstanley
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Verinf.Symbolic (
   -- * Lit operations
    module Verinf.Symbolic.Lit
  , Lit
  , createBitEngine
  -- * Lit vector operations
  , BitWidth (..)
  , maxBitIdx
  , bitWidthSize
  , numBits
  , fromLsbf
  , fromMsbf
  , toLsbf_lit
  , toLsbfV
  , toMsbf
  , LitResult(..)
  , mapLitResult
  , flattenLitResult
  , boolLitResultFromCValue
  , litToLitResult
  , lMkInputLitResult
  -- * DagType
  -- ** Width expressions
  , WidthVar
  , WidthExpr
  , constantWidth
  , varWidth
  , addWidth
  , widthConstant
  , widthVar
  , ppWidthExpr
  -- ** DagType expressions
  , ShapeVar
  , SymTypeVar(..)
  , SymRecDef, recDefFieldTypes, recDefCtor, recDefFieldOps
  , DagType(..)
  , ppType
  , defaultValue
  , TypeSubst(..)
  , emptySubst
  , idSubst
  , matchSubst
  , applyTypeSubst
  , recDefParams
  , recFieldTypes
  -- * Constant values
  , CValue(..)
  , ConstantInjection(..)
  , ConstantProjection(..)
  , mkCIntFromLsbfV
  , lFromCValue
  -- * OpCache
  , module Verinf.Symbolic.OpCache
  -- * Symbolic level operations.
  -- ** Ops
  , OpPrec
  , OpDef
  , opDefName
  , opDefArgTypes
  , opDefResultType
  , opDefFnType
  , opDefDefinition
  , defaultPrec
  , Op, opDef, opSubst , mkOp
  , opName
  , opPrec
  , opArgTypes
  , opResultType
  , opFnType
  -- ** Nodes
  , TypedTerm(..)
  , TermClass(..)
  , DagTerm(ConstTerm)
  -- ** Rewriter terms
  , module Verinf.Symbolic.Rewriter
  -- ** Monadic interface
  , SymbolicExc(..)
  , evalAig
  , writeAiger
  , evalAndBlast
  -- ** Misc
  , PrettyNotation(..)
  , ppCValue
  , ppCValueD
  -- ** SExpression style Pretty printing
  , PPConfig(..), defaultPPConfig, PrettyTerm(..)
  -- ** Term Utility functions.
  , eq
  , (&&&)
  , (|||)
  , ushr
  , (#)
  , aget, aset
  -- ** TermSemantics
  , TermSemantics (..)
  , tsBoolConstant
  , tsIntConstant
  , mkTermSemantics
  , evalTermSemantics
  , mkBitBlastTermSemantics
  -- ** Dag engine
  , DagEngine(..)
  , mkExactDagEngine
  , mkConstantFoldingDagEngine
  , concreteEvalFn 
  , evalDagTerm
  , evalDagTermFn
  , deTermSemantics
  , deInputTypes
  ) where

-- Imports {{{1

import Control.Applicative
import Control.Monad
import Data.IORef
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Text.PrettyPrint.HughesPJ

import Verinf.Symbolic.Common
import Verinf.Symbolic.Dag
import Verinf.Symbolic.Lit
import Verinf.Symbolic.OpCache
import Verinf.Symbolic.Rewriter

import Verinf.Symbolic.Lit.ABC

-- Utility functions {{{1

-- | List-based AIG evaluation function.
evalAig :: SV.Storable l => BitEngine l -> [Bool] -> [l] -> IO [Bool]
evalAig be bl ll = SV.toList <$> beEvalAigV be (SV.fromList bl) (SV.fromList ll)

-- | List-based write Aiger function.
writeAiger :: SV.Storable l
           => BitEngine l
           -> FilePath -- ^ Filename
           -> [l] -- ^ Output lits
           -> IO ()
writeAiger be path l = do
  inputs <- beInputLits be
  beWriteAigerV be path inputs (SV.fromList l)

instance PrettyTerm Lit where
  prettyTerm = prettyLit
  prettyTermWithD _ = text . prettyLit

-- | Evaluates each node in dag on inputs and compares against bitblasted
-- version.  This is useful for tracking down bugs in AIG generation.
evalAndBlast :: (Eq l, SV.Storable l)
             => DagEngine
             -> BitEngine l
             -> Vector CValue
             -> Vector (LitResult l)
             -> IO ()
evalAndBlast de be cv lv = do
  -- Get concreate evaluation
  cEval <- concreteEvalFn cv
  lEval <- 
    let inputFn i _ = return (lv V.! i)
     in evalDagTermFn inputFn (mkBitBlastTermSemantics be)
  -- Get lit evaluator.
  let litEval = do
        let flattenCV v = flattenLitResult (boolLitResultFromCValue v)
        beEvalAigV be $ SV.concatMap (\i -> flattenCV (cv V.! i))
                      $ SV.enumFromN 0 (V.length cv)
  let runCompare n = do
        x <- cEval n
        lits <- lEval n
        let expected = flattenLitResult (boolLitResultFromCValue x)
        found <- litEval (flattenLitResult lits)
        unless (expected == found) $
          fail $ "Error in blasting term:\n" ++ prettyTerm n ++ "\n"
                     ++ "Expected: " ++ show expected ++ "\n"
                     ++ "Found:    " ++ show found
  cacheRef <- newIORef Set.empty
  let eval n = do
        case termApp n of
          Nothing -> return ()
          Just (op,args) -> do
            mr <- Set.member n <$> readIORef cacheRef
            unless mr $ do
              V.mapM_ eval args
              case opDefKind (opDef op) of
                DefinedOp rhs -> do
                  let inputFn i _ = return (args V.! i)
                  eval =<< evalDagTerm inputFn (deTermSemantics de) rhs
                _ -> runCompare n
              modifyIORef cacheRef (Set.insert n)
  nodes <- deGetTerms de
  mapM_ eval nodes

-- LitVector Circuit generators {{{1

-- | Literals in an int with least-significant bit first
toLsbf_lit :: SV.Storable l => LitResult l -> [l]
toLsbf_lit = SV.toList . toLsbfV

-- | Literals in an int with most-significant bit first
toMsbf :: SV.Storable l => LitResult l -> [l]
toMsbf = SV.toList . toMsbfV

-- | LitVector with literals ordered with least-significant bit first
fromLsbf :: SV.Storable l => BitWidth -> [l] -> LitResult l
fromLsbf (Wx n) lits = fromLsbfV (SV.fromListN n lits)

-- | LitVector with literals ordered with most-significant bit first
fromMsbf :: SV.Storable l => BitWidth -> [l] -> LitResult l
fromMsbf (Wx n) lits = fromMsbfV (SV.fromListN n lits)

-- Rewriter Term utility functions {{{1

-- | Return term indicating if two terms are equal.
eq :: Term -> Term -> Term
eq x y = appTerm (eqOp (termType x)) [x,y]

-- | Take conjunction of two rewriter terms.
(&&&) :: Term -> Term -> Term
(&&&) x y = appTerm bAndOp [x,y]

-- | Take disjunction of two rewriter terms.
(|||) :: Term -> Term -> Term
(|||) x y = appTerm bOrOp [x,y]

ushr :: Term -> Term -> Term
ushr v i =
  let SymInt wv = termType v
      SymInt ws = termType i
   in appTerm (ushrOp wv ws) [v, i]

(#) :: Term -> Term -> Term
(#) x y =
  case (termType x, termType y) of
    (SymInt wx, SymInt wy) -> appTerm (appendIntOp wx wy) [x, y]
    _ -> error "internal: (#) applied to arguments with incorrect types"

aget :: Term -> Term -> Term
aget a i =
  case (termType a, termType i) of
    (SymArray l e, SymInt idxType) ->
      appTerm (getArrayValueOp l idxType e) [a, i]
    _ -> error "Illegal types to aget"

aset :: Term -> Term -> Term -> Term
aset a i v =
  case (termType a, termType i) of
    (SymArray l e, SymInt idxType) ->
      appTerm (setArrayValueOp l idxType e) [a, i, v]
    _ -> error "Illegal types to aset"
