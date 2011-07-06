{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : atomb
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ProofUtils ( module ProofUtils
                  , module Control.Monad
                  , module Control.Monad.Trans
                  , module JavaParser
                  , module Simulation
                  , module SBVParser
                  , module Symbolic
                  , module MethodSpec
                  ) where

import Control.Monad
import Control.Monad.Trans
import Prelude hiding (catch)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import JavaParser
import SBVParser
import Simulation hiding ((&&&), (|||), loadClass)
import Symbolic hiding (SymbolicTerm, liftIO)
import MethodSpec
import Utils.IOStateT

trunc :: OpDef -> Term -> Term
trunc op x =
    let SymInt wx = termType x
        s = emptySubst { widthSubst = Map.fromList [("x", wx)] }
    in appTerm (mkOp op s) [x]

ite :: Term -> Term -> Term -> Term
ite c t e =
  let tp = termType t
      s = TypeSubst { shapeSubst = Map.fromList [("x", tp)]
                    , widthSubst = Map.empty }
   in appTerm (mkOp iteOpDef s) [c, t, e]

band :: WidthExpr -> Term -> Term -> Term
band w a b =
  let s = TypeSubst { shapeSubst = Map.empty
                    , widthSubst = Map.fromList [("x", w)] }
   in appTerm (mkOp iAndOpDef s) [a, b]

type OpFn sym = V.Vector (MonadTerm sym) -> sym (MonadTerm sym)

type RuleMonad a = StateT [Rule] OpSession a

defRule :: String -> Term -> Term -> RuleMonad ()
defRule name t u = r `seq` modify (r :)
  where r = mkRule name t u

runRuleMonad :: RuleMonad () -> OpSession [Rule]
runRuleMonad m = fmap (reverse . snd) $ runStateT m []

commonJars :: [String]
commonJars = [ "../../jdk1.6/classes.jar"        -- jdk
             , "../../support/galois.jar"        -- JAPI
             ]

fieldMethodSpec :: String
                -> MethodKey
                -> [([SpecJavaExpr], SpecType)]
                -> SpecExpr sym
                -> [(SpecJavaExpr, RefChange sym)]
                -> MethodSpec sym
fieldMethodSpec specClass specMethodKey argRefs specPrecondition changes =
  let fieldPrimeId = FieldId { fieldIdClass = "com/galois/ecc/ECCProvider"
                             , fieldIdName = "field_prime"
                             , fieldIdType = ArrayType IntType }
  in MethodSpec
       { specClass
       , specMethodKey
       , specInitializedClasses =
         [ "com/galois/ecc/ECCProvider"
         , "com/galois/ecc/NIST64"
         , "com/galois/ecc/P384ECC64" ]
       , specRefs =
         [ ( [This], SpecRefClass "com/galois/ecc/P384ECC64")
         , ( [InstanceField This fieldPrimeId]
           , SpecIntArrayConstant $ V.fromListN 12 [-1, 0, 0, -1, -2, -1, -1, -1, -1, -1, -1, -1])
         ] ++ argRefs
       , specPrecondition
       , specAssertions = []
       , specChanges = changes
       }

appList :: ([MonadTerm sym] -> sym (MonadTerm sym))
        -> [SpecExpr sym]
        -> SpecExpr sym
appList fn expr = SymApp (fn . V.toList) (V.fromList expr)

-- Field operation that takes return vector and one argument.
unaryFieldOp :: String -> MethodKey
unaryFieldOp methNm = makeMethodKey methNm "([I[I)V"

-- Field operation that takes return vector and two argument.
binaryFieldOp :: String -> MethodKey
binaryFieldOp methNm = makeMethodKey methNm "([I[I[I)V"

trivialCond ::
  (Monad sym, ConstantInjection (MonadTerm sym))
  => SpecExpr sym
trivialCond =
  let checkValFn _ = return (mkCBool True)
  in SymApp checkValFn V.empty

validFieldValues :: WordMonad sym =>
                    (V.Vector a -> sym (MonadTerm sym))
                 -> (V.Vector (MonadTerm sym) -> sym a)
                 -> [SpecJavaExpr]
                 -> SpecExpr sym
validFieldValues refIsValFn fieldJoinFn argIndices =
  let argVec          = V.fromList argIndices
      checkValFn args = do
        preds <- V.forM (V.enumFromN 0 (V.length argVec)) $ \i -> do
                   r <- fieldJoinFn (V.singleton $ args V.! i)
                   refIsValFn (V.singleton r)
        V.fold1M applyBAnd preds
  in SymApp checkValFn (V.map ArrayValue argVec)

aId :: FieldId
aId = FieldId { fieldIdClass = "com/galois/ecc/NIST64"
              , fieldIdName = "a"
              , fieldIdType = ArrayType IntType }

w32 :: DagType
w32 = SymInt (constantWidth 32)

w384 :: DagType
w384 = SymInt (constantWidth 384)

arrType :: DagType
arrType = SymArray (constantWidth 12) w32

affineRecDef :: OpSession SymRecDef
affineRecDef = getStructuralRecord (Set.fromList ["x", "y"])

affRecSubst, jacRecSubst :: DagType -> TypeSubst
affRecSubst fieldType =
  emptySubst { shapeSubst = Map.fromList [("x", fieldType), ("y", fieldType)] }
jacRecSubst fieldType =
  emptySubst { shapeSubst = 
    Map.fromList [("x", fieldType), ("y", fieldType), ("z", fieldType)] }

affineRec :: SymRecDef -> DagType -> DagType
affineRec recDef fieldType = SymRec recDef (affRecSubst fieldType)

mkAffineOp :: SymRecDef -> DagType -> Op
mkAffineOp recDef fieldType =
  mkOp (recDefCtor recDef) (affRecSubst fieldType)

affineFields :: SymRecDef -> V.Vector OpDef
affineFields = recDefFieldOps

affineXOp :: SymRecDef -> OpDef
affineXOp recDef = affineFields recDef V.! 0

affineYOp :: SymRecDef -> OpDef
affineYOp recDef = affineFields recDef V.! 1

jacobianRecDef :: OpSession SymRecDef
jacobianRecDef = getStructuralRecord (Set.fromList ["x", "y", "z"])

jacobianRec :: SymRecDef -> DagType -> DagType
jacobianRec recDef fieldType = SymRec recDef (jacRecSubst fieldType)

mkJacobianOp :: SymRecDef -> DagType -> Op
mkJacobianOp = mkAffineOp

jacobianFields :: SymRecDef -> V.Vector OpDef
jacobianFields = recDefFieldOps

jacobianXOp :: SymRecDef -> OpDef
jacobianXOp recDef = jacobianFields recDef V.! 0

jacobianYOp :: SymRecDef -> OpDef
jacobianYOp recDef = jacobianFields recDef V.! 1

jacobianZOp :: SymRecDef -> OpDef
jacobianZOp recDef = jacobianFields recDef V.! 2

