{- |
Module           : $Header$
Description      : Provides a mechanism for creating new dags.
Stability        : stable
Point-of-contact : jhendrix
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
module Verinf.Symbolic.Dag
  ( -- * DagEngine
    DagEngine(..)
  , deInputTypes
  , deGetInputCount
  , deTermSemantics
  , mkExactDagEngine
  , mkConstantFoldingDagEngine
  , SymbolicExc(..)
  ) where

-- Imports {{{1

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative hiding (empty)
#endif
import Control.Exception (assert, Exception)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as  Map
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as V

import Verinf.Symbolic.Common

-- DagEngine {{{1

data DagEngine = DagEngine {
    deApplyUnary   :: Op -> DagTerm                       -> IO DagTerm
  , deApplyBinary  :: Op -> DagTerm -> DagTerm            -> IO DagTerm
  , deApplyTernary :: Op -> DagTerm -> DagTerm -> DagTerm -> IO DagTerm
  , deApplyOp      :: Op -> Vector DagTerm                -> IO DagTerm
    -- | Create a fresh unnamed input term.
  , deFreshInput :: DagType -> IO DagTerm
  , deInputTerms :: IO (Vector DagTerm)
    -- | Returns application nodes stored in this dag.
    -- Used for debugging purposes.
  , deGetTerms :: IO [DagTerm]
  }

-- | Returns types of inputs so far.
deInputTypes :: DagEngine -> IO (Vector DagType)
deInputTypes de = V.map termType <$> deInputTerms de 

-- | Return number of input nodes created so far.
deGetInputCount :: DagEngine -> IO Int
deGetInputCount de = V.length <$> deInputTerms de 

-- | Create TermSemantics from DagEngine
deTermSemantics :: DagEngine -> TermSemantics IO DagTerm
deTermSemantics de =
  TermSemantics {
      tsConstant     = \c tp     -> return $ ConstTerm c tp
    , tsApplyUnary   = \op mx    -> deApplyUnary de op =<< mx
    , tsApplyBinary  = \op mx my -> do x <- mx
                                       y <- my
                                       deApplyBinary de op x y
    , tsApplyTernary = \op mx my mz -> do x <- mx
                                          y <- my
                                          z <- mz
                                          deApplyTernary de op x y z
    , tsApplyOp = \op args -> deApplyOp de op =<< V.sequence args
    }

-- evalTermFn {{{1

-- DagEngine implementations {{{1

-- | State of the node dag engine.
data NodeDagEngineState = NodeDagEngineState {
    -- | Maps applications to fully reduced node.
    bindings    :: !(Map DagApp DagTerm)
    -- | Next index for application variables.
  , nextAppIndex :: !AppIndex
  }

getBinding :: IORef NodeDagEngineState -> DagApp -> IO DagTerm
getBinding bindingsRef a = do
  s <- readIORef bindingsRef
  case Map.lookup a (bindings s) of
    Just v -> return v
    Nothing -> do
      let ni = nextAppIndex s
      let v = AppVar ni a
      seq v $ do
        writeIORef bindingsRef $!
          NodeDagEngineState {
              bindings = Map.insert a v (bindings s)
            , nextAppIndex = ni + 1
            }
        return v

mkExactDagEngine :: IO DagEngine
mkExactDagEngine = do
  bindingsRef <- newIORef 
    NodeDagEngineState {
        bindings     = Map.empty
      , nextAppIndex = 0
      }
  inputTermsRef <- newIORef V.empty
  let deApplyUnary op x =
        assert (opArgTypes op == V.singleton (termType x)) $
          getBinding bindingsRef (UnaryApp op x)
  let deApplyBinary op x y =
        assert (opArgTypes op == V.map termType (V.fromListN 2 [x,y])) $
          getBinding bindingsRef (BinaryApp op x y)
  let deApplyTernary op x y z =
        assert (opArgTypes op == V.map termType (V.fromListN 3 [x, y, z])) $
          getBinding bindingsRef (TernaryApp op x y z)
  return DagEngine {
      deApplyUnary
    , deApplyBinary
    , deApplyTernary
    , deApplyOp = \op args ->
        case V.length args of
          1 -> deApplyUnary op (args V.! 0)
          2 -> deApplyBinary op (args V.! 0) (args V.! 1)
          3 -> deApplyTernary op (args V.! 0) (args V.! 1) (args V.! 2)
          _ -> assert (opArgTypes op == V.map termType args) $
                 getBinding bindingsRef (App op args)
    , deFreshInput = \tp -> do
        v <- readIORef inputTermsRef
        let r = InputTerm (V.length v) tp
        seq r $ do
          writeIORef inputTermsRef $! V.snoc v r
          return r
    , deInputTerms = readIORef inputTermsRef
    , deGetTerms = (Map.elems . bindings) <$> readIORef bindingsRef
    }

-- | Dag engine that tries to simplify ground terms rather than generating
-- new application.
mkConstantFoldingDagEngine :: IO DagEngine
mkConstantFoldingDagEngine = do
  bindingsRef <- newIORef $!
    NodeDagEngineState {
        bindings     = Map.empty
      , nextAppIndex = 0
      }
  inputTermsRef <- newIORef V.empty
  let mkConst op m = do v <- m
                        return (ConstTerm v (opResultType op))
  let deApplyUnary op x =
        assert (opArgTypes op == V.singleton (termType x)) $
          case (opEval op, x) of
            (UnaryOpEval fn, ConstTerm cx _) ->
              mkConst op $ fn (opSubst op) (return cx)

            (VectorOpEval fn, ConstTerm cx _) ->
              mkConst op $ fn (opSubst op)
                         $ V.singleton (return cx)

            _ -> getBinding bindingsRef (UnaryApp op x)

  let deApplyBinary op x y =
        assert (opArgTypes op == V.map termType (V.fromListN 2 [x,y])) $
          case (opEval op, x, y) of
            (BinaryOpEval fn, ConstTerm cx _, ConstTerm cy _) ->
              mkConst op $ fn (opSubst op) (return cx) (return cy)

            (VectorOpEval fn, ConstTerm cx _, ConstTerm cy _) ->
              mkConst op $ fn (opSubst op)
                         $ V.fromListN 2 [return cx, return cy]

            _ -> getBinding bindingsRef (BinaryApp op x y)

  let deApplyTernary op x y z =
        assert (opArgTypes op == V.map termType (V.fromListN 3 [x, y, z])) $
          case (opEval op, x, y, z) of
            (TernaryOpEval fn, ConstTerm cx _, ConstTerm cy _, ConstTerm cz _) ->
              mkConst op $ fn (opSubst op) (return cx) (return cy) (return cz)

            (VectorOpEval fn,  ConstTerm cx _, ConstTerm cy _, ConstTerm cz _) ->
              mkConst op $ fn (opSubst op)
                         $ V.fromListN 3 [return cx, return cy, return cz]

            _ -> getBinding bindingsRef (TernaryApp op x y z)
  return DagEngine {
      deApplyUnary
    , deApplyBinary
    , deApplyTernary
    , deApplyOp = \op args ->
        case V.length args of
          1 -> deApplyUnary op (args V.! 0)
          2 -> deApplyBinary op (args V.! 0) (args V.! 1)
          3 -> deApplyTernary op (args V.! 0) (args V.! 1) (args V.! 2)
          _ -> assert (opArgTypes op == V.map termType args) $
                 case (opEval op, V.mapM termConst args) of
                   (VectorOpEval fn, Just cns) -> 
                     mkConst op $ fn (opSubst op) (V.map return cns)
                   _ -> getBinding bindingsRef (App op args)
    , deFreshInput = \tp -> do
        v <- readIORef inputTermsRef
        let r = InputTerm (V.length v) tp
        seq r $ do
          writeIORef inputTermsRef $! V.snoc v r
          return r
    , deInputTerms = readIORef inputTermsRef
    , deGetTerms = (Map.elems . bindings) <$> readIORef bindingsRef
    }

-- SymbolicExc {{{1

data SymbolicExc
  = SymExtErr
    { symExtErrMsg       :: String
    , symExtErrVerbosity :: Int
    }
  | SymExtTbd
  deriving (Show,Typeable)

instance Exception SymbolicExc

