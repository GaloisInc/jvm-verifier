{- |
Module           : $Header$
Description      : Provides a first-order Dag structure for representing symbolic terms,
                   along with operations for constructing new dags, pretty printing terms,
                   and other operations.
Stability        : stable
Point-of-contact : jhendrix, lerkok (for pretty printer)
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module Symbolic.Dag
  ( -- * Applications
    DagApp(..)
  , appOp
  , appArgCount
  , appArg
  , appArgs
  , appArgsList
  -- * Nodes
  , Node(..)
  , isWellFormedNode
  , nodeCounts
  -- ** Application nodes
  , AppIndex
  , termApp
  , termAppIndex
  , getApp
  -- ** Input
  , termInputLitResult
  -- * Monads
  , TermDagMonad(..)
  , TermDag
  , runTermDag
  , SymbolicExc(..)
  ) where

import Control.Applicative hiding (empty)
import qualified Control.Exception as CE
import Control.Monad
import Control.Monad.Error
import qualified Control.Monad.State.Lazy as MTLL
import qualified Control.Monad.State.Strict as MTLS
import Data.Map (Map)
import qualified Data.Map as  Map
import qualified Data.Map    as M
import qualified Data.Set    as S
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Text.PrettyPrint.HughesPJ

import IO.Session
import Symbolic.Lit
import Symbolic.Common
import Utils.IOStateT
import Utils.LogMonad

-- App {{{1

-- | A function application
data DagApp
   = UnaryApp   !Op !Node
   | BinaryApp  !Op !Node !Node
   | TernaryApp !Op !Node !Node !Node
   | App !Op !(Vector Node)
   deriving (Eq, Ord, Show)

-- | Returns operator used in application
appOp :: DagApp -> Op
appOp (UnaryApp op _) = op
appOp (BinaryApp op _ _) = op
appOp (TernaryApp op _ _ _) = op
appOp (App op _) = op

-- | Returns true if an App node has at least 4 arguments, or 0
--   used for sanity checking
isWellFormedNode :: Node -> Bool
isWellFormedNode (AppVar _ a@(App{})) = let nc = appArgCount a in nc == 0 || nc >= 4
isWellFormedNode _                    = True

-- | Returns number of arguments in application.
appArgCount :: DagApp -> Int
appArgCount UnaryApp{} = 1
appArgCount BinaryApp{} = 2
appArgCount TernaryApp{} = 3
appArgCount (App _ v) = V.length v

-- | Returns argument to application using 0-based index.
appArg :: DagApp -> Int -> Node
appArg (UnaryApp _ t1) 0 = t1
appArg (BinaryApp _ t1 _) 0 = t1
appArg (BinaryApp _ _ t2) 1 = t2
appArg (TernaryApp _ t1 _ _) 0 = t1
appArg (TernaryApp _ _ t2 _) 1 = t2
appArg (TernaryApp _ _ _ t3) 2 = t3
appArg (App _ v) i | 0 <= i && i < V.length v = v V.! i
appArg _ _ = error $ "internal: Ilegal app argument"

-- | Returns a vector of the application's arguments
appArgs :: DagApp -> Vector Node
appArgs (UnaryApp _ t)          = V.singleton t
appArgs (BinaryApp _ t1 t2)     = V.fromListN 2 [t1, t2]
appArgs (TernaryApp _ t1 t2 t3) = V.fromListN 3 [t1, t2, t3]
appArgs (App _ v)               = v

-- | Returns a list of the application's arguments
appArgsList :: DagApp -> [Node]
appArgsList (UnaryApp _ t)          = [t]
appArgsList (BinaryApp _ t1 t2)     = [t1, t2]
appArgsList (TernaryApp _ t1 t2 t3) = [t1, t2, t3]
appArgsList (App _ v)               = V.toList v

-- Node {{{1

type AppIndex = Word64

-- | A possible symbolic term which may be a fresh input variable, a function
-- application, a ground value, or an array of symbolic terms.
data Node
  = InputTerm InputIndex -- ^ The name of a named input, or the
                         -- index of a unnamed input.
              (Maybe (LitResult Lit))
              DagType
  | AppVar AppIndex DagApp
  | ConstTerm CValue DagType
  deriving (Typeable)

instance Eq Node where
  InputTerm i _ _ == InputTerm j _ _ = i == j
  AppVar i _ == AppVar j _ = i == j
  ConstTerm x _ == ConstTerm y _ = x == y
  _ == _ = False

instance Ord Node where
  compare (InputTerm i _ _) (InputTerm j _ _) = i `compare` j
  compare (InputTerm _ _ _) _ = LT
  compare _ (InputTerm _ _ _) = GT
  compare (AppVar i _) (AppVar j _) = i `compare` j
  compare (AppVar _ _) _ = LT
  compare _ (AppVar _ _) = GT
  compare (ConstTerm x _) (ConstTerm y _) = x `compare` y

instance Show Node where
  showsPrec d t = showParen (d > 10) $ showTerm t
    where showTerm (InputTerm i _ _) =
            showString "InputTerm " . showsPrec 11 i . showString " XXXX"
          showTerm (AppVar ti app)
            = showString "AppVar "
                . showsPrec 11 ti . showChar ' '
                . showsPrec 11 app
          showTerm (ConstTerm val _) =
            showString "ConstTerm " . showsPrec 11 val


instance TypedTerm Node where
  termType (InputTerm _ _ tp) = tp
  termType (AppVar _ app) = opResultType (appOp app)
  termType (ConstTerm _ tp) = tp

instance TermClass Node where
  getAppVector (AppVar _ app) = Just (appOp app, appArgs app)
  getAppVector _ = Nothing

  getTypedApp (AppVar _ app) =
    Just (opDef (appOp app), opSubst (appOp app), appArgs app)
  getTypedApp _ = Nothing

  termInputId (InputTerm i _ _) = Just i
  termInputId _ = Nothing


-- | Returns App from term if any.
getApp :: Node -> Maybe (Op, [Node])
getApp (AppVar _ app) = Just (appOp app, appArgsList app)
getApp _ = Nothing

{-# DEPRECATED termApp "Use getApp or getAppVector instead" #-}
-- | Returns app associated with term.
termApp :: Node -> Maybe DagApp
termApp (AppVar _ app) = Just app
termApp _ = Nothing

-- | Returns node app index if any.
termAppIndex :: Node -> Maybe AppIndex
termAppIndex (AppVar i _) = Just i
termAppIndex _ = Nothing

-- | Returns term input lit result if one is defined.
termInputLitResult :: Node -> Maybe (LitResult Lit)
termInputLitResult (InputTerm _ d _) = d
termInputLitResult _ = Nothing

instance ConstantInjection Node where
  mkCBool b = ConstTerm (CBool b) SymBool
  mkCInt w i = ConstTerm (mkCInt w i) (SymInt (constantWidth w))

instance ConstantProjection Node where
  termConst (ConstTerm v _) = Just v
  termConst _ = Nothing
  getBool = getBool <=< termConst
  getSVal = getSVal <=< termConst
  getUVal = getUVal <=< termConst

-- | Given a node returns a map that maps each subnode to it's size.
nodeCounts :: Node -> Map Node Int
nodeCounts node = MTLS.execState (visit node) Map.empty
  where visit :: Node -> MTLS.State (Map Node Int) ()
        visit n@(getAppVector -> Just (_op, args)) = do
          m <- get
          case Map.lookup n m of
            Nothing -> do
              put $ Map.insert n 1 m
              V.mapM_ visit args
            Just cnt -> do
              let newCnt = cnt + 1
              put $ newCnt `seq` Map.insert n newCnt m
        visit _ = return ()

-- Term pretty printing {{{1

instance PrettyTerm Node where
  prettyTermWithD = ppSymTermSExpWith

type Visited  = S.Set Word64
type NodeInfo = M.Map Word64 (Int, Int)  -- (depth, #of occs)

ppSymTermSExpWith :: PPConfig -> Node -> Doc
ppSymTermSExpWith cfg dag = doc
  where (_, doc) = ppN S.empty dag
        cutoffDepth    = ppMinSharingDepth cfg
        nodeInfos      = collectNodeInfo dag M.empty

        ppN :: Visited -> Node -> (Visited, Doc)
        ppN sofar n@(getApp -> Just (op, args))
         | maybe True (depth <=) cutoffDepth = (sofar,  line empty)
         | i `S.member` sofar                = (sofar,  nid)
         | cnt > 1                           = (sofar', line nref)
         | True                              = (sofar', line empty)
         where Just i    = termAppIndex n
               si        = show i
               nid       = text $ 'n' : si
               nref      = braces $ text $ si
               line r
                 | RecCtor fieldNames <- opDefKind (opDef op) =
                    let ppField nm d = parens (text nm <+> d)
                     in call (text "mkRec") r (zipWith ppField fieldNames argDs)
                 | otherwise =
                    call (text (opName op)) r argDs
               call o r as = parens $ fsep (o <> r : as)
               (depth, cnt) = nodeInfos M.! i
               (sofar', reverse -> argDs) = foldl nextArg (i `S.insert` sofar, []) args
               nextArg (s, ds) a = let (s', d) = ppN s a in (s', d : ds)
        ppN sofar (termInputId -> Just i) = (sofar, text $ '?' : show i)
        ppN sofar (termConst -> Just c)   = (sofar, ppCValueD Prefix c)
        ppN _     _                       = error "illegal term in Symbolic.PrettySExp.ppSymTermSExp.ppN"

-- | Collect the occurrence count and depth of nodes
collectNodeInfo :: Node -> NodeInfo -> NodeInfo
collectNodeInfo n@(getApp -> Just (_, args)) !ni
   = case M.lookup i ni of
       Just _  -> M.adjust bumpCnt i ni
       Nothing -> d `seq` M.insert i (d, 1) ni'
   where Just i = termAppIndex n
         bumpCnt (depth, count) = let count' = count+1 in count' `seq` (depth, count')
         ni' = foldr collectNodeInfo ni args
         d   = 1 + maximum (0 : map (depthOf ni') args)
collectNodeInfo _ ni = ni

-- | Compute the depth of a given node; note that this is a mere look-up from
-- the map, no computation is actually done
depthOf :: NodeInfo -> Node -> Int
depthOf ni n@(getApp -> Just _)
  | Just (d, _) <- i `M.lookup` ni = d
  | True                           = error $ "Cannot locate depth info for node: " ++ show i
  where Just i = termAppIndex n
depthOf _ _ = 0

-- TermDagMonad {{{1

-- | Operations on a term dag representation.
class MonadIO m => TermDagMonad m where
  -- | Create a DAG node from inputs without performing simplification.
  rawOpVector :: Op -> Vector (MonadTerm m) -> m (MonadTerm m)

  -- | Returns application nodes stored in this dag.
  -- Used for debugging purposes.
  getAppNodes :: m [MonadTerm m]

  -- | Create a fresh unnamed input term.
  freshInput :: Maybe (LitResult Lit) -> DagType -> m (MonadTerm m)

  -- | Return number of input nodes created so far.
  getInputCount :: m Int

-- State Transformers {{{2

instance (Monad m, TermDagMonad m) => TermDagMonad (StateT s m) where
  rawOpVector op args = lift $ rawOpVector op args
  getAppNodes = lift $ getAppNodes
  freshInput d tp = lift $ freshInput d tp
  getInputCount = lift getInputCount

instance (Monad m, TermDagMonad m) => TermDagMonad (MTLL.StateT s m) where
  rawOpVector op args = lift $ rawOpVector op args
  getAppNodes = lift $ getAppNodes
  freshInput d tp = lift $ freshInput d tp
  getInputCount = lift getInputCount

instance (Monad m, TermDagMonad m) => TermDagMonad (MTLS.StateT s m) where
  rawOpVector op args = lift $ rawOpVector op args
  getAppNodes = lift $ getAppNodes
  freshInput d tp = lift $ freshInput d tp
  getInputCount = lift getInputCount

-- TermDag {{{1

-- | State of the symbolic monad.
data TermDagState = TermDagState {
    -- | Maps applications to fully reduced node.
    bindings    :: !(Map DagApp Node)
    -- | Next index for input variables.
  , nextInput   :: !InputIndex
    -- | Next index for application variables.
  , nextAppIndex :: !AppIndex
    -- | Maps each term index to the associated lit vector if any.
  , aigerAppMap :: !(Map Node (LitResult Lit))
    -- | Verbosity of lit vector.
  , verbosity   :: !Int
  }

-- | Monadic interface to a term dag that is parameterized over a descriptor
-- type and underlying monad.
newtype TermDag a = TD (StateT TermDagState (AigComputation OpSession) a)
   deriving ( Functor
            , Monad
            , MonadIO
            , CatchMIO
            , MonadState TermDagState
            , SessionMonad)

type instance MonadTerm TermDag = Node

instance Applicative TermDag where
  pure = return
  af <*> aa = af >>= (<$> aa)

instance SessionState TermDagState where
  initialState = TermDagState {
      bindings    = Map.empty
    , nextInput   = 0
    , nextAppIndex = 0
    , aigerAppMap = Map.empty
    , verbosity   = 1
    }

-- | Run symbolic monad in IO.
runTermDag :: TermDag a -> OpSession a
runTermDag (TD m) = runAigComputation $ evalStateT m initialState

-- TermDag operations {{{2

-- | Return node formed from given application.
getBinding :: DagApp -> TermDag Node
getBinding a = do
  d <- get
  case Map.lookup a (bindings d) of
    Just v -> return v
    Nothing -> do
      let ni = nextAppIndex d
      let n = AppVar ni a
      put d { bindings = Map.insert a n (bindings d)
            , nextAppIndex = ni + 1 }
      return n

instance ApplyOpMonad TermDag where
  makeConstant c tp = return $ ConstTerm c tp

  applyUnaryOp op x = do
    let argTypes = V.singleton (termType x)
    unless (opArgTypes op == argTypes) $
      error $ "internal: op " ++ opName op ++
              " applied to argument with arguments type " ++ show argTypes ++
              " when " ++ show (opArgTypes op) ++ " expected.\n" ++ show x
    case (opEval op, x) of
      (UnaryOpEval fn, ConstTerm cx _) ->
        return $ ConstTerm (fn (opSubst op) cx) (opResultType op)
      (VectorOpEval fn, ConstTerm cx _) ->
         return $ ConstTerm (fn (opSubst op) (V.singleton cx)) (opResultType op)
      _ -> getBinding (UnaryApp op x)

  applyBinaryOp op x y = do
    let argTypes = V.map termType (V.fromListN 2 [x,y])
    unless (opArgTypes op == argTypes) $
      error $ "internal: op " ++ opName op ++ " applied to arguments "
              ++ show argTypes ++ ".\n"
              ++ show x ++ "\n"
              ++ show y
    case (opEval op, x, y) of
     (BinaryOpEval fn, ConstTerm cx _, ConstTerm cy _) ->
       return $ ConstTerm (fn (opSubst op) cx cy) (opResultType op)
     (VectorOpEval fn, ConstTerm cx _, ConstTerm cy _) ->
       return $ ConstTerm (fn (opSubst op) (V.fromListN 2 [cx, cy])) (opResultType op)
     _ -> getBinding (BinaryApp op x y)

  applyTernaryOp op x y z = do
    let argTypes = V.map termType $ V.fromListN 3 [x, y, z]
    unless (opArgTypes op == argTypes) $
      error $ "internal: applyTernaryOp " ++ opName op ++
              " applied to arguments of type " ++ show argTypes ++
              " instead of type " ++ show (opArgTypes op)
    case (opEval op, x, y, z) of
      (TernaryOpEval fn, ConstTerm cx _, ConstTerm cy _, ConstTerm cz _) ->
         return $ ConstTerm (fn (opSubst op) cx cy cz) (opResultType op)
      (VectorOpEval fn,  ConstTerm cx _, ConstTerm cy _, ConstTerm cz _) ->
         return $ ConstTerm (fn (opSubst op) (V.fromListN 3 [cx, cy, cz])) (opResultType op)
      _ -> getBinding (TernaryApp op x y z)

  applyOpVector op args =
    case V.length args of
      1 -> applyUnaryOp op (args V.! 0)
      2 -> applyBinaryOp op (args V.! 0) (args V.! 1)
      3 -> applyTernaryOp op (args V.! 0) (args V.! 1) (args V.! 2)
      _ -> CE.assert (opArgTypes op == V.map termType args) $
             case (opEval op, V.mapM termConst args) of
               (VectorOpEval fn, Just cns) -> return $ ConstTerm (fn (opSubst op) cns) (opResultType op)
               _ -> getBinding (App op args)

instance BitBlastMonad TermDag where
  getAppLitResult n = TD $ do
    return . Map.lookup n =<< gets aigerAppMap
  setAppLitResult n lit = TD $ modify $ \s ->
    s { aigerAppMap = Map.insert n lit (aigerAppMap s) }
  constLitResult (ConstTerm c _) = liftAigMonad $ concreteValueLit c
  constLitResult (termInputLitResult -> Just d) = return d
  constLitResult _ = do
    v <- getVerbosity
    liftIO $ CE.throwIO
           $ SymExtErr "Cannot bitblast uninterpreted input variable" v
  liftAigMonad m = TD $ lift m

instance TermDagMonad TermDag where
  rawOpVector op args =
    CE.assert (opArgTypes op == V.map termType args) $
     case V.length args of
        1 -> getBinding $ UnaryApp op (args V.! 0)
        2 -> getBinding $ BinaryApp op (args V.! 0) (args V.! 1)
        3 -> getBinding $ TernaryApp op (args V.! 0) (args V.! 1) (args V.! 2)
        _ -> getBinding $ App op args

  getAppNodes = fmap Map.elems $ gets bindings

  freshInput d tp = do
    dag <- get
    let n = nextInput dag
    put dag { nextInput = n + 1 }
    return $ InputTerm n d tp

  getInputCount = gets nextInput

instance LogMonad TermDag where
  getVerbosity = gets verbosity
  setVerbosity v = modify $ \s -> s{ verbosity = v}

-- SymbolicExc {{{1
data SymbolicExc
  = SymExtErr
    { symExtErrMsg       :: String
    , symExtErrVerbosity :: Int
    }
  | SymExtTbd
  deriving (Show,Typeable)

instance CE.Exception SymbolicExc

-- This is the path for error messages that originate from use of 'fail'; since
-- we should be handling these explicitly, we die here.
instance Error SymbolicExc where
  strMsg msg = error $ "Unhandled exception raised: (" ++ msg ++ ")"
