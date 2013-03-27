{- |
Module           : $Header$
Description      : Shared types and utility functions for JSS
Stability        : stable
Point-of-contact : acfoltzer
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Verifier.Java.Common
  ( -- * Core types
    -- ** Simulator
    Simulator(..)
  , MonadSim
    -- ** Simulator state
  , State
  , initialState
    -- *** Lenses
  , codebase
  , instanceOverrides
  , staticOverrides
  , ctrlStk
  , nextPSS
  , strings
  , nextRef
  , verbosity
  , simulationFlags
  , backend
  , errorPaths
  , printErrPaths
  , evHandlers

    -- ** Simulator configuration
  , SimulationFlags(..)
  , defaultSimFlags
  , InstanceOverride
  , StaticOverride

    -- ** Simulator control stack and continuations
  , CS(..)
  , initialCtrlStk
  , SimCont(..)
    -- *** Utilities
  , isFinished
  , modifyCS
  , modifyCSM
  , modifyCSM_

    -- ** Symbolic execution paths
  , Path
  , Path'
    -- *** Lenses
  , pathStack
  , pathStackHt
  , pathBlockId
  , pathRetVal
  , pathException
  , pathMemory
  , pathAssertions
  , pathName
  , pathStartingPC
  , pathBreakpoints
  , pathInsnCount
    -- *** Utilities
  , getPath
  , getPathMaybe
  , currentPath
  , modifyPath
  , modifyCurrentPath
  , modifyCurrentPathM
  , modifyPathM
  , modifyPathM_
  , addPathAssertion

    -- ** Java call frames
  , CallFrame
    -- *** Lenses
  , cfReturnBlock
  , cfClass
  , cfMethod
  , cfLocals
  , cfOpds
    -- *** Utilities
  , currentCallFrame
  , modifyCallFrameM
  , modifyCallFrameM_
  , getCurrentClassName
  , getCurrentMethod

    -- ** Value and memory representation
  , Value
  , Value'
  , Ref(..)
  , Memory
  , InitializationStatus(..)
    -- *** Lenses
  , memInitialization
  , memStaticFields
  , memInstanceFields
  , memScalarArrays
  , memRefArrays
  , memClassObjects
    -- *** Utilities
  , getMem
  , setInitializationStatus

    -- ** Exceptions and errors
  , FailRsn(FailRsn)
  , JavaException(..)
  , excRef
  , excStack
  , ErrorPath(EP)
  , epRsn
  , epPath
  , InternalExc(ErrorPathExc, UnknownExc)
  , SEH(..)

    -- ** Pretty-printers
  , ppPath
  , ppMethod
  , ppValue
  , ppCurrentPath
  , ppState
  , ppMemory
  , ppFailRsn
  , ppJavaException
  , ppInternalExc

    -- * Control flow primitives
  , pushCallFrame
  , addCtrlBranch
  , jumpCurrentPath
  , returnCurrentPath
  , markCurrentPathAsError

    -- * Miscellaneous & debugging
  , lookupClass
  , dumpCtrlStk
  , dumpMemory
  , dumpCurrentPath
  , assert
  ) where

import Prelude hiding (EQ, GT, LT)
import qualified Prelude as P

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Arrow ((***))
import Control.Lens
import Control.Monad.Error 
import Control.Monad.State hiding (State)

import Data.Array (Array, Ix, assocs)
import qualified Data.Foldable as DF
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word (Word32)

import Text.PrettyPrint

import Language.JVM.Common (ppFldId, ppType)
import Data.JVM.Symbolic.AST
import Execution.JavaSemantics (AtomicValue(..), JSValue)

import Verifier.Java.Backend
import Verifier.Java.Codebase hiding (lookupClass)
import qualified Verifier.Java.Codebase as Codebase

-- | A Simulator is a monad transformer around a symbolic backend
newtype Simulator sbe (m :: * -> *) a = 
  SM { runSM :: ErrorT (InternalExc sbe m) (StateT (State sbe m) IO) a }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState (State sbe m)
    , MonadError (InternalExc sbe m)
    )

-- | These constraints are common to monads with a symbolic execution
-- backend. Enable @{-# LANGUAGE ConstraintKinds #-}@ to use this in
-- client code.
type MonadSim sbe m = (AigOps sbe, Functor m, MonadIO m)

-- | Overrides for instance methods
type InstanceOverride sbe m = Ref -> [Value' sbe m] -> Simulator sbe m ()
-- | Overrides for static methods
type StaticOverride sbe m = [Value' sbe m] -> Simulator sbe m ()

data State sbe m = State {
    _codebase          :: !Codebase
  , _instanceOverrides :: !(Map (String, MethodKey) (InstanceOverride sbe m))
    -- ^ Maps instance method identifiers to a function for executing them.
  , _staticOverrides   :: !(Map (String, MethodKey) (StaticOverride sbe m))
    -- ^ Maps static method identifiers to a function for executing them.
  , _ctrlStk           :: CS sbe
  , _nextPSS           :: PathDescriptor
    -- ^ Name supply for unique path state selectors
  , _strings           :: !(Map String Ref)
  , _nextRef           :: !Word32 -- ^ Next index for constant ref.
  , _verbosity         :: Int
  , _simulationFlags   :: SimulationFlags
  , _backend           :: Backend sbe
  , _errorPaths        :: [ErrorPath sbe]
  , _printErrPaths     :: Bool
  , _evHandlers        :: SEH sbe m
  }

-- | Set up a new State with the given arguments and default values
-- for the rest of the fields.
initialState :: Codebase
             -> Backend sbe
             -> SimulationFlags
             -> SEH sbe m
             -> IO (State sbe m)
initialState cb sbe flags seh = do
  cs <- initialCtrlStk sbe
  return $ State { _codebase          = cb
                 , _instanceOverrides = M.empty
                 , _staticOverrides   = M.empty
                 , _ctrlStk           = cs
                 , _nextPSS           = 0
                 , _strings           = M.empty
                 , _nextRef           = 0
                 , _verbosity         = 6
                 , _simulationFlags   = flags
                 , _backend           = sbe
                 , _errorPaths        = []
                 , _printErrPaths     = False
                 , _evHandlers        = seh
                 }

type Value term   = AtomicValue Double Float term term Ref
type Value' sbe m = JSValue (Simulator sbe m)

-- Address in heap
data Ref
  = NullRef
  | Ref !Word32 !Type
  deriving (Show)

-- | A point in the control flow of a program where a branched
-- computation merges
data MergePoint
  -- | Merge at any @return@ statement at the given stack height
  = ReturnPoint Int
  -- | Merge at the given postdominator node at the given stack height.
  | PostdomPoint Int BlockId

-- | Actions to take when a path reaches the merge point of a branched
-- computation. This type is essentially the environment of the
-- 'SimCont' "closure".
data BranchAction sbe
  -- | @BranchRunTrue cond p@ is the continuation after finishing a
  -- path for the @else@ of a branch with condition @cond@. When
  -- applied, it starts running the @then@ branch with the suspended
  -- path @p@.
  = BranchRunTrue (SBETerm sbe) (Path sbe)   
  -- | @BranchMerge a cond p@ is the continuation after finishing a
  -- path for the @then@ of a branch with condition cond. When
  -- applied, the finished path @p@ from the @else@ branch is merged
  -- with the current path and the pre-branch assertions @a@.
  | BranchMerge (SBETerm sbe) (SBETerm sbe) (Path sbe)

-- | First-order continuations for the symbolic simulation.
data SimCont sbe
  -- | Empty continuation: there are no remaining paths to run
  = EmptyCont
  -- | @HandleBranch mp ba k@ is a handler for part of a branch. The
  -- 'BranchAction' @ba@ will be applied when the current path reaches
  -- the 'MergePoint' @mp@. The 'SimCont' @k@ represents the simulator
  -- continuation after all paths of the branch are merged.
  | HandleBranch MergePoint (BranchAction sbe) (SimCont sbe)

-- | A control stack 'CS' is a stack of first-order continuations. It
-- represents either a computation with no work remaining, or a pair
-- of the current path and its continuation.
data CS sbe 
  -- | A completed computation, potentially with a successful result path
  = CompletedCS (Maybe (Path sbe))
  -- | @ActiveCS p k@ is an active computation on path @p@ with the pending continuation @k@
  | ActiveCS (Path sbe) (SimCont sbe)

initialCtrlStk :: Backend sbe -> IO (CS sbe)
initialCtrlStk sbe = do
  true <- termBool sbe True
  let p = Path { _pathStack       = []
               , _pathStackHt     = 0
               , _pathBlockId     = Just entryBlock
               , _pathRetVal      = Nothing
               , _pathException   = Nothing
               , _pathMemory      = emptyMemory
               , _pathAssertions  = true
               , _pathName        = 0
               , _pathStartingPC  = 0
               , _pathBreakpoints = S.empty
               , _pathInsnCount   = 0
               }
  return $ CompletedCS (Just p)

type PathDescriptor = Integer

data SimulationFlags =
  SimulationFlags { alwaysBitBlastBranchTerms :: Bool }
  deriving Show

defaultSimFlags :: SimulationFlags
defaultSimFlags = SimulationFlags { alwaysBitBlastBranchTerms = False }

type Path sbe = Path' (SBETerm sbe)

data Path' term = Path {
    _pathStack          :: ![CallFrame term]
    -- ^ the current JVM call stack
  , _pathStackHt        :: !Int
    -- ^ the current call frames count
  , _pathBlockId        :: !(Maybe BlockId)
    -- ^ the currently-executing basic block on this path, if any
  , _pathRetVal         :: !(Maybe (Value term))
    -- ^ the current return value, if this path has returned its last call frame
  , _pathException      :: !(Maybe (JavaException term))
    -- ^ the exception thrown on this path, if any  
  , _pathMemory         :: !(Memory term)
  , _pathAssertions     :: !term
    -- ^ facts assumed to be true on this path
  , _pathName           :: !PathDescriptor
    -- ^ a unique name for this path
  , _pathStartingPC     :: !PC
    -- ^ the program counter where this path began (TODO: this might not make sense with new branching)
  , _pathBreakpoints    :: !(Set (String, MethodKey, PC))
  -- ^ Breakpoint locations. REVISIT: might want to have a map in
  -- state from (String, MethodKey) to Map PC (..., ...), and then
  -- this map could just be Map PC (..., ...), and would get set every
  -- time we modify the frame list.
  , _pathInsnCount      :: !Int
  -- ^ The number of instructions executed so far on this path.
  }

data Memory term = Memory {
    _memInitialization :: !(Map String InitializationStatus)
    -- ^ the initialization status of classes
  , _memStaticFields   :: !(Map FieldId (Value term))
    -- ^ static field values 
  , _memInstanceFields :: !(Map InstanceFieldRef (Value term))
    -- ^ instance field values 
  , _memScalarArrays   :: !(Map Ref (Int32, term))
    -- ^ integer and long array values (floating point not supported)
  , _memRefArrays      :: !(Map Ref (Array Int32 Ref))
    -- ^ reference array values
  , _memClassObjects   :: !(Map String Ref)
    -- ^ java.lang.Class objects
  }

emptyMemory :: Memory term
emptyMemory = Memory M.empty M.empty M.empty M.empty M.empty M.empty

-- | A JVM call frame
data CallFrame term
  = CallFrame {
      _cfClass       :: !String                               
      -- ^ Name of the class containing the current method
    , _cfMethod      :: !Method
      -- ^ The current method
    , _cfReturnBlock :: !BlockId
      -- ^ The basic block to return to
    , _cfLocals      :: !(Map LocalVariableIndex (Value term)) 
      -- ^ The current local variables (<http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-2.html#jvms-2.6.1>)
    , _cfOpds        :: ![Value term]                          
      -- ^ The current operand stack
    }
    deriving (Eq)

data InitializationStatus
  = Started
  | Initialized
  | Erroneous
  deriving (Eq, Ord, Show)

type InstanceFieldRef = (Ref, FieldId)

data FailRsn       = FailRsn String deriving (Show)
data ErrorPath sbe = EP { _epRsn :: FailRsn, _epPath :: Path sbe }

-- | The exception type for errors that are both thrown and caught within the
-- simulator.
data InternalExc sbe m
  = ErrorPathExc FailRsn (State sbe m)
  | UnknownExc (Maybe FailRsn)

instance Error (InternalExc sbe m) where
  noMsg  = UnknownExc Nothing
  strMsg = UnknownExc . Just . FailRsn

-- | Simulation event handlers, useful for debugging nontrivial codes.
data SEH sbe m = SEH
  {
    -- | Invoked after function overrides have been registered
    onPostOverrideReg :: Simulator sbe m ()
    -- | Invoked before each instruction executes
  , onPreStep         :: SymInsn -> Simulator sbe m ()
    -- | Invoked after each instruction executes
  , onPostStep        :: SymInsn -> Simulator sbe m ()
  }

assert :: Bool -> Simulator sbe m ()
assert b = unless b . throwError $ strMsg "assertion failed"

data JavaException term =
  JavaException
    { _excRef   :: Ref          -- ^ the java.lang.Exception instance
    , _excStack :: [CallFrame term] -- ^ stack trace @ raise point
    }

instance Eq (JavaException term) where
  e1@JavaException{} == e2@JavaException{} = _excRef e1 == _excRef e2


instance Eq Ref where
  NullRef == NullRef = True
  (Ref x _) == (Ref y _) = x == y
  _ == _ = False

instance Ord Ref where
  NullRef `compare` NullRef = P.EQ
  NullRef `compare` _ = P.LT
  _ `compare` NullRef = P.GT
  (Ref x _) `compare` (Ref y _) = x `compare` y

-- SEE BOTTOM OF FILE FOR LENS IMPLEMENTATIONS
-- makeLenses ''State
-- makeLenses ''Path'
-- makeLenses ''Memory
-- makeLenses ''CallFrame
-- makeLenses ''ErrorPath
-- makeLenses ''JavaException

-- | Manipulate the control stack
modifyCS :: (CS sbe -> CS sbe) -> (State sbe m -> State sbe m)
modifyCS = over ctrlStk

-- | Return true if all paths in control stack have no more work.
isFinished :: CS sbe -> Bool
isFinished CompletedCS{} = True
isFinished _ = False

-- | For consistency with LSS api... probably not needed
modifyPath :: (Path sbe -> Path sbe) -> CS sbe -> Maybe (CS sbe)
modifyPath f cs = 
  case cs of
    CompletedCS mp -> (CompletedCS . Just . f) <$> mp
    ActiveCS p k -> Just (ActiveCS (f p) k)

-- | Apply @f@ to the current path, if one exists
modifyCurrentPath :: (Path sbe -> Path sbe) -> CS sbe -> CS sbe
modifyCurrentPath f (CompletedCS mp) = CompletedCS (f <$> mp)
modifyCurrentPath f (ActiveCS p k)   = ActiveCS (f p) k

-- | Modify current path in control stack, returning an extra value
modifyCurrentPathM :: forall m sbe a .
                      Functor m
                   => CS sbe
                   -> (Path sbe -> m (a,Path sbe))
                   -> Maybe (m (a,CS sbe))
modifyCurrentPathM cs f =
  case cs of
    CompletedCS mp -> (run (CompletedCS . Just)) <$> mp
    ActiveCS p h -> Just (run fn p)
      where fn p' = ActiveCS p' h
 where run :: (Path sbe -> CS sbe) -> Path sbe -> m (a, CS sbe) 
       run csfn = fmap (id *** csfn) . f

-- | Return the topmost path from a control stack, if any
currentPath :: CS sbe -> Maybe (Path sbe)
currentPath (CompletedCS mp) = mp
currentPath (ActiveCS p _) = Just p

-- | Modify the current control stack with the given function, which
-- may also return a result.
modifyCSM :: (CS sbe -> Simulator sbe m (a, (CS sbe)))
          -> Simulator sbe m a
modifyCSM f = do
  cs <- use ctrlStk
  (x, cs') <- f cs
  ctrlStk .= cs'
  return x

-- | Modify the current control stack with the given function
modifyCSM_ :: (CS sbe -> Simulator sbe m (CS sbe)) -> Simulator sbe m ()
modifyCSM_ f = modifyCSM (\cs -> ((),) <$> f cs)

-- | Modify the current path with the given function, which may
-- also return a result. Fails if there is no current path.
modifyPathM :: Doc
            -> (Path sbe -> Simulator sbe m (a, (Path sbe)))
            -> Simulator sbe m a
modifyPathM ctx f = 
  modifyCSM $ \cs ->
      case cs of 
        CompletedCS Nothing -> fail . render $ ctx <> ": no current paths"
        CompletedCS (Just p) -> do
                (x, p') <- f p
                return $ (x, CompletedCS (Just p'))
        ActiveCS p k -> do
                (x, p') <- f p
                return $ (x, ActiveCS p' k)

-- | Modify the current path with the given function. Fails if there
-- is no current path.
modifyPathM_ :: Doc 
             -> (Path sbe -> Simulator sbe m (Path sbe)) 
             -> Simulator sbe m ()
modifyPathM_ ctx f = modifyPathM ctx (\p -> ((),) <$> f p)

-- | Obtain the current path. Fails if there is no current path.
getPath :: (Functor m, Monad m) => Doc -> Simulator sbe m (Path sbe)
getPath ctx = do
  mp <- getPathMaybe
  case mp of
    Just p -> return p
    _      -> fail . render $ ctx <> ":" <+> "no current path"

-- | Obtain the current path, if present.
getPathMaybe :: (Functor m, Monad m) => Simulator sbe m (Maybe (Path sbe))
getPathMaybe = uses ctrlStk currentPath
  

-- | Get the memory model of the current path. If there is no current
-- path, this fails.
getMem :: (Functor m, Monad m) => Doc -> Simulator sbe m (Memory (SBETerm sbe))
getMem ctx = do
  p <- getPath ctx
  return $ p^.pathMemory

-- | Modify the current call frame with the given function, which may
-- also return a result. Fails if there is no current call frame.
modifyCallFrameM :: 
     Doc
  -> (CallFrame (SBETerm sbe) -> Simulator sbe m (a, (CallFrame (SBETerm sbe))))
  -> Simulator sbe m a
modifyCallFrameM ctx f = 
  modifyPathM ctx $ \p ->
    case p^.pathStack of
      [] -> fail . render $ ctx <> ": no stack frames"
      (cf:cfs) -> do
        (x, cf') <- f cf
        return (x, p & pathStack .~ (cf':cfs))

-- | Modify the current call frame with the given function. Fails if
-- there is no current call frame.
modifyCallFrameM_ :: 
     Doc
  -> (CallFrame (SBETerm sbe) -> Simulator sbe m (CallFrame (SBETerm sbe)))
  -> Simulator sbe m ()
modifyCallFrameM_ ctx f = modifyCallFrameM ctx (\cf -> ((),) <$> f cf)

-- | Return the enclosing class of the method being executed by the
-- current path
getCurrentClassName :: Simulator sbe m String
getCurrentClassName = 
  modifyCallFrameM "getCurrentClassName" $ \cf -> return (cf^.cfClass, cf)

-- | Return the method being executed by the current path
getCurrentMethod :: Simulator sbe m Method
getCurrentMethod = 
  modifyCallFrameM "getCurrentClassName" $ \cf -> return (cf^.cfMethod, cf)

-- | Resolve the given class in the simulator's current codebase
lookupClass :: String -> Simulator sbe m Class
lookupClass cName = do
  cb <- use codebase
  liftIO $ Codebase.lookupClass cb cName


-- | Push a new call frame to the current path, if any. Needs the
-- initial function arguments, and basic block (in the caller's
-- context) to return to once this method is finished.
pushCallFrame :: String
              -- ^ Class name   
              -> Method
              -- ^ Method
              -> BlockId
              -- ^ Basic block to return to
              -> Map LocalVariableIndex (Value (SBETerm sbe))
              -- ^ Initial locals
              -> CS sbe
              -- ^ Current control stack
              -> Maybe (CS sbe)
pushCallFrame clname method retBB locals cs = 
    case cs of
      CompletedCS Nothing -> fail "all paths failed"
      CompletedCS (Just p) -> Just $ ActiveCS (pushFrame p) EmptyCont
      _ -> return $ modifyCurrentPath pushFrame cs
  where pushFrame p = p'
          where cf = CallFrame clname method retBB locals []
                p' = p & pathStack   %~ (cf :)
                       & pathStackHt +~ 1
                       & pathBlockId .~ Just entryBlock

-- | Push a new continuation onto the control stack for a branching
-- computation. A new path is created and suspended for the @then@
-- branch.
addCtrlBranch :: SBETerm sbe   -- ^ Condition to branch on.
              -> BlockId       -- ^ Location for newly-branched paused path to start at.
              -> Integer       -- ^ Name of new path
              -> MergeLocation -- ^ Control point to merge at.
              -> CS sbe        -- ^ Current control stack.
              -> Maybe (CS sbe) 
addCtrlBranch c nb nm ml cs =
    case cs of
      CompletedCS{} -> fail "Path is completed"
      ActiveCS p k -> return . ActiveCS p $ HandleBranch point (BranchRunTrue c pt) k
        where point = case ml of
                       Just b -> PostdomPoint (p^.pathStackHt) b
                       Nothing -> ReturnPoint (p^.pathStackHt - 1)
              pt = p & pathBlockId .~ Just nb
                     & pathName    .~ nm

-- | Move current path to target block, checking for merge points.
jumpCurrentPath :: (Functor m, MonadIO m)
                => BlockId -> CS sbe -> Simulator sbe m (CS sbe)
jumpCurrentPath _ CompletedCS{} = fail "Path is completed"
jumpCurrentPath b (ActiveCS p k) = 
  mergeNextCont (p & pathBlockId .~ Just b) k

-- | Return from current path, checking for merge points, and putting
-- the optional return value on the operand stack of the next frame.
returnCurrentPath :: forall sbe m . (Functor m, MonadIO m)
                  => Maybe (Value (SBETerm sbe))
                  -> CS sbe 
                  -> Simulator sbe m (CS sbe)
returnCurrentPath _ CompletedCS{} = fail "Path is completed"
returnCurrentPath retVal (ActiveCS p k) = do
  let cf : cfs = p^.pathStack
      cfs' = case retVal of
               Nothing -> cfs
               Just rv -> addRv rv cfs
      addRv _  []          = []
      addRv rv (cf':cfs'') = (cf' & cfOpds %~ (rv:)):cfs''
      p' :: Path sbe
      p' = p & pathStack   .~ cfs'
             & pathStackHt -~ 1
             & pathBlockId .~ Just (cf^.cfReturnBlock)
             & pathRetVal  .~ if null cfs' then retVal else Nothing
  mergeNextCont p' k

branchError :: (Functor m, MonadIO m)
            => BranchAction sbe -- ^ action to run if branch occurs.
            -> SimCont sbe      -- ^ previous continuation
            -> Simulator sbe m (CS sbe) 
branchError ba k = do
  sbe <- use backend
  case ba of
    BranchRunTrue c pt -> do
      pt' <- pt & pathAssertions %%~ (liftIO . termAnd sbe c)
      return $ ActiveCS pt' k
    BranchMerge a c pf -> do
      -- Update assertions on current path
      a1   <- liftIO $ termAnd sbe a (pf^.pathAssertions)
      cNot <- liftIO $ termNot sbe c
      a2   <- liftIO $ termAnd sbe a1 cNot
      let pf' = pf & pathAssertions .~ a2 
      -- Try to merge states that may have been waiting for the current path to terminate.
      mergeNextCont pf' k

-- | Mark the current path as an error path.
markCurrentPathAsError :: (Functor m, MonadIO m)
                       => CS sbe 
                       -> Simulator sbe m (CS sbe)
markCurrentPathAsError cs = case cs of
  CompletedCS{}                   -> fail "path is completed"
  ActiveCS _ (HandleBranch _ a k) -> branchError a k
  ActiveCS _ EmptyCont            -> return $ CompletedCS Nothing

addPathAssertion :: (MonadIO m, Functor m)
                 => Backend sbe 
                 -> SBETerm sbe 
                 -> Path sbe 
                 -> m (Path sbe)
addPathAssertion sbe t p = 
  p & pathAssertions %%~ \a -> liftIO (termAnd sbe a t)

-- | Get top call frame from a path
currentCallFrame :: Path' term -> Maybe (CallFrame term)
currentCallFrame p = p^.pathStack^?_head

-- | Called at symbolic return instructions and jumps to new basic
-- blocks to check whether it is time to merge a path and move on to
-- the next continuation. There are three cases:
-- 
--   1. There are no more call frames on the stack, and the
--   continuation is empty. This leaves us with a completed control
--   stack containing the current path.
-- 
--   2. We've reached the current path's 'MergePoint', so the current
--   continuation is complete. Depending on the type of continuation,
--   we either move on to a different path, or merge the current path
--   with an already-finished path before continuing.
-- 
--   3. The current path's merge point does not indicate the current
--   location, so we continue with the same path and continuation.
mergeNextCont :: (Functor m, MonadIO m)
              => Path sbe
              -> SimCont sbe
              -> Simulator sbe m (CS sbe)
-- 1.
mergeNextCont p EmptyCont | 0 == p^.pathStackHt =
  return (CompletedCS (Just p))
-- 2.
mergeNextCont p (HandleBranch point ba h) 
  | p `atMergePoint` point = do
  sbe <- use backend
  case ba of
    BranchRunTrue c tp -> do
      true <- liftIO $ termBool sbe True
      let tp' = tp & pathAssertions .~ true
      let ba' = BranchMerge (tp^.pathAssertions) c p
      return $ ActiveCS tp' (HandleBranch point ba' h) 
    BranchMerge a c pf -> do
      mergedCallStack <- 
        case (p^.pathStack, pf^.pathStack) of
          ([]      , []   ) -> return []
          -- TODO is it right to ignore the other frames on one path?
          -- That's what old sim did...
          (cf1:cfs1, cf2:_) -> do 
            cf' <- mergeCallFrames c cf1 cf2
            return (cf':cfs1)
          _ -> throwError $ strMsg "call frame mismatch when merging paths"
      mergedMemory <- mergeMemories c (p^.pathMemory) (pf^.pathMemory)
      mergedAssertions <- 
          liftIO $ termIte sbe c (p^.pathAssertions) (pf^.pathAssertions)
      mergedRetVal <- mergeRetVals c (p^.pathRetVal) (pf^.pathRetVal)
      a' <- liftIO $ termAnd sbe a mergedAssertions
      let p' = p & pathStack      .~ mergedCallStack
                 & pathMemory     .~ mergedMemory
                 & pathAssertions .~ a'
                 & pathRetVal     .~ mergedRetVal
      -- recur in case multiple continuations have the same merge point
      mergeNextCont p' h
-- 3.
mergeNextCont p h = return (ActiveCS p h)

-- | Is the given path at its 'MergePoint'?
atMergePoint :: Path' term -> MergePoint -> Bool
p `atMergePoint` point = case point of
  ReturnPoint n -> n == p^.pathStackHt
  PostdomPoint n b -> 
    n == p^.pathStackHt && Just b == p^.pathBlockId

mergeRetVals :: MonadIO m 
             => SBETerm sbe
             -> Maybe (Value (SBETerm sbe))
             -> Maybe (Value (SBETerm sbe))
             -> Simulator sbe m (Maybe (Value (SBETerm sbe)))
mergeRetVals c (Just rv1) (Just rv2) = Just <$> mergeValues c rv1 rv2
mergeRetVals _ Nothing    Nothing    = return Nothing
mergeRetVals _ _          _          = 
    throwError $ strMsg "return value mismatch when merging paths"

mergeMemories :: MonadIO m
              => SBETerm sbe
              -> Memory (SBETerm sbe)
              -> Memory (SBETerm sbe)
              -> Simulator sbe m (Memory (SBETerm sbe))
mergeMemories assertions mem1 mem2 = do
  sbe <- use backend
  let Memory init1 sFields1 iFields1 scArrays1 rArrays1 cObjs1 = mem1
      Memory init2 sFields2 iFields2 scArrays2 rArrays2 cObjs2 = mem2
      mergedInit    = M.unionWith max init1 init2
      -- no symbolic references, so these are just unioned
      mergedRArrays = M.union rArrays1 rArrays2
      mergedCObjs   = M.union cObjs1 cObjs2
      -- pointwise merging of tuples
      mergeTup (l1, v1) (l2, v2) = do
        assert (l1 == l2)
        (,) l1 <$> (liftIO $ termIte sbe assertions v1 v2)
            
  mergedSFields <- mergeBy (mergeValues assertions) sFields1 sFields2
  mergedIFields <- mergeBy (mergeValues assertions) iFields1 iFields2
  mergedScArrays <- mergeBy mergeTup scArrays1 scArrays2
  return $ mem2 & memInitialization .~ mergedInit
                & memStaticFields   .~ mergedSFields
                & memInstanceFields .~ mergedIFields
                & memScalarArrays   .~ mergedScArrays
                & memRefArrays      .~ mergedRArrays
                & memClassObjects   .~ mergedCObjs

mergeCallFrames :: MonadIO m
                => SBETerm sbe
                -> CallFrame (SBETerm sbe)
                -> CallFrame (SBETerm sbe)
                -> Simulator sbe m (CallFrame (SBETerm sbe))
mergeCallFrames assertions cf1 cf2 = do
  let CallFrame class1 method1 _bb1 locals1 opds1 = cf1
      CallFrame class2 method2 _bb2 locals2 opds2 = cf2
  assert (class1  == class2)
  assert (method1 == method2)
  -- pcs may differ if paths merge at different return insts
  mergedLocals <- mergeBy (mergeValues assertions) locals1 locals2
  assert (length opds1 == length opds2)
  mergedOpds <- zipWithM (mergeValues assertions) opds1 opds2
  return $ cf2 & cfLocals .~ mergedLocals
               & cfOpds   .~ mergedOpds

-- | Merge the map elements (given via the selector 'sel') common to both
-- states via the given action 'mrg', and then union in elements unique to
-- each state as well (via the left-biased map union operator)
mergeBy :: (Functor m, Monad m, Ord k)
        => (a -> a -> m a)
        -> Map k a
        -> Map k a
        -> m (Map k a)
mergeBy mrg m1 m2 = leftUnion <$> merged
  where
    -- Use the left-biasing of M.union to prefer the key/value mappings in
    -- 'x', and then take everything else in the selected 'from' and 'to'
    -- maps.
    leftUnion x = x `M.union` m1 `M.union` m2
    merged      =
      DF.foldrM
        (\(k, v1, v2) acc -> flip (M.insert k) acc <$> mrg v1 v2)
        M.empty
        (M.intersectionWithKey (\k v1 v2 -> (k, v1, v2)) m1 m2)


-- | Merge the two symbolic values under the given assertions
mergeValues :: MonadIO m 
            => SBETerm sbe 
            -> Value (SBETerm sbe) 
            -> Value (SBETerm sbe) 
            -> Simulator sbe m (Value (SBETerm sbe)) 
mergeValues assertions x y = do
  sbe <- use backend 
  let abort = fail . render
      t1 <-> t2 = liftIO $ termIte sbe assertions t1 t2
      mergeV (IValue v1) (IValue v2)             = IValue <$> v1 <-> v2
      mergeV (LValue v1) (LValue v2)             = LValue <$> v1 <-> v2
      mergeV (FValue v1) (FValue v2) = do
        if (isNaN v1 && isNaN v2 || v1 == v2)
          then return x
          else abort $ "Attempt to merge two concrete non-NaN unequal floats:"
               <+> ppValue sbe x <+> "and" <+> ppValue sbe y
      mergeV (DValue v1) (DValue v2) = do
        if (isNaN v1 && isNaN v2 || v1 == v2)
          then return x
          else abort $ "Attempt to merge two concrete non-NaN unequal doubles: "
               <+> ppValue sbe x <+> "and" <+> ppValue sbe y
      mergeV (RValue NullRef) (RValue NullRef) = return x
      mergeV (RValue (Ref r1 ty1)) (RValue (Ref r2 ty2)) = do
        when (r1 /= r2) $
          abort $ "References differ when merging:" 
          <+> ppValue sbe x <+> "and" <+> ppValue sbe y
        assert (ty1 == ty2)
        return x
      mergeV _ _ = 
        abort $ "Unsupported or mismatched type when merging values:"
        <+> ppValue sbe x <+> "and" <+> ppValue sbe y
  mergeV x y

--------------------------------------------------------------------------------
-- Pretty printers

ppJavaException :: JavaException term -> Doc
ppJavaException (JavaException (Ref _ (ClassType nm)) _) =
  "Exception of type" <+> text (slashesToDots nm)
ppJavaException (JavaException r _) =
  "Unknown exception type" <+> ppRef r

ppValue :: Backend sbe -> Value (SBETerm sbe) -> Doc
ppValue sbe (IValue st) = prettyTermD sbe st
ppValue sbe (LValue st) = prettyTermD sbe st
ppValue _   (RValue r)  = ppRef r
ppValue _   (FValue f)  = float f
ppValue _   (DValue d)  = double d
ppValue _   (AValue a)  = "Address" <+> ppPC a

ppRef :: Ref -> Doc
ppRef NullRef    = "null"
ppRef (Ref n ty) = integer (fromIntegral n) <> "::" <> ppType ty

ppMethod :: Method -> Doc
ppMethod = text . methodKeyName . methodKey

ppState :: State sbe m -> Doc
ppState s = hang (text "state" <+> lbrace) 2 (ppCtrlStk (s^.backend) (s^.ctrlStk)) $+$ rbrace

ppPath :: Backend sbe -> Path sbe -> Doc
ppPath sbe p =
  case currentCallFrame p of
    Just cf -> 
      text "Path #"
      <>  integer (p^.pathName)
      <>  brackets ( text (cf^.cfClass) <> "." <> ppMethod (cf^.cfMethod)
                   <> "/" <> maybe "none" ppBlockId (p^.pathBlockId)
                   )
      <>  colon
      $+$ nest 2 (text "Locals:"   $+$ nest 2 (ppLocals sbe (cf^.cfLocals)))
      $+$ nest 2 (text "Operands:" $+$ nest 2 (ppOpds sbe (cf^.cfOpds)))
      $+$ nest 2 (text "Call Stack:" $+$ nest 2 (ppStackTrace (p^.pathStack)))
    Nothing ->
      text "Path #" <> integer (p^.pathName) <> colon <+> "stopped"

ppStackTrace :: [CallFrame term] -> Doc
ppStackTrace cfs = braces . commas $ 
  [ text cName <> "." <> ppMethod method
  | CallFrame { _cfClass = cName, _cfMethod = method } <- cfs
  ]

ppLocals :: Backend sbe 
         -> Map LocalVariableIndex (Value (SBETerm sbe))
         -> Doc
ppLocals sbe = braces . commas . M.elems . M.mapWithKey ppPair
  where ppPair idx val = int (fromIntegral idx) <+> ":=" <+> ppValue sbe val

ppMemory :: Backend sbe
         -> Memory (SBETerm sbe)
         -> Doc
ppMemory sbe mem = hang ("memory" <> colon) 2 (brackets . commas $ rest)
  where rest = [ 
            hang ("class initialization" <> colon) 2 $
              ppMap text (text . show) (mem^.memInitialization)
          , hang ("static fields" <> colon) 2 $ 
              ppMap (text . ppFldId) (ppValue sbe) (mem^.memStaticFields)
          , hang ("instance fields" <> colon) 2 $
              ppMap ppInstanceFieldRef (ppValue sbe) (mem^.memInstanceFields)
          , let ppArr (len, a) = brackets (int (fromIntegral len)) 
                                 <+> prettyTermD sbe a
            in hang ("scalar arrays" <> colon) 2 $ 
                 ppMap ppRef ppArr (mem^.memScalarArrays)
          , let ppArr = ppArray (int . fromIntegral) ppRef
            in hang ("reference arrays" <> colon) 2 $
                 ppMap ppRef ppArr (mem^.memRefArrays)
          , hang ("class objects" <> colon) 2 $
              ppMap text ppRef (mem^.memClassObjects)
          ]

dumpMemory :: (Functor m, MonadIO m) => Doc -> Simulator sbe m ()
dumpMemory ctx = do
  sbe <- use backend
  m <- getMem ctx
  liftIO . putStrLn . render $ ctx <> ":" <+> (ppMemory sbe m)

ppInstanceFieldRef :: InstanceFieldRef -> Doc
ppInstanceFieldRef (ref, fld) = text (ppFldId fld) <> (braces . ppRef $ ref)

ppArray :: Ix i => (i -> Doc) -> (e -> Doc) -> Array i e -> Doc
ppArray idxf eltf = braces . commas . map ppPair . assocs
  where ppPair (idx, elt) = idxf idx <+> ":=" <+> eltf elt

ppMap :: (k -> Doc) -> (v -> Doc) -> Map k v -> Doc
ppMap kf vf = braces . commas . map ppPair . M.toList
  where ppPair (k, v) = kf k <+> ":=" <+> vf v

ppOpds :: Backend sbe -> [Value (SBETerm sbe)] -> Doc
ppOpds sbe = brackets . commas . map (ppValue sbe)

ppCtrlStk :: Backend sbe -> CS sbe -> Doc
ppCtrlStk sbe (CompletedCS mp) =
  "Completed stack:" <+> maybe (text "All paths failed") (ppPath sbe) mp
ppCtrlStk sbe (ActiveCS p k) =
  text "Active path:" $$
  ppPath sbe p $$
  ppSimCont sbe k

ppSimCont :: Backend sbe -> SimCont sbe -> Doc
ppSimCont sbe (HandleBranch point ba h) = 
  text "on" <+> ppMergePoint point <+> text "do" $$
  nest 2 (ppBranchAction sbe ba) $$
  ppSimCont sbe h
ppSimCont _ EmptyCont = text "stop"

ppBranchAction :: Backend sbe -> BranchAction sbe -> Doc
ppBranchAction sbe (BranchRunTrue c p) = 
  text "runTrue" <+> prettyTermD sbe c $$
  nest 2 (ppPath sbe p)
ppBranchAction sbe (BranchMerge a c p) =
  text "mergeBranch" <+> prettyTermD sbe c $$
  nest 2 (text "assumptions:" <+> prettyTermD sbe a) $$
  nest 2 (ppPath sbe p)

ppMergePoint :: MergePoint -> Doc
ppMergePoint (ReturnPoint n) = text "return" <> parens (int n)
ppMergePoint (PostdomPoint n b) =
    text "postdom" <> parens (int n <+> ppBlockId b)

dumpCtrlStk :: (MonadIO m) => Simulator sbe m ()
dumpCtrlStk = do
  (sbe, cs) <- (,) <$> use backend <*> use ctrlStk
  liftIO $ putStrLn $ replicate 80 '-'
  liftIO $ putStrLn $ show $ ppCtrlStk sbe cs
  liftIO $ putStrLn $ replicate 80 '-'

ppFailRsn :: FailRsn -> Doc
ppFailRsn (FailRsn msg) = text msg

setInitializationStatus :: String
                        -> InitializationStatus
                        -> Memory term
                        -> Memory term
setInitializationStatus clName status = 
  memInitialization %~ M.insert clName status

ppCurrentPath :: MonadSim sbe m => Simulator sbe m Doc
ppCurrentPath = do
  cs <- use ctrlStk
  sbe <- use backend
  return $ case cs of
    CompletedCS Nothing  -> "all paths failed"
    CompletedCS (Just p) -> "completed path" <> colon <+> ppPath sbe p
    ActiveCS p _         -> "active path" <> colon <+> ppPath sbe p

dumpCurrentPath :: MonadSim sbe m => Simulator sbe m ()
dumpCurrentPath = do
  path <- ppCurrentPath
  liftIO . putStrLn . render $ path

ppInternalExc :: InternalExc sbe m -> Doc
ppInternalExc exc = case exc of
  ErrorPathExc rsn s -> 
      "internal error" <> colon <+> ppFailRsn rsn <+> ppState s
  UnknownExc Nothing -> 
      "unknown error"
  UnknownExc (Just rsn) -> 
      "unknown error" <> colon <+> ppFailRsn rsn

--------------------------------------------------------------------------------
-- Manual lens implementations. These are captured by using
-- -ddump-splices output from makeLenses, but is reproduced here
-- statically in order to avoid having to link libabc at compile
-- time. Note that these will have to be regenerated whenever fields
-- are changed for the relevant types.

-- TODO: fix linking problems so we can do this with TH

-- src/Verifier/Java/Common.hs:1:1: Splicing declarations
--     makeLenses ''State
--   ======>
--     src/Verifier/Java/Common.hs:440:1-18
backend ::
  forall sbe_a3Ep m_a3Eq.
  Lens' (State sbe_a3Ep m_a3Eq) (Backend sbe_a3Ep)
backend
  _f_a5uF
  (State __codebase_a5uG
         __instanceOverrides_a5uH
         __staticOverrides_a5uI
         __ctrlStk_a5uJ
         __nextPSS_a5uK
         __strings_a5uL
         __nextRef_a5uM
         __verbosity_a5uN
         __simulationFlags_a5uO
         __backend'_a5uP
         __errorPaths_a5uR
         __printErrPaths_a5uS
         __evHandlers_a5uT)
  = ((\ __backend_a5uQ
        -> State
             __codebase_a5uG
             __instanceOverrides_a5uH
             __staticOverrides_a5uI
             __ctrlStk_a5uJ
             __nextPSS_a5uK
             __strings_a5uL
             __nextRef_a5uM
             __verbosity_a5uN
             __simulationFlags_a5uO
             __backend_a5uQ
             __errorPaths_a5uR
             __printErrPaths_a5uS
             __evHandlers_a5uT)
     <$> (_f_a5uF __backend'_a5uP))
{-# INLINE backend #-}
codebase ::
  forall sbe_a3Ep m_a3Eq. Lens' (State sbe_a3Ep m_a3Eq) Codebase
codebase
  _f_a5uU
  (State __codebase'_a5uV
         __instanceOverrides_a5uX
         __staticOverrides_a5uY
         __ctrlStk_a5uZ
         __nextPSS_a5v0
         __strings_a5v1
         __nextRef_a5v2
         __verbosity_a5v3
         __simulationFlags_a5v4
         __backend_a5v5
         __errorPaths_a5v6
         __printErrPaths_a5v7
         __evHandlers_a5v8)
  = ((\ __codebase_a5uW
        -> State
             __codebase_a5uW
             __instanceOverrides_a5uX
             __staticOverrides_a5uY
             __ctrlStk_a5uZ
             __nextPSS_a5v0
             __strings_a5v1
             __nextRef_a5v2
             __verbosity_a5v3
             __simulationFlags_a5v4
             __backend_a5v5
             __errorPaths_a5v6
             __printErrPaths_a5v7
             __evHandlers_a5v8)
     <$> (_f_a5uU __codebase'_a5uV))
{-# INLINE codebase #-}
ctrlStk ::
  forall sbe_a3Ep m_a3Eq. Lens' (State sbe_a3Ep m_a3Eq) (CS sbe_a3Ep)
ctrlStk
  _f_a5v9
  (State __codebase_a5va
         __instanceOverrides_a5vb
         __staticOverrides_a5vc
         __ctrlStk'_a5vd
         __nextPSS_a5vf
         __strings_a5vg
         __nextRef_a5vh
         __verbosity_a5vi
         __simulationFlags_a5vj
         __backend_a5vk
         __errorPaths_a5vl
         __printErrPaths_a5vm
         __evHandlers_a5vn)
  = ((\ __ctrlStk_a5ve
        -> State
             __codebase_a5va
             __instanceOverrides_a5vb
             __staticOverrides_a5vc
             __ctrlStk_a5ve
             __nextPSS_a5vf
             __strings_a5vg
             __nextRef_a5vh
             __verbosity_a5vi
             __simulationFlags_a5vj
             __backend_a5vk
             __errorPaths_a5vl
             __printErrPaths_a5vm
             __evHandlers_a5vn)
     <$> (_f_a5v9 __ctrlStk'_a5vd))
{-# INLINE ctrlStk #-}
errorPaths ::
  forall sbe_a3Ep m_a3Eq.
  Lens' (State sbe_a3Ep m_a3Eq) [ErrorPath sbe_a3Ep]
errorPaths
  _f_a5vo
  (State __codebase_a5vp
         __instanceOverrides_a5vq
         __staticOverrides_a5vr
         __ctrlStk_a5vs
         __nextPSS_a5vt
         __strings_a5vu
         __nextRef_a5vv
         __verbosity_a5vw
         __simulationFlags_a5vx
         __backend_a5vy
         __errorPaths'_a5vz
         __printErrPaths_a5vB
         __evHandlers_a5vC)
  = ((\ __errorPaths_a5vA
        -> State
             __codebase_a5vp
             __instanceOverrides_a5vq
             __staticOverrides_a5vr
             __ctrlStk_a5vs
             __nextPSS_a5vt
             __strings_a5vu
             __nextRef_a5vv
             __verbosity_a5vw
             __simulationFlags_a5vx
             __backend_a5vy
             __errorPaths_a5vA
             __printErrPaths_a5vB
             __evHandlers_a5vC)
     <$> (_f_a5vo __errorPaths'_a5vz))
{-# INLINE errorPaths #-}
evHandlers ::
  forall sbe_a3Ep m_a3Eq.
  Lens' (State sbe_a3Ep m_a3Eq) (SEH sbe_a3Ep m_a3Eq)
evHandlers
  _f_a5vD
  (State __codebase_a5vE
         __instanceOverrides_a5vF
         __staticOverrides_a5vG
         __ctrlStk_a5vH
         __nextPSS_a5vI
         __strings_a5vJ
         __nextRef_a5vK
         __verbosity_a5vL
         __simulationFlags_a5vM
         __backend_a5vN
         __errorPaths_a5vO
         __printErrPaths_a5vP
         __evHandlers'_a5vQ)
  = ((\ __evHandlers_a5vR
        -> State
             __codebase_a5vE
             __instanceOverrides_a5vF
             __staticOverrides_a5vG
             __ctrlStk_a5vH
             __nextPSS_a5vI
             __strings_a5vJ
             __nextRef_a5vK
             __verbosity_a5vL
             __simulationFlags_a5vM
             __backend_a5vN
             __errorPaths_a5vO
             __printErrPaths_a5vP
             __evHandlers_a5vR)
     <$> (_f_a5vD __evHandlers'_a5vQ))
{-# INLINE evHandlers #-}
instanceOverrides ::
  forall sbe_a3Ep m_a3Eq.
  Lens' (State sbe_a3Ep m_a3Eq) (Map (String,
                                      MethodKey) (InstanceOverride sbe_a3Ep m_a3Eq))
instanceOverrides
  _f_a5vS
  (State __codebase_a5vT
         __instanceOverrides'_a5vU
         __staticOverrides_a5vW
         __ctrlStk_a5vX
         __nextPSS_a5vY
         __strings_a5vZ
         __nextRef_a5w0
         __verbosity_a5w1
         __simulationFlags_a5w2
         __backend_a5w3
         __errorPaths_a5w4
         __printErrPaths_a5w5
         __evHandlers_a5w6)
  = ((\ __instanceOverrides_a5vV
        -> State
             __codebase_a5vT
             __instanceOverrides_a5vV
             __staticOverrides_a5vW
             __ctrlStk_a5vX
             __nextPSS_a5vY
             __strings_a5vZ
             __nextRef_a5w0
             __verbosity_a5w1
             __simulationFlags_a5w2
             __backend_a5w3
             __errorPaths_a5w4
             __printErrPaths_a5w5
             __evHandlers_a5w6)
     <$> (_f_a5vS __instanceOverrides'_a5vU))
{-# INLINE instanceOverrides #-}
nextPSS ::
  forall sbe_a3Ep m_a3Eq.
  Lens' (State sbe_a3Ep m_a3Eq) PathDescriptor
nextPSS
  _f_a5w7
  (State __codebase_a5w8
         __instanceOverrides_a5w9
         __staticOverrides_a5wa
         __ctrlStk_a5wb
         __nextPSS'_a5wc
         __strings_a5we
         __nextRef_a5wf
         __verbosity_a5wg
         __simulationFlags_a5wh
         __backend_a5wi
         __errorPaths_a5wj
         __printErrPaths_a5wk
         __evHandlers_a5wl)
  = ((\ __nextPSS_a5wd
        -> State
             __codebase_a5w8
             __instanceOverrides_a5w9
             __staticOverrides_a5wa
             __ctrlStk_a5wb
             __nextPSS_a5wd
             __strings_a5we
             __nextRef_a5wf
             __verbosity_a5wg
             __simulationFlags_a5wh
             __backend_a5wi
             __errorPaths_a5wj
             __printErrPaths_a5wk
             __evHandlers_a5wl)
     <$> (_f_a5w7 __nextPSS'_a5wc))
{-# INLINE nextPSS #-}
nextRef ::
  forall sbe_a3Ep m_a3Eq. Lens' (State sbe_a3Ep m_a3Eq) Word32
nextRef
  _f_a5wm
  (State __codebase_a5wn
         __instanceOverrides_a5wo
         __staticOverrides_a5wp
         __ctrlStk_a5wq
         __nextPSS_a5wr
         __strings_a5ws
         __nextRef'_a5wt
         __verbosity_a5wv
         __simulationFlags_a5ww
         __backend_a5wx
         __errorPaths_a5wy
         __printErrPaths_a5wz
         __evHandlers_a5wA)
  = ((\ __nextRef_a5wu
        -> State
             __codebase_a5wn
             __instanceOverrides_a5wo
             __staticOverrides_a5wp
             __ctrlStk_a5wq
             __nextPSS_a5wr
             __strings_a5ws
             __nextRef_a5wu
             __verbosity_a5wv
             __simulationFlags_a5ww
             __backend_a5wx
             __errorPaths_a5wy
             __printErrPaths_a5wz
             __evHandlers_a5wA)
     <$> (_f_a5wm __nextRef'_a5wt))
{-# INLINE nextRef #-}
printErrPaths ::
  forall sbe_a3Ep m_a3Eq. Lens' (State sbe_a3Ep m_a3Eq) Bool
printErrPaths
  _f_a5wB
  (State __codebase_a5wC
         __instanceOverrides_a5wD
         __staticOverrides_a5wE
         __ctrlStk_a5wF
         __nextPSS_a5wG
         __strings_a5wH
         __nextRef_a5wI
         __verbosity_a5wJ
         __simulationFlags_a5wK
         __backend_a5wL
         __errorPaths_a5wM
         __printErrPaths'_a5wN
         __evHandlers_a5wP)
  = ((\ __printErrPaths_a5wO
        -> State
             __codebase_a5wC
             __instanceOverrides_a5wD
             __staticOverrides_a5wE
             __ctrlStk_a5wF
             __nextPSS_a5wG
             __strings_a5wH
             __nextRef_a5wI
             __verbosity_a5wJ
             __simulationFlags_a5wK
             __backend_a5wL
             __errorPaths_a5wM
             __printErrPaths_a5wO
             __evHandlers_a5wP)
     <$> (_f_a5wB __printErrPaths'_a5wN))
{-# INLINE printErrPaths #-}
simulationFlags ::
  forall sbe_a3Ep m_a3Eq.
  Lens' (State sbe_a3Ep m_a3Eq) SimulationFlags
simulationFlags
  _f_a5wQ
  (State __codebase_a5wR
         __instanceOverrides_a5wS
         __staticOverrides_a5wT
         __ctrlStk_a5wU
         __nextPSS_a5wV
         __strings_a5wW
         __nextRef_a5wX
         __verbosity_a5wY
         __simulationFlags'_a5wZ
         __backend_a5x1
         __errorPaths_a5x2
         __printErrPaths_a5x3
         __evHandlers_a5x4)
  = ((\ __simulationFlags_a5x0
        -> State
             __codebase_a5wR
             __instanceOverrides_a5wS
             __staticOverrides_a5wT
             __ctrlStk_a5wU
             __nextPSS_a5wV
             __strings_a5wW
             __nextRef_a5wX
             __verbosity_a5wY
             __simulationFlags_a5x0
             __backend_a5x1
             __errorPaths_a5x2
             __printErrPaths_a5x3
             __evHandlers_a5x4)
     <$> (_f_a5wQ __simulationFlags'_a5wZ))
{-# INLINE simulationFlags #-}
staticOverrides ::
  forall sbe_a3Ep m_a3Eq.
  Lens' (State sbe_a3Ep m_a3Eq) (Map (String,
                                      MethodKey) (StaticOverride sbe_a3Ep m_a3Eq))
staticOverrides
  _f_a5x5
  (State __codebase_a5x6
         __instanceOverrides_a5x7
         __staticOverrides'_a5x8
         __ctrlStk_a5xa
         __nextPSS_a5xb
         __strings_a5xc
         __nextRef_a5xd
         __verbosity_a5xe
         __simulationFlags_a5xf
         __backend_a5xg
         __errorPaths_a5xh
         __printErrPaths_a5xi
         __evHandlers_a5xj)
  = ((\ __staticOverrides_a5x9
        -> State
             __codebase_a5x6
             __instanceOverrides_a5x7
             __staticOverrides_a5x9
             __ctrlStk_a5xa
             __nextPSS_a5xb
             __strings_a5xc
             __nextRef_a5xd
             __verbosity_a5xe
             __simulationFlags_a5xf
             __backend_a5xg
             __errorPaths_a5xh
             __printErrPaths_a5xi
             __evHandlers_a5xj)
     <$> (_f_a5x5 __staticOverrides'_a5x8))
{-# INLINE staticOverrides #-}
strings ::
  forall sbe_a3Ep m_a3Eq.
  Lens' (State sbe_a3Ep m_a3Eq) (Map String Ref)
strings
  _f_a5xk
  (State __codebase_a5xl
         __instanceOverrides_a5xm
         __staticOverrides_a5xn
         __ctrlStk_a5xo
         __nextPSS_a5xp
         __strings'_a5xq
         __nextRef_a5xs
         __verbosity_a5xt
         __simulationFlags_a5xu
         __backend_a5xv
         __errorPaths_a5xw
         __printErrPaths_a5xx
         __evHandlers_a5xy)
  = ((\ __strings_a5xr
        -> State
             __codebase_a5xl
             __instanceOverrides_a5xm
             __staticOverrides_a5xn
             __ctrlStk_a5xo
             __nextPSS_a5xp
             __strings_a5xr
             __nextRef_a5xs
             __verbosity_a5xt
             __simulationFlags_a5xu
             __backend_a5xv
             __errorPaths_a5xw
             __printErrPaths_a5xx
             __evHandlers_a5xy)
     <$> (_f_a5xk __strings'_a5xq))
{-# INLINE strings #-}
verbosity ::
  forall sbe_a3Ep m_a3Eq. Lens' (State sbe_a3Ep m_a3Eq) Int
verbosity
  _f_a5xz
  (State __codebase_a5xA
         __instanceOverrides_a5xB
         __staticOverrides_a5xC
         __ctrlStk_a5xD
         __nextPSS_a5xE
         __strings_a5xF
         __nextRef_a5xG
         __verbosity'_a5xH
         __simulationFlags_a5xJ
         __backend_a5xK
         __errorPaths_a5xL
         __printErrPaths_a5xM
         __evHandlers_a5xN)
  = ((\ __verbosity_a5xI
        -> State
             __codebase_a5xA
             __instanceOverrides_a5xB
             __staticOverrides_a5xC
             __ctrlStk_a5xD
             __nextPSS_a5xE
             __strings_a5xF
             __nextRef_a5xG
             __verbosity_a5xI
             __simulationFlags_a5xJ
             __backend_a5xK
             __errorPaths_a5xL
             __printErrPaths_a5xM
             __evHandlers_a5xN)
     <$> (_f_a5xz __verbosity'_a5xH))
{-# INLINE verbosity #-}
-- src/Verifier/Java/Common.hs:1:1: Splicing declarations
--     makeLenses ''Path'
--   ======>
--     src/Verifier/Java/Common.hs:441:1-18
pathAssertions ::
  forall term_a3vM. Lens' (Path' term_a3vM) term_a3vM
pathAssertions
  _f_a5Bi
  (Path __pathStack_a5Bj
        __pathStackHt_a5Bk
        __pathBlockId_a5Bl
        __pathRetVal_a5Bm
        __pathException_a5Bn
        __pathMemory_a5Bo
        __pathAssertions'_a5Bp
        __pathName_a5Br
        __pathStartingPC_a5Bs
        __pathBreakpoints_a5Bt
        __pathInsnCount_a5Bu)
  = ((\ __pathAssertions_a5Bq
        -> Path
             __pathStack_a5Bj
             __pathStackHt_a5Bk
             __pathBlockId_a5Bl
             __pathRetVal_a5Bm
             __pathException_a5Bn
             __pathMemory_a5Bo
             __pathAssertions_a5Bq
             __pathName_a5Br
             __pathStartingPC_a5Bs
             __pathBreakpoints_a5Bt
             __pathInsnCount_a5Bu)
     <$> (_f_a5Bi __pathAssertions'_a5Bp))
{-# INLINE pathAssertions #-}
pathBlockId ::
  forall term_a3vM. Lens' (Path' term_a3vM) (Maybe BlockId)
pathBlockId
  _f_a5Bv
  (Path __pathStack_a5Bw
        __pathStackHt_a5Bx
        __pathBlockId'_a5By
        __pathRetVal_a5BA
        __pathException_a5BB
        __pathMemory_a5BC
        __pathAssertions_a5BD
        __pathName_a5BE
        __pathStartingPC_a5BF
        __pathBreakpoints_a5BG
        __pathInsnCount_a5BH)
  = ((\ __pathBlockId_a5Bz
        -> Path
             __pathStack_a5Bw
             __pathStackHt_a5Bx
             __pathBlockId_a5Bz
             __pathRetVal_a5BA
             __pathException_a5BB
             __pathMemory_a5BC
             __pathAssertions_a5BD
             __pathName_a5BE
             __pathStartingPC_a5BF
             __pathBreakpoints_a5BG
             __pathInsnCount_a5BH)
     <$> (_f_a5Bv __pathBlockId'_a5By))
{-# INLINE pathBlockId #-}
pathBreakpoints ::
  forall term_a3vM.
  Lens' (Path' term_a3vM) (Set (String, MethodKey, PC))
pathBreakpoints
  _f_a5BI
  (Path __pathStack_a5BJ
        __pathStackHt_a5BK
        __pathBlockId_a5BL
        __pathRetVal_a5BM
        __pathException_a5BN
        __pathMemory_a5BO
        __pathAssertions_a5BP
        __pathName_a5BQ
        __pathStartingPC_a5BR
        __pathBreakpoints'_a5BS
        __pathInsnCount_a5BU)
  = ((\ __pathBreakpoints_a5BT
        -> Path
             __pathStack_a5BJ
             __pathStackHt_a5BK
             __pathBlockId_a5BL
             __pathRetVal_a5BM
             __pathException_a5BN
             __pathMemory_a5BO
             __pathAssertions_a5BP
             __pathName_a5BQ
             __pathStartingPC_a5BR
             __pathBreakpoints_a5BT
             __pathInsnCount_a5BU)
     <$> (_f_a5BI __pathBreakpoints'_a5BS))
{-# INLINE pathBreakpoints #-}
pathException ::
  forall term_a3vM.
  Lens' (Path' term_a3vM) (Maybe (JavaException term_a3vM))
pathException
  _f_a5BV
  (Path __pathStack_a5BW
        __pathStackHt_a5BX
        __pathBlockId_a5BY
        __pathRetVal_a5BZ
        __pathException'_a5C0
        __pathMemory_a5C2
        __pathAssertions_a5C3
        __pathName_a5C4
        __pathStartingPC_a5C5
        __pathBreakpoints_a5C6
        __pathInsnCount_a5C7)
  = ((\ __pathException_a5C1
        -> Path
             __pathStack_a5BW
             __pathStackHt_a5BX
             __pathBlockId_a5BY
             __pathRetVal_a5BZ
             __pathException_a5C1
             __pathMemory_a5C2
             __pathAssertions_a5C3
             __pathName_a5C4
             __pathStartingPC_a5C5
             __pathBreakpoints_a5C6
             __pathInsnCount_a5C7)
     <$> (_f_a5BV __pathException'_a5C0))
{-# INLINE pathException #-}
pathInsnCount :: forall term_a3vM. Lens' (Path' term_a3vM) Int
pathInsnCount
  _f_a5C8
  (Path __pathStack_a5C9
        __pathStackHt_a5Ca
        __pathBlockId_a5Cb
        __pathRetVal_a5Cc
        __pathException_a5Cd
        __pathMemory_a5Ce
        __pathAssertions_a5Cf
        __pathName_a5Cg
        __pathStartingPC_a5Ch
        __pathBreakpoints_a5Ci
        __pathInsnCount'_a5Cj)
  = ((\ __pathInsnCount_a5Ck
        -> Path
             __pathStack_a5C9
             __pathStackHt_a5Ca
             __pathBlockId_a5Cb
             __pathRetVal_a5Cc
             __pathException_a5Cd
             __pathMemory_a5Ce
             __pathAssertions_a5Cf
             __pathName_a5Cg
             __pathStartingPC_a5Ch
             __pathBreakpoints_a5Ci
             __pathInsnCount_a5Ck)
     <$> (_f_a5C8 __pathInsnCount'_a5Cj))
{-# INLINE pathInsnCount #-}
pathMemory ::
  forall term_a3vM. Lens' (Path' term_a3vM) (Memory term_a3vM)
pathMemory
  _f_a5Cl
  (Path __pathStack_a5Cm
        __pathStackHt_a5Cn
        __pathBlockId_a5Co
        __pathRetVal_a5Cp
        __pathException_a5Cq
        __pathMemory'_a5Cr
        __pathAssertions_a5Ct
        __pathName_a5Cu
        __pathStartingPC_a5Cv
        __pathBreakpoints_a5Cw
        __pathInsnCount_a5Cx)
  = ((\ __pathMemory_a5Cs
        -> Path
             __pathStack_a5Cm
             __pathStackHt_a5Cn
             __pathBlockId_a5Co
             __pathRetVal_a5Cp
             __pathException_a5Cq
             __pathMemory_a5Cs
             __pathAssertions_a5Ct
             __pathName_a5Cu
             __pathStartingPC_a5Cv
             __pathBreakpoints_a5Cw
             __pathInsnCount_a5Cx)
     <$> (_f_a5Cl __pathMemory'_a5Cr))
{-# INLINE pathMemory #-}
pathName ::
  forall term_a3vM. Lens' (Path' term_a3vM) PathDescriptor
pathName
  _f_a5Cy
  (Path __pathStack_a5Cz
        __pathStackHt_a5CA
        __pathBlockId_a5CB
        __pathRetVal_a5CC
        __pathException_a5CD
        __pathMemory_a5CE
        __pathAssertions_a5CF
        __pathName'_a5CG
        __pathStartingPC_a5CI
        __pathBreakpoints_a5CJ
        __pathInsnCount_a5CK)
  = ((\ __pathName_a5CH
        -> Path
             __pathStack_a5Cz
             __pathStackHt_a5CA
             __pathBlockId_a5CB
             __pathRetVal_a5CC
             __pathException_a5CD
             __pathMemory_a5CE
             __pathAssertions_a5CF
             __pathName_a5CH
             __pathStartingPC_a5CI
             __pathBreakpoints_a5CJ
             __pathInsnCount_a5CK)
     <$> (_f_a5Cy __pathName'_a5CG))
{-# INLINE pathName #-}
pathRetVal ::
  forall term_a3vM. Lens' (Path' term_a3vM) (Maybe (Value term_a3vM))
pathRetVal
  _f_a5CL
  (Path __pathStack_a5CM
        __pathStackHt_a5CN
        __pathBlockId_a5CO
        __pathRetVal'_a5CP
        __pathException_a5CR
        __pathMemory_a5CS
        __pathAssertions_a5CT
        __pathName_a5CU
        __pathStartingPC_a5CV
        __pathBreakpoints_a5CW
        __pathInsnCount_a5CX)
  = ((\ __pathRetVal_a5CQ
        -> Path
             __pathStack_a5CM
             __pathStackHt_a5CN
             __pathBlockId_a5CO
             __pathRetVal_a5CQ
             __pathException_a5CR
             __pathMemory_a5CS
             __pathAssertions_a5CT
             __pathName_a5CU
             __pathStartingPC_a5CV
             __pathBreakpoints_a5CW
             __pathInsnCount_a5CX)
     <$> (_f_a5CL __pathRetVal'_a5CP))
{-# INLINE pathRetVal #-}
pathStack ::
  forall term_a3vM. Lens' (Path' term_a3vM) [CallFrame term_a3vM]
pathStack
  _f_a5CY
  (Path __pathStack'_a5CZ
        __pathStackHt_a5D1
        __pathBlockId_a5D2
        __pathRetVal_a5D3
        __pathException_a5D4
        __pathMemory_a5D5
        __pathAssertions_a5D6
        __pathName_a5D7
        __pathStartingPC_a5D8
        __pathBreakpoints_a5D9
        __pathInsnCount_a5Da)
  = ((\ __pathStack_a5D0
        -> Path
             __pathStack_a5D0
             __pathStackHt_a5D1
             __pathBlockId_a5D2
             __pathRetVal_a5D3
             __pathException_a5D4
             __pathMemory_a5D5
             __pathAssertions_a5D6
             __pathName_a5D7
             __pathStartingPC_a5D8
             __pathBreakpoints_a5D9
             __pathInsnCount_a5Da)
     <$> (_f_a5CY __pathStack'_a5CZ))
{-# INLINE pathStack #-}
pathStackHt :: forall term_a3vM. Lens' (Path' term_a3vM) Int
pathStackHt
  _f_a5Db
  (Path __pathStack_a5Dc
        __pathStackHt'_a5Dd
        __pathBlockId_a5Df
        __pathRetVal_a5Dg
        __pathException_a5Dh
        __pathMemory_a5Di
        __pathAssertions_a5Dj
        __pathName_a5Dk
        __pathStartingPC_a5Dl
        __pathBreakpoints_a5Dm
        __pathInsnCount_a5Dn)
  = ((\ __pathStackHt_a5De
        -> Path
             __pathStack_a5Dc
             __pathStackHt_a5De
             __pathBlockId_a5Df
             __pathRetVal_a5Dg
             __pathException_a5Dh
             __pathMemory_a5Di
             __pathAssertions_a5Dj
             __pathName_a5Dk
             __pathStartingPC_a5Dl
             __pathBreakpoints_a5Dm
             __pathInsnCount_a5Dn)
     <$> (_f_a5Db __pathStackHt'_a5Dd))
{-# INLINE pathStackHt #-}
pathStartingPC :: forall term_a3vM. Lens' (Path' term_a3vM) PC
pathStartingPC
  _f_a5Do
  (Path __pathStack_a5Dp
        __pathStackHt_a5Dq
        __pathBlockId_a5Dr
        __pathRetVal_a5Ds
        __pathException_a5Dt
        __pathMemory_a5Du
        __pathAssertions_a5Dv
        __pathName_a5Dw
        __pathStartingPC'_a5Dx
        __pathBreakpoints_a5Dz
        __pathInsnCount_a5DA)
  = ((\ __pathStartingPC_a5Dy
        -> Path
             __pathStack_a5Dp
             __pathStackHt_a5Dq
             __pathBlockId_a5Dr
             __pathRetVal_a5Ds
             __pathException_a5Dt
             __pathMemory_a5Du
             __pathAssertions_a5Dv
             __pathName_a5Dw
             __pathStartingPC_a5Dy
             __pathBreakpoints_a5Dz
             __pathInsnCount_a5DA)
     <$> (_f_a5Do __pathStartingPC'_a5Dx))
{-# INLINE pathStartingPC #-}
-- src/Verifier/Java/Common.hs:1:1: Splicing declarations
--     makeLenses ''Memory
--   ======>
-- src/Verifier/Java/Common.hs:442:1-19
memClassObjects ::
  forall term_a3sz. Lens' (Memory term_a3sz) (Map String Ref)
memClassObjects
  _f_a5Ex
  (Memory __memInitialization_a5Ey
          __memStaticFields_a5Ez
          __memInstanceFields_a5EA
          __memScalarArrays_a5EB
          __memRefArrays_a5EC
          __memClassObjects'_a5ED)
  = ((\ __memClassObjects_a5EE
        -> Memory
             __memInitialization_a5Ey
             __memStaticFields_a5Ez
             __memInstanceFields_a5EA
             __memScalarArrays_a5EB
             __memRefArrays_a5EC
             __memClassObjects_a5EE)
     <$> (_f_a5Ex __memClassObjects'_a5ED))
{-# INLINE memClassObjects #-}
memInitialization ::
  forall term_a3sz.
  Lens' (Memory term_a3sz) (Map String InitializationStatus)
memInitialization
  _f_a5EF
  (Memory __memInitialization'_a5EG
          __memStaticFields_a5EI
          __memInstanceFields_a5EJ
          __memScalarArrays_a5EK
          __memRefArrays_a5EL
          __memClassObjects_a5EM)
  = ((\ __memInitialization_a5EH
        -> Memory
             __memInitialization_a5EH
             __memStaticFields_a5EI
             __memInstanceFields_a5EJ
             __memScalarArrays_a5EK
             __memRefArrays_a5EL
             __memClassObjects_a5EM)
     <$> (_f_a5EF __memInitialization'_a5EG))
{-# INLINE memInitialization #-}
memInstanceFields ::
  forall term_a3sz.
  Lens' (Memory term_a3sz) (Map InstanceFieldRef (Value term_a3sz))
memInstanceFields
  _f_a5EN
  (Memory __memInitialization_a5EO
          __memStaticFields_a5EP
          __memInstanceFields'_a5EQ
          __memScalarArrays_a5ES
          __memRefArrays_a5ET
          __memClassObjects_a5EU)
  = ((\ __memInstanceFields_a5ER
        -> Memory
             __memInitialization_a5EO
             __memStaticFields_a5EP
             __memInstanceFields_a5ER
             __memScalarArrays_a5ES
             __memRefArrays_a5ET
             __memClassObjects_a5EU)
     <$> (_f_a5EN __memInstanceFields'_a5EQ))
{-# INLINE memInstanceFields #-}
memRefArrays ::
  forall term_a3sz.
  Lens' (Memory term_a3sz) (Map Ref (Array Int32 Ref))
memRefArrays
  _f_a5EV
  (Memory __memInitialization_a5EW
          __memStaticFields_a5EX
          __memInstanceFields_a5EY
          __memScalarArrays_a5EZ
          __memRefArrays'_a5F0
          __memClassObjects_a5F2)
  = ((\ __memRefArrays_a5F1
        -> Memory
             __memInitialization_a5EW
             __memStaticFields_a5EX
             __memInstanceFields_a5EY
             __memScalarArrays_a5EZ
             __memRefArrays_a5F1
             __memClassObjects_a5F2)
     <$> (_f_a5EV __memRefArrays'_a5F0))
{-# INLINE memRefArrays #-}
memScalarArrays ::
  forall term_a3sz.
  Lens' (Memory term_a3sz) (Map Ref (Int32, term_a3sz))
memScalarArrays
  _f_a5F3
  (Memory __memInitialization_a5F4
          __memStaticFields_a5F5
          __memInstanceFields_a5F6
          __memScalarArrays'_a5F7
          __memRefArrays_a5F9
          __memClassObjects_a5Fa)
  = ((\ __memScalarArrays_a5F8
        -> Memory
             __memInitialization_a5F4
             __memStaticFields_a5F5
             __memInstanceFields_a5F6
             __memScalarArrays_a5F8
             __memRefArrays_a5F9
             __memClassObjects_a5Fa)
     <$> (_f_a5F3 __memScalarArrays'_a5F7))
{-# INLINE memScalarArrays #-}
memStaticFields ::
  forall term_a3sz.
  Lens' (Memory term_a3sz) (Map FieldId (Value term_a3sz))
memStaticFields
  _f_a5Fb
  (Memory __memInitialization_a5Fc
          __memStaticFields'_a5Fd
          __memInstanceFields_a5Ff
          __memScalarArrays_a5Fg
          __memRefArrays_a5Fh
          __memClassObjects_a5Fi)
  = ((\ __memStaticFields_a5Fe
        -> Memory
             __memInitialization_a5Fc
             __memStaticFields_a5Fe
             __memInstanceFields_a5Ff
             __memScalarArrays_a5Fg
             __memRefArrays_a5Fh
             __memClassObjects_a5Fi)
     <$> (_f_a5Fb __memStaticFields'_a5Fd))
{-# INLINE memStaticFields #-}
-- src/Verifier/Java/Common.hs:1:1: Splicing declarations
--     makeLenses ''CallFrame
--   ======>
--    src/Verifier/Java/Common.hs:443:1-22
cfClass :: forall term_a3sy. Lens' (CallFrame term_a3sy) String
cfClass
  _f_a5FV
  (CallFrame __cfClass'_a5FW
             __cfMethod_a5FY
             __cfReturnBlock_a5FZ
             __cfLocals_a5G0
             __cfOpds_a5G1)
  = ((\ __cfClass_a5FX
        -> CallFrame
             __cfClass_a5FX
             __cfMethod_a5FY
             __cfReturnBlock_a5FZ
             __cfLocals_a5G0
             __cfOpds_a5G1)
     <$> (_f_a5FV __cfClass'_a5FW))
{-# INLINE cfClass #-}
cfLocals ::
  forall term_a3sy.
  Lens' (CallFrame term_a3sy) (Map LocalVariableIndex (Value term_a3sy))
cfLocals
  _f_a5G2
  (CallFrame __cfClass_a5G3
             __cfMethod_a5G4
             __cfReturnBlock_a5G5
             __cfLocals'_a5G6
             __cfOpds_a5G8)
  = ((\ __cfLocals_a5G7
        -> CallFrame
             __cfClass_a5G3
             __cfMethod_a5G4
             __cfReturnBlock_a5G5
             __cfLocals_a5G7
             __cfOpds_a5G8)
     <$> (_f_a5G2 __cfLocals'_a5G6))
{-# INLINE cfLocals #-}
cfMethod :: forall term_a3sy. Lens' (CallFrame term_a3sy) Method
cfMethod
  _f_a5G9
  (CallFrame __cfClass_a5Ga
             __cfMethod'_a5Gb
             __cfReturnBlock_a5Gd
             __cfLocals_a5Ge
             __cfOpds_a5Gf)
  = ((\ __cfMethod_a5Gc
        -> CallFrame
             __cfClass_a5Ga
             __cfMethod_a5Gc
             __cfReturnBlock_a5Gd
             __cfLocals_a5Ge
             __cfOpds_a5Gf)
     <$> (_f_a5G9 __cfMethod'_a5Gb))
{-# INLINE cfMethod #-}
cfOpds ::
  forall term_a3sy. Lens' (CallFrame term_a3sy) [Value term_a3sy]
cfOpds
  _f_a5Gg
  (CallFrame __cfClass_a5Gh
             __cfMethod_a5Gi
             __cfReturnBlock_a5Gj
             __cfLocals_a5Gk
             __cfOpds'_a5Gl)
  = ((\ __cfOpds_a5Gm
        -> CallFrame
             __cfClass_a5Gh
             __cfMethod_a5Gi
             __cfReturnBlock_a5Gj
             __cfLocals_a5Gk
             __cfOpds_a5Gm)
     <$> (_f_a5Gg __cfOpds'_a5Gl))
{-# INLINE cfOpds #-}
cfReturnBlock ::
  forall term_a3sy. Lens' (CallFrame term_a3sy) BlockId
cfReturnBlock
  _f_a5Gn
  (CallFrame __cfClass_a5Go
             __cfMethod_a5Gp
             __cfReturnBlock'_a5Gq
             __cfLocals_a5Gs
             __cfOpds_a5Gt)
  = ((\ __cfReturnBlock_a5Gr
        -> CallFrame
             __cfClass_a5Go
             __cfMethod_a5Gp
             __cfReturnBlock_a5Gr
             __cfLocals_a5Gs
             __cfOpds_a5Gt)
     <$> (_f_a5Gn __cfReturnBlock'_a5Gq))
{-# INLINE cfReturnBlock #-}
-- src/Verifier/Java/Common.hs:1:1: Splicing declarations
--     makeLenses ''ErrorPath
--   ======>
--    src/Verifier/Java/Common.hs:444:1-22
epPath ::
  forall sbe_a3sx sbe_a5H2.
  Lens (ErrorPath sbe_a3sx) (ErrorPath sbe_a5H2) (Path sbe_a3sx) (Path sbe_a5H2)
epPath _f_a5H3 (EP __epRsn_a5H4 __epPath'_a5H5)
  = ((\ __epPath_a5H6 -> EP __epRsn_a5H4 __epPath_a5H6)
     <$> (_f_a5H3 __epPath'_a5H5))
{-# INLINE epPath #-}
epRsn :: forall sbe_a3sx. Lens' (ErrorPath sbe_a3sx) FailRsn
epRsn _f_a5H7 (EP __epRsn'_a5H8 __epPath_a5Ha)
  = ((\ __epRsn_a5H9 -> EP __epRsn_a5H9 __epPath_a5Ha)
     <$> (_f_a5H7 __epRsn'_a5H8))
{-# INLINE epRsn #-}
-- src/Verifier/Java/Common.hs:1:1: Splicing declarations
--     makeLenses ''JavaException
--   ======>
--    src/Verifier/Java/Common.hs:445:1-26
excRef :: forall term_a3ss. Lens' (JavaException term_a3ss) Ref
excRef _f_a5HA (JavaException __excRef'_a5HB __excStack_a5HD)
  = ((\ __excRef_a5HC -> JavaException __excRef_a5HC __excStack_a5HD)
     <$> (_f_a5HA __excRef'_a5HB))
{-# INLINE excRef #-}
excStack ::
  forall term_a3ss term_a5HE.
  Lens (JavaException term_a3ss) (JavaException term_a5HE) [CallFrame term_a3ss] [CallFrame term_a5HE]
excStack _f_a5HF (JavaException __excRef_a5HG __excStack'_a5HH)
  = ((\ __excStack_a5HI
        -> JavaException __excRef_a5HG __excStack_a5HI)
     <$> (_f_a5HF __excStack'_a5HH))
{-# INLINE excStack #-}
