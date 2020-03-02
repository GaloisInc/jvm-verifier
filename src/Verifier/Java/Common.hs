{- |
Module           : Verifier.Java.Common
Description      : Shared types and utility functions for JSS
License          : BSD3
Stability        : stable
Point-of-contact : acfoltzer
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  , err
  , throwSM
  , catchSM
  , printErrPaths
  , evHandlers
  , breakpoints
  , trBreakpoints

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
  , isResumed
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
  , getCurrentLineNumber

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
  , InternalExc(ErrorPathExc, InvalidType, UnknownExc)
  , SimulatorException(..)

    -- ** Event handlers and debug interface
  , SEH(..)
  , Breakpoint(..)
  , breakpointToPC
  , TransientBreakpoint(..)

    -- ** Pretty-printers
  , ppPath
  , ppMethod
  , ppValue
  , ppRef
  , ppCurrentPath
  , ppStackTrace
  , ppState
  , ppMemory
  , ppFailRsn
  , ppJavaException
  , ppInternalExc
  , ppBreakpoints
  , ppLocals

    -- * Control flow primitives
  , pushCallFrame
  , suspendCS
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

import Prelude hiding (EQ, GT, LT, (<>))
import qualified Prelude as P

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>), (<*>))
#endif
import Control.Arrow ((***))
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict hiding (State)
import qualified Control.Monad.Trans.State.Strict as Strict

import Data.Array (Array, Ix, assocs)
import qualified Data.Foldable as DF
import Data.Kind
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word (Word16, Word32)

import System.Console.Haskeline.MonadException (MonadException(..), RunIO(..))

import Text.PrettyPrint

import Language.JVM.Common (ppFldId, ppType)
import Data.JVM.Symbolic.AST
import Execution.JavaSemantics (AtomicValue(..), JSValue)

import Verifier.Java.Backend
import Verifier.Java.Codebase hiding (lookupClass)
import qualified Verifier.Java.Codebase as Codebase

-- | A Simulator is a monad transformer around a symbolic backend
newtype Simulator sbe (m :: Type -> Type) a =
  SM { runSM :: ExceptT (InternalExc sbe m) (StateT (State sbe m) IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , Fail.MonadFail
    , MonadIO
    , MonadThrow
    , MonadException
    )

instance MonadState (State sbe m) (Simulator sbe m) where
  get = SM (lift Strict.get)
  put = SM . lift . Strict.put

throwSM :: InternalExc sbe m -> Simulator sbe m a
throwSM = SM . throwE

catchSM :: Simulator sbe m a
        -> (InternalExc sbe m -> Simulator sbe m a)
        -> Simulator sbe m a
catchSM (SM m) h = SM (catchE m (runSM . h))

instance (MonadException m) => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'

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
  , _instanceOverrides :: !(Map (ClassName, MethodKey) (InstanceOverride sbe m))
    -- ^ Maps instance method identifiers to a function for executing them.
  , _staticOverrides   :: !(Map (ClassName, MethodKey) (StaticOverride sbe m))
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
  , _breakpoints       :: Map (ClassName, Method) (Set PC)
  , _trBreakpoints     :: S.Set TransientBreakpoint
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
                 , _breakpoints       = M.empty
                 , _trBreakpoints     = S.empty
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
  -- | @SuspCont rsn cs'@ represents a delimeter in the continuation stack
  -- introduced by some "unusual" manipulation of control flow. It
  -- represents the suspended control stack, along with a textual
  -- reason for its suspension. The common case for this is executing
  -- symbolic instructions for an override or to run class
  -- initialization logic.
  | SuspCont String (CS sbe)

-- | A control stack 'CS' is a stack of first-order continuations. It
-- represents either a computation with no work remaining, or a pair
-- of the current path and its continuation.
data CS sbe
  -- | A completed computation, potentially with a successful result path
  = CompletedCS (Maybe (Path sbe))
  -- | @ActiveCS p k@ is an active computation on path @p@ with the pending continuation @k@
  | ActiveCS (Path sbe) (SimCont sbe)
  -- | @ResumedCS p cs'@ represents a completed sub-computiation
  -- corresponding to the application of a @SuspCS@ continuation.
  | ResumedCS (Path sbe) (CS sbe)

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
               }
  return $ CompletedCS (Just p)

type PathDescriptor = Integer

data SimulationFlags =
  SimulationFlags { alwaysBitBlastBranchTerms :: Bool
                  , satAtBranches             :: Bool
                  }
  deriving Show

defaultSimFlags :: SimulationFlags
defaultSimFlags = SimulationFlags { alwaysBitBlastBranchTerms = False
                                  , satAtBranches             = False
                                  }

type Path sbe = Path' (SBETerm sbe)

data Path' term = Path {
    _pathStack          :: ![CallFrame term]
    -- ^ The current JVM call stack.
  , _pathStackHt        :: !Int
    -- ^ The current call frames count.
  , _pathBlockId        :: !(Maybe BlockId)
    -- ^ The currently-executing basic block on this path, if any.
  , _pathRetVal         :: !(Maybe (Value term))
    -- ^ The current return value, if this path has returned its last
    -- call frame.
  , _pathException      :: !(Maybe (JavaException term))
    -- ^ The exception thrown on this path, if any.
  , _pathMemory         :: !(Memory term)
    -- ^ The contents of the heap on the current path.
  , _pathAssertions     :: !term
    -- ^ The conditions necessary for the state of this path to be
    -- well-defined. Note that this is different from the typical notion
    -- of a path condition in symbolic execution! It does not include
    -- assumptions (e.g., branch conditions up to this point).
  , _pathName           :: !PathDescriptor
    -- ^ A unique name for this path.
  }

data Memory term = Memory {
    _memInitialization :: !(Map ClassName InitializationStatus)
    -- ^ The initialization status of classes.
  , _memStaticFields   :: !(Map FieldId (Value term))
    -- ^ The values of all static fields.
  , _memInstanceFields :: !(Map InstanceFieldRef (Value term))
    -- ^ The values of all instance fields.
  , _memScalarArrays   :: !(Map Ref (Int32, term))
    -- ^ The values of integer and long arrays (floating point not
    -- supported).
  , _memRefArrays      :: !(Map Ref (Array Int32 Ref))
    -- ^ The values of reference arrays.
  , _memClassObjects   :: !(Map ClassName Ref)
    -- ^ References pointing to java.lang.Class objects.
  }

emptyMemory :: Memory term
emptyMemory = Memory M.empty M.empty M.empty M.empty M.empty M.empty

-- | A JVM call frame
data CallFrame term
  = CallFrame {
      _cfClass       :: !ClassName
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
  | InvalidType ExpectedType ActualType
  | UnknownExc (Maybe FailRsn)

type ExpectedType = String
type ActualType = String

strExc :: String -> InternalExc sbe m
strExc = UnknownExc . Just . FailRsn

err :: String -> Simulator sbe m a
err = throwSM . strExc

-- | InternalExc is suitable only for internals; its parameterization
-- and inclusion State makes it inappropriate for Show or Exception
-- instances, so when those are needed, use SimulatorException
-- instead.
data SimulatorException = SimulatorException String deriving Show
instance Exception SimulatorException

-- | Types of breakpoints
data Breakpoint = BreakEntry | BreakPC PC | BreakLineNum Word16
  deriving (Eq, Ord, Show)

-- | Transient breakpoints for interactive debugging
data TransientBreakpoint = BreakNextInsn
                         -- ^ Break at the next instruction
                         | BreakReturnFrom Method
                         -- ^ Break when returning from a method
                         | BreakLineChange (Maybe Word16)
                         -- ^ Break when line number changes or becomes known
  deriving (Eq, Ord, Show)

-- | Simulation event handlers, useful for debugging nontrivial codes.
data SEH sbe m = SEH
  {
    -- | Invoked after function overrides have been registered
    onPostOverrideReg :: Simulator sbe m ()
    -- | Invoked before each instruction executes
  , onPreStep         :: Maybe PC -> SymInsn -> Simulator sbe m ()
    -- | Invoked after each instruction executes
  , onPostStep        :: Maybe PC -> SymInsn -> Simulator sbe m ()
  }

assert :: String -> Bool -> Simulator sbe m ()
assert msg b =
  unless b . throwSM . strExc $ "assertion failed (" ++ msg ++ ")"

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

isResumed :: CS sbe -> Bool
isResumed ResumedCS{} = True
isResumed _ = False

-- | For consistency with LSS api... probably not needed
modifyPath :: (Path sbe -> Path sbe) -> CS sbe -> Maybe (CS sbe)
modifyPath f cs =
  case cs of
    CompletedCS mp -> (CompletedCS . Just . f) <$> mp
    ResumedCS p cs' -> Just (ResumedCS (f p) cs')
    ActiveCS p k -> Just (ActiveCS (f p) k)

-- | Apply @f@ to the current path, if one exists
modifyCurrentPath :: (Path sbe -> Path sbe) -> CS sbe -> CS sbe
modifyCurrentPath f (CompletedCS mp)  = CompletedCS (f <$> mp)
modifyCurrentPath f (ResumedCS p cs') = ResumedCS (f p) cs'
modifyCurrentPath f (ActiveCS p k)    = ActiveCS (f p) k

-- | Modify current path in control stack, returning an extra value
modifyCurrentPathM :: forall m sbe a .
                      Functor m
                   => CS sbe
                   -> (Path sbe -> m (a,Path sbe))
                   -> Maybe (m (a,CS sbe))
modifyCurrentPathM cs f =
  case cs of
    CompletedCS mp -> (run (CompletedCS . Just)) <$> mp
    ResumedCS p cs' -> Just (run fn p)
      where fn p' = ResumedCS p' cs'
    ActiveCS p h -> Just (run fn p)
      where fn p' = ActiveCS p' h
 where run :: (Path sbe -> CS sbe) -> Path sbe -> m (a, CS sbe)
       run csfn = fmap (id *** csfn) . f

-- | Return the topmost path from a control stack, if any
currentPath :: CS sbe -> Maybe (Path sbe)
currentPath (CompletedCS mp) = mp
currentPath (ResumedCS p _)  = Just p
currentPath (ActiveCS p _)   = Just p

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
        CompletedCS Nothing -> err . render $ ctx <> ": no current paths"
        CompletedCS (Just p) -> do
                (x, p') <- f p
                return $ (x, CompletedCS (Just p'))
        ResumedCS p cs' -> do
                (x, p') <- f p
                return $ (x, ResumedCS p' cs')
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
    _      -> err . render $ ctx <> ":" <+> "no current path"

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
      [] -> err . render $ ctx <> ": no stack frames"
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
getCurrentClassName :: Simulator sbe m ClassName
getCurrentClassName =
  modifyCallFrameM "getCurrentClassName" $ \cf -> return (cf^.cfClass, cf)

-- | Return the method being executed by the current path
getCurrentMethod :: Simulator sbe m Method
getCurrentMethod =
  modifyCallFrameM "getCurrentClassName" $ \cf -> return (cf^.cfMethod, cf)

-- | Get the current line number, if there is one associated in the code
getCurrentLineNumber :: PC -> Simulator sbe m (Maybe Word16)
getCurrentLineNumber pc = do
  method <- getCurrentMethod
  return $ sourceLineNumberOrPrev method pc

-- | Resolve the given class in the simulator's current codebase
lookupClass :: ClassName -> Simulator sbe m Class
lookupClass cName = do
  cb <- use codebase
  mcl <- liftIO $ Codebase.tryLookupClass cb cName
  maybe (err . render $ "class not found:" <+> text (unClassName cName)) return mcl


-- | Push a new call frame to the current path, if any. Needs the
-- initial function arguments, and basic block (in the caller's
-- context) to return to once this method is finished.
pushCallFrame :: ClassName
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
      CompletedCS Nothing -> error "all paths failed"
      CompletedCS (Just p) -> Just $ ActiveCS (pushFrame p) EmptyCont
      _ -> return $ modifyCurrentPath pushFrame cs
  where pushFrame p = p'
          where cf = CallFrame clname method retBB locals []
                p' = p & pathStack   %~ (cf :)
                       & pathStackHt +~ 1
                       & pathBlockId .~ Just entryBlock

-- | Suspend the control stack with the given suspension reason.
suspendCS :: String -> CS sbe -> Maybe (CS sbe)
suspendCS rsn cs = do
  let suspend suspPath = ActiveCS p (SuspCont rsn cs)
        where p = suspPath & pathStack   .~ []
                           & pathStackHt .~ 0
  case cs of
    CompletedCS Nothing -> error "suspendCS: all paths failed"
    -- if we're already a finished computation, don't do anything fancy
    CompletedCS (Just _) -> return cs
    ActiveCS suspPath _ -> return $ suspend suspPath
    ResumedCS suspPath _ -> return $ suspend suspPath

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
      CompletedCS{} -> error "path is completed"
      ResumedCS{}   -> error "path is completed"
      ActiveCS p k  ->
          return . ActiveCS p $ HandleBranch point (BranchRunTrue c pt) k
        where point = case ml of
                       Just b -> PostdomPoint (p^.pathStackHt) b
                       Nothing -> ReturnPoint (p^.pathStackHt - 1)
              pt = p & pathBlockId .~ Just nb
                     & pathName    .~ nm

-- | Move current path to target block, checking for merge points.
jumpCurrentPath :: (Functor m, MonadIO m)
                => BlockId -> CS sbe -> Simulator sbe m (CS sbe)
jumpCurrentPath _ CompletedCS{}  = err "path is completed"
jumpCurrentPath _ ResumedCS{}    = err "path is completed"
jumpCurrentPath b (ActiveCS p k) =
  mergeNextCont (p & pathBlockId .~ Just b) k

-- | Return from current path, checking for merge points, and putting
-- the optional return value on the operand stack of the next frame.
returnCurrentPath :: forall sbe m . (Functor m, MonadIO m)
                  => Maybe (Value (SBETerm sbe))
                  -> CS sbe
                  -> Simulator sbe m (CS sbe)
returnCurrentPath _ CompletedCS{} = err "path is completed"
returnCurrentPath _ ResumedCS{}   = err "path is completed"
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
  CompletedCS{}                   -> err "path is completed"
  ResumedCS{}                     -> err "path is completed"
  ActiveCS _ (HandleBranch _ a k) -> branchError a k
  ActiveCS _ EmptyCont            -> return $ CompletedCS Nothing
  -- we're throwing away the rest of the control stack here, but that's ok
  ActiveCS _ (SuspCont _ _)       -> return $ CompletedCS Nothing

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
--   continuation is empty. This leaves us with a completed or resumed
--   control stack containing the current path.
--
--   1a. If we are resuming a parent computation, merge appropriate
--   fields from the path.
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
-- 1a.
mergeNextCont p (SuspCont _rsn cs') | 0 == p^.pathStackHt = do
  return $ ResumedCS p cs'
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
          _ -> throwSM $ strExc "call frame mismatch when merging paths"
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
    throwSM $ strExc "return value mismatch when merging paths"

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
        assert "mergeMemories lengths equal" (l1 == l2)
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
  assert "mergeCallFrames classes equal" (class1  == class2)
  assert "mergeCallFrames methods equal" (method1 == method2)
  -- pcs may differ if paths merge at different return insts
  mergedLocals <- mergeBy (mergeValues assertions) locals1 locals2
  assert "mergeCallFrames stack depths equal" (length opds1 == length opds2)
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
  let abort = err . render
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
        assert "mergeValues types equal" (ty1 == ty2)
        return x
      mergeV _ _ =
        abort $ "Unsupported or mismatched type when merging values:"
        <+> ppValue sbe x <+> "and" <+> ppValue sbe y
  mergeV x y

--------------------------------------------------------------------------------
-- Debugging support

breakpointToPC :: Method -> Breakpoint -> Maybe PC
breakpointToPC _      BreakEntry       = return 0
breakpointToPC _      (BreakPC pc)     = return pc
breakpointToPC method (BreakLineNum n) = lookupLineStartPC method n

--------------------------------------------------------------------------------
-- Pretty printers

ppJavaException :: JavaException term -> Doc
ppJavaException (JavaException (Ref _ (ClassType nm)) _) =
  "Exception of type" <+> text (slashesToDots (unClassName nm))
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
ppMethod method =
  text (methodKeyName (methodKey method)) <>
  text (unparseMethodDescriptor (methodKey method))

ppState :: State sbe m -> Doc
ppState s = hang (text "state" <+> lbrace) 2 (ppCtrlStk (s^.backend) (s^.ctrlStk)) $+$ rbrace

ppPath :: Backend sbe -> Path sbe -> Doc
ppPath sbe p =
  case currentCallFrame p of
    Just cf ->
      text "Path #"
      <>  integer (p^.pathName)
      <>  brackets ( text (unClassName (cf^.cfClass)) <> "." <> ppMethod (cf^.cfMethod)
                   <> "/" <> maybe "none" ppBlockId (p^.pathBlockId)
                   )
      <>  colon
      $+$ nest 2 (text "Locals:"   $+$ nest 2 (ppLocals sbe (cf^.cfLocals)))
      $+$ nest 2 (text "Operands:" $+$ nest 2 (ppOpds sbe (cf^.cfOpds)))
      $+$ nest 2 (text "Call Stack:" $+$ nest 2 (ppStackTrace (p^.pathStack)))
    Nothing ->
      text "Path #" <> integer (p^.pathName) <> colon <+> "stopped"

ppStackTrace :: [CallFrame term] -> Doc
ppStackTrace cfs = braces . vcat $
  [ text (unClassName cName) <> "." <> ppMethod method
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
              ppMap (text . unClassName) (text . show) (mem^.memInitialization)
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
              ppMap (text . unClassName) ppRef (mem^.memClassObjects)
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
  "Completed stack:" <+> maybe "All paths failed" (ppPath sbe) mp
ppCtrlStk sbe (ActiveCS p k) =
  "Active path:" $$ ppPath sbe p $$ ppSimCont sbe k
ppCtrlStk sbe (ResumedCS p cs') =
  "Resume stack:" <+> ppPath sbe p
  $$ nest 2 (ppCtrlStk sbe cs')

ppSimCont :: Backend sbe -> SimCont sbe -> Doc
ppSimCont sbe (HandleBranch point ba h) =
  "on" <+> ppMergePoint point <+> "do" $$
  nest 2 (ppBranchAction sbe ba) $$
  ppSimCont sbe h
ppSimCont _ EmptyCont = "stop"
ppSimCont _ (SuspCont rsn _cs) = "resume:" <+> text rsn

ppBranchAction :: Backend sbe -> BranchAction sbe -> Doc
ppBranchAction sbe (BranchRunTrue c p) =
  "runTrue" <+> prettyTermD sbe c $$ nest 2 (ppPath sbe p)
ppBranchAction sbe (BranchMerge a c p) =
  "mergeBranch" <+> prettyTermD sbe c $$
  nest 2 ("assumptions:" <+> prettyTermD sbe a) $$
  nest 2 (ppPath sbe p)

ppMergePoint :: MergePoint -> Doc
ppMergePoint (ReturnPoint n) = "return" <> parens (int n)
ppMergePoint (PostdomPoint n b) =
    "postdom" <> parens (int n <+> ppBlockId b)

dumpCtrlStk :: (MonadIO m) => Simulator sbe m ()
dumpCtrlStk = do
  (sbe, cs) <- (,) <$> use backend <*> use ctrlStk
  liftIO $ putStrLn $ replicate 80 '-'
  liftIO $ putStrLn $ show $ ppCtrlStk sbe cs
  liftIO $ putStrLn $ replicate 80 '-'

ppFailRsn :: FailRsn -> Doc
ppFailRsn (FailRsn msg) = text msg

setInitializationStatus :: ClassName
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
    ResumedCS p _        -> "completed subcomputation path"
                            <> colon <+> ppPath sbe p

dumpCurrentPath :: MonadSim sbe m => Simulator sbe m ()
dumpCurrentPath = do
  path <- ppCurrentPath
  liftIO . putStrLn . render $ path

ppInternalExc :: InternalExc sbe m -> Doc
ppInternalExc exc = case exc of
  ErrorPathExc rsn s ->
      "internal error" <> colon <+> ppFailRsn rsn <+> ppState s
  InvalidType wanted got ->
      "invalid type, wanted" <+> text wanted <+> "but got" <+> text got
  UnknownExc Nothing ->
      "unknown error"
  UnknownExc (Just rsn) ->
      "unknown error" <> colon <+> ppFailRsn rsn

ppBreakpoints :: Map (ClassName, Method) (Set PC) -> Doc
ppBreakpoints bps =
  hang "breakpoints set:" 2 . vcat $
    [ text (unClassName clName) <> "." <> ppMethod method <> "%" <> text (show pc)
    | ((clName, method), pcs) <- M.toList bps
    , pc <- S.toList pcs
    ]

--------------------------------------------------------------------------------
-- Manual lens implementations. These are captured by using
-- -ddump-splices output from makeLenses, but is reproduced here
-- statically in order to avoid having to link libabc at compile
-- time. Note that these will have to be regenerated whenever fields
-- are changed for the relevant types.

-- TODO: fix linking problems so we can do this with TH

{-# INLINE backend #-}
backend :: Simple Lens (State sbe m) (Backend sbe)
backend = lens _backend (\s v -> s { _backend = v })

-- src/Verifier/Java/Common.hs:1:1: Splicing declarations
--     makeLenses ''State
breakpoints :: Simple Lens (State sbe m) (Map (ClassName, Method) (Set PC))
breakpoints = lens _breakpoints (\s v -> s { _breakpoints = v })
{-# INLINE breakpoints #-}

codebase ::
  forall sbe_aDu4 m_aDu5. Lens' (State sbe_aDu4 m_aDu5) Codebase
codebase
  _f_aEnk
  (State __codebase'_aEnl
         __instanceOverrides_aEnn
         __staticOverrides_aEno
         __ctrlStk_aEnp
         __nextPSS_aEnq
         __strings_aEnr
         __nextRef_aEns
         __verbosity_aEnt
         __simulationFlags_aEnu
         __backend_aEnv
         __errorPaths_aEnw
         __printErrPaths_aEnx
         __breakpoints_aEny
         __trBreakpoints_aEnz
         __evHandlers_aEnA)
  = ((\ __codebase_aEnm
        -> State
             __codebase_aEnm
             __instanceOverrides_aEnn
             __staticOverrides_aEno
             __ctrlStk_aEnp
             __nextPSS_aEnq
             __strings_aEnr
             __nextRef_aEns
             __verbosity_aEnt
             __simulationFlags_aEnu
             __backend_aEnv
             __errorPaths_aEnw
             __printErrPaths_aEnx
             __breakpoints_aEny
             __trBreakpoints_aEnz
             __evHandlers_aEnA)
     <$> (_f_aEnk __codebase'_aEnl))
{-# INLINE codebase #-}
ctrlStk ::
  forall sbe_aDu4 m_aDu5. Lens' (State sbe_aDu4 m_aDu5) (CS sbe_aDu4)
ctrlStk
  _f_aEnB
  (State __codebase_aEnC
         __instanceOverrides_aEnD
         __staticOverrides_aEnE
         __ctrlStk'_aEnF
         __nextPSS_aEnH
         __strings_aEnI
         __nextRef_aEnJ
         __verbosity_aEnK
         __simulationFlags_aEnL
         __backend_aEnM
         __errorPaths_aEnN
         __printErrPaths_aEnO
         __breakpoints_aEnP
         __trBreakpoints_aEnQ
         __evHandlers_aEnR)
  = ((\ __ctrlStk_aEnG
        -> State
             __codebase_aEnC
             __instanceOverrides_aEnD
             __staticOverrides_aEnE
             __ctrlStk_aEnG
             __nextPSS_aEnH
             __strings_aEnI
             __nextRef_aEnJ
             __verbosity_aEnK
             __simulationFlags_aEnL
             __backend_aEnM
             __errorPaths_aEnN
             __printErrPaths_aEnO
             __breakpoints_aEnP
             __trBreakpoints_aEnQ
             __evHandlers_aEnR)
     <$> (_f_aEnB __ctrlStk'_aEnF))
{-# INLINE ctrlStk #-}
errorPaths ::
  forall sbe_aDu4 m_aDu5.
  Lens' (State sbe_aDu4 m_aDu5) [ErrorPath sbe_aDu4]
errorPaths
  _f_aEnS
  (State __codebase_aEnT
         __instanceOverrides_aEnU
         __staticOverrides_aEnV
         __ctrlStk_aEnW
         __nextPSS_aEnX
         __strings_aEnY
         __nextRef_aEnZ
         __verbosity_aEo0
         __simulationFlags_aEo1
         __backend_aEo2
         __errorPaths'_aEo3
         __printErrPaths_aEo5
         __breakpoints_aEo6
         __trBreakpoints_aEo7
         __evHandlers_aEo8)
  = ((\ __errorPaths_aEo4
        -> State
             __codebase_aEnT
             __instanceOverrides_aEnU
             __staticOverrides_aEnV
             __ctrlStk_aEnW
             __nextPSS_aEnX
             __strings_aEnY
             __nextRef_aEnZ
             __verbosity_aEo0
             __simulationFlags_aEo1
             __backend_aEo2
             __errorPaths_aEo4
             __printErrPaths_aEo5
             __breakpoints_aEo6
             __trBreakpoints_aEo7
             __evHandlers_aEo8)
     <$> (_f_aEnS __errorPaths'_aEo3))
{-# INLINE errorPaths #-}
evHandlers ::
  forall sbe_aDu4 m_aDu5.
  Lens' (State sbe_aDu4 m_aDu5) (SEH sbe_aDu4 m_aDu5)
evHandlers
  _f_aEo9
  (State __codebase_aEoa
         __instanceOverrides_aEob
         __staticOverrides_aEoc
         __ctrlStk_aEod
         __nextPSS_aEoe
         __strings_aEof
         __nextRef_aEog
         __verbosity_aEoh
         __simulationFlags_aEoi
         __backend_aEoj
         __errorPaths_aEok
         __printErrPaths_aEol
         __breakpoints_aEom
         __trBreakpoints_aEon
         __evHandlers'_aEoo)
  = ((\ __evHandlers_aEop
        -> State
             __codebase_aEoa
             __instanceOverrides_aEob
             __staticOverrides_aEoc
             __ctrlStk_aEod
             __nextPSS_aEoe
             __strings_aEof
             __nextRef_aEog
             __verbosity_aEoh
             __simulationFlags_aEoi
             __backend_aEoj
             __errorPaths_aEok
             __printErrPaths_aEol
             __breakpoints_aEom
             __trBreakpoints_aEon
             __evHandlers_aEop)
     <$> (_f_aEo9 __evHandlers'_aEoo))
{-# INLINE evHandlers #-}
instanceOverrides ::
  forall sbe_aDu4 m_aDu5.
  Lens' (State sbe_aDu4 m_aDu5) (Map (ClassName,
                                      MethodKey) (InstanceOverride sbe_aDu4 m_aDu5))
instanceOverrides
  _f_aEoq
  (State __codebase_aEor
         __instanceOverrides'_aEos
         __staticOverrides_aEou
         __ctrlStk_aEov
         __nextPSS_aEow
         __strings_aEox
         __nextRef_aEoy
         __verbosity_aEoz
         __simulationFlags_aEoA
         __backend_aEoB
         __errorPaths_aEoC
         __printErrPaths_aEoD
         __breakpoints_aEoE
         __trBreakpoints_aEoF
         __evHandlers_aEoG)
  = ((\ __instanceOverrides_aEot
        -> State
             __codebase_aEor
             __instanceOverrides_aEot
             __staticOverrides_aEou
             __ctrlStk_aEov
             __nextPSS_aEow
             __strings_aEox
             __nextRef_aEoy
             __verbosity_aEoz
             __simulationFlags_aEoA
             __backend_aEoB
             __errorPaths_aEoC
             __printErrPaths_aEoD
             __breakpoints_aEoE
             __trBreakpoints_aEoF
             __evHandlers_aEoG)
     <$> (_f_aEoq __instanceOverrides'_aEos))
{-# INLINE instanceOverrides #-}
nextPSS ::
  forall sbe_aDu4 m_aDu5.
  Lens' (State sbe_aDu4 m_aDu5) PathDescriptor
nextPSS
  _f_aEoH
  (State __codebase_aEoI
         __instanceOverrides_aEoJ
         __staticOverrides_aEoK
         __ctrlStk_aEoL
         __nextPSS'_aEoM
         __strings_aEoO
         __nextRef_aEoP
         __verbosity_aEoQ
         __simulationFlags_aEoR
         __backend_aEoS
         __errorPaths_aEoT
         __printErrPaths_aEoU
         __breakpoints_aEoV
         __trBreakpoints_aEoW
         __evHandlers_aEoX)
  = ((\ __nextPSS_aEoN
        -> State
             __codebase_aEoI
             __instanceOverrides_aEoJ
             __staticOverrides_aEoK
             __ctrlStk_aEoL
             __nextPSS_aEoN
             __strings_aEoO
             __nextRef_aEoP
             __verbosity_aEoQ
             __simulationFlags_aEoR
             __backend_aEoS
             __errorPaths_aEoT
             __printErrPaths_aEoU
             __breakpoints_aEoV
             __trBreakpoints_aEoW
             __evHandlers_aEoX)
     <$> (_f_aEoH __nextPSS'_aEoM))
{-# INLINE nextPSS #-}
nextRef ::
  forall sbe_aDu4 m_aDu5. Lens' (State sbe_aDu4 m_aDu5) Word32
nextRef
  _f_aEoY
  (State __codebase_aEoZ
         __instanceOverrides_aEp0
         __staticOverrides_aEp1
         __ctrlStk_aEp2
         __nextPSS_aEp3
         __strings_aEp4
         __nextRef'_aEp5
         __verbosity_aEp7
         __simulationFlags_aEp8
         __backend_aEp9
         __errorPaths_aEpa
         __printErrPaths_aEpb
         __breakpoints_aEpc
         __trBreakpoints_aEpd
         __evHandlers_aEpe)
  = ((\ __nextRef_aEp6
        -> State
             __codebase_aEoZ
             __instanceOverrides_aEp0
             __staticOverrides_aEp1
             __ctrlStk_aEp2
             __nextPSS_aEp3
             __strings_aEp4
             __nextRef_aEp6
             __verbosity_aEp7
             __simulationFlags_aEp8
             __backend_aEp9
             __errorPaths_aEpa
             __printErrPaths_aEpb
             __breakpoints_aEpc
             __trBreakpoints_aEpd
             __evHandlers_aEpe)
     <$> (_f_aEoY __nextRef'_aEp5))
{-# INLINE nextRef #-}
printErrPaths ::
  forall sbe_aDu4 m_aDu5. Lens' (State sbe_aDu4 m_aDu5) Bool
printErrPaths
  _f_aEpf
  (State __codebase_aEpg
         __instanceOverrides_aEph
         __staticOverrides_aEpi
         __ctrlStk_aEpj
         __nextPSS_aEpk
         __strings_aEpl
         __nextRef_aEpm
         __verbosity_aEpn
         __simulationFlags_aEpo
         __backend_aEpp
         __errorPaths_aEpq
         __printErrPaths'_aEpr
         __breakpoints_aEpt
         __trBreakpoints_aEpu
         __evHandlers_aEpv)
  = ((\ __printErrPaths_aEps
        -> State
             __codebase_aEpg
             __instanceOverrides_aEph
             __staticOverrides_aEpi
             __ctrlStk_aEpj
             __nextPSS_aEpk
             __strings_aEpl
             __nextRef_aEpm
             __verbosity_aEpn
             __simulationFlags_aEpo
             __backend_aEpp
             __errorPaths_aEpq
             __printErrPaths_aEps
             __breakpoints_aEpt
             __trBreakpoints_aEpu
             __evHandlers_aEpv)
     <$> (_f_aEpf __printErrPaths'_aEpr))
{-# INLINE printErrPaths #-}
simulationFlags ::
  forall sbe_aDu4 m_aDu5.
  Lens' (State sbe_aDu4 m_aDu5) SimulationFlags
simulationFlags
  _f_aEpw
  (State __codebase_aEpx
         __instanceOverrides_aEpy
         __staticOverrides_aEpz
         __ctrlStk_aEpA
         __nextPSS_aEpB
         __strings_aEpC
         __nextRef_aEpD
         __verbosity_aEpE
         __simulationFlags'_aEpF
         __backend_aEpH
         __errorPaths_aEpI
         __printErrPaths_aEpJ
         __breakpoints_aEpK
         __trBreakpoints_aEpL
         __evHandlers_aEpM)
  = ((\ __simulationFlags_aEpG
        -> State
             __codebase_aEpx
             __instanceOverrides_aEpy
             __staticOverrides_aEpz
             __ctrlStk_aEpA
             __nextPSS_aEpB
             __strings_aEpC
             __nextRef_aEpD
             __verbosity_aEpE
             __simulationFlags_aEpG
             __backend_aEpH
             __errorPaths_aEpI
             __printErrPaths_aEpJ
             __breakpoints_aEpK
             __trBreakpoints_aEpL
             __evHandlers_aEpM)
     <$> (_f_aEpw __simulationFlags'_aEpF))
{-# INLINE simulationFlags #-}
staticOverrides ::
  forall sbe_aDu4 m_aDu5.
  Lens' (State sbe_aDu4 m_aDu5) (Map (ClassName,
                                      MethodKey) (StaticOverride sbe_aDu4 m_aDu5))
staticOverrides
  _f_aEpN
  (State __codebase_aEpO
         __instanceOverrides_aEpP
         __staticOverrides'_aEpQ
         __ctrlStk_aEpS
         __nextPSS_aEpT
         __strings_aEpU
         __nextRef_aEpV
         __verbosity_aEpW
         __simulationFlags_aEpX
         __backend_aEpY
         __errorPaths_aEpZ
         __printErrPaths_aEq0
         __breakpoints_aEq1
         __trBreakpoints_aEq2
         __evHandlers_aEq3)
  = ((\ __staticOverrides_aEpR
        -> State
             __codebase_aEpO
             __instanceOverrides_aEpP
             __staticOverrides_aEpR
             __ctrlStk_aEpS
             __nextPSS_aEpT
             __strings_aEpU
             __nextRef_aEpV
             __verbosity_aEpW
             __simulationFlags_aEpX
             __backend_aEpY
             __errorPaths_aEpZ
             __printErrPaths_aEq0
             __breakpoints_aEq1
             __trBreakpoints_aEq2
             __evHandlers_aEq3)
     <$> (_f_aEpN __staticOverrides'_aEpQ))
{-# INLINE staticOverrides #-}
strings ::
  forall sbe_aDu4 m_aDu5.
  Lens' (State sbe_aDu4 m_aDu5) (Map String Ref)
strings
  _f_aEq4
  (State __codebase_aEq5
         __instanceOverrides_aEq6
         __staticOverrides_aEq7
         __ctrlStk_aEq8
         __nextPSS_aEq9
         __strings'_aEqa
         __nextRef_aEqc
         __verbosity_aEqd
         __simulationFlags_aEqe
         __backend_aEqf
         __errorPaths_aEqg
         __printErrPaths_aEqh
         __breakpoints_aEqi
         __trBreakpoints_aEqj
         __evHandlers_aEqk)
  = ((\ __strings_aEqb
        -> State
             __codebase_aEq5
             __instanceOverrides_aEq6
             __staticOverrides_aEq7
             __ctrlStk_aEq8
             __nextPSS_aEq9
             __strings_aEqb
             __nextRef_aEqc
             __verbosity_aEqd
             __simulationFlags_aEqe
             __backend_aEqf
             __errorPaths_aEqg
             __printErrPaths_aEqh
             __breakpoints_aEqi
             __trBreakpoints_aEqj
             __evHandlers_aEqk)
     <$> (_f_aEq4 __strings'_aEqa))
{-# INLINE strings #-}
trBreakpoints ::
  forall sbe_aDu4 m_aDu5.
  Lens' (State sbe_aDu4 m_aDu5) (Set TransientBreakpoint)
trBreakpoints
  _f_aEql
  (State __codebase_aEqm
         __instanceOverrides_aEqn
         __staticOverrides_aEqo
         __ctrlStk_aEqp
         __nextPSS_aEqq
         __strings_aEqr
         __nextRef_aEqs
         __verbosity_aEqt
         __simulationFlags_aEqu
         __backend_aEqv
         __errorPaths_aEqw
         __printErrPaths_aEqx
         __breakpoints_aEqy
         __trBreakpoints'_aEqz
         __evHandlers_aEqB)
  = ((\ __trBreakpoints_aEqA
        -> State
             __codebase_aEqm
             __instanceOverrides_aEqn
             __staticOverrides_aEqo
             __ctrlStk_aEqp
             __nextPSS_aEqq
             __strings_aEqr
             __nextRef_aEqs
             __verbosity_aEqt
             __simulationFlags_aEqu
             __backend_aEqv
             __errorPaths_aEqw
             __printErrPaths_aEqx
             __breakpoints_aEqy
             __trBreakpoints_aEqA
             __evHandlers_aEqB)
     <$> (_f_aEql __trBreakpoints'_aEqz))
{-# INLINE trBreakpoints #-}
verbosity ::
  forall sbe_aDu4 m_aDu5. Lens' (State sbe_aDu4 m_aDu5) Int
verbosity
  _f_aEqC
  (State __codebase_aEqD
         __instanceOverrides_aEqE
         __staticOverrides_aEqF
         __ctrlStk_aEqG
         __nextPSS_aEqH
         __strings_aEqI
         __nextRef_aEqJ
         __verbosity'_aEqK
         __simulationFlags_aEqM
         __backend_aEqN
         __errorPaths_aEqO
         __printErrPaths_aEqP
         __breakpoints_aEqQ
         __trBreakpoints_aEqR
         __evHandlers_aEqS)
  = ((\ __verbosity_aEqL
        -> State
             __codebase_aEqD
             __instanceOverrides_aEqE
             __staticOverrides_aEqF
             __ctrlStk_aEqG
             __nextPSS_aEqH
             __strings_aEqI
             __nextRef_aEqJ
             __verbosity_aEqL
             __simulationFlags_aEqM
             __backend_aEqN
             __errorPaths_aEqO
             __printErrPaths_aEqP
             __breakpoints_aEqQ
             __trBreakpoints_aEqR
             __evHandlers_aEqS)
     <$> (_f_aEqC __verbosity'_aEqK))
{-# INLINE verbosity #-}

{-# INLINE pathAssertions #-}
pathAssertions :: Simple Lens (Path' term) term
pathAssertions = lens _pathAssertions (\s v -> s { _pathAssertions = v })

-- src/Verifier/Java/Common.hs:1:1: Splicing declarations
--     makeLenses ''Path'
--   ======>
--     src/Verifier/Java/Common.hs:441:1-18
pathBlockId ::
  forall term_aaA1u. Lens' (Path' term_aaA1u) (Maybe BlockId)
pathBlockId
  _f_aaAcO
  (Path __pathStack_aaAcP
        __pathStackHt_aaAcQ
        __pathBlockId'_aaAcR
        __pathRetVal_aaAcT
        __pathException_aaAcU
        __pathMemory_aaAcV
        __pathAssertions_aaAcW
        __pathName_aaAcX)
  = ((\ __pathBlockId_aaAcS
        -> Path
             __pathStack_aaAcP
             __pathStackHt_aaAcQ
             __pathBlockId_aaAcS
             __pathRetVal_aaAcT
             __pathException_aaAcU
             __pathMemory_aaAcV
             __pathAssertions_aaAcW
             __pathName_aaAcX)
     <$> (_f_aaAcO __pathBlockId'_aaAcR))
{-# INLINE pathBlockId #-}
pathException ::
  forall term_aaA1u.
  Lens' (Path' term_aaA1u) (Maybe (JavaException term_aaA1u))
pathException
  _f_aaAcY
  (Path __pathStack_aaAcZ
        __pathStackHt_aaAd0
        __pathBlockId_aaAd1
        __pathRetVal_aaAd2
        __pathException'_aaAd3
        __pathMemory_aaAd5
        __pathAssertions_aaAd6
        __pathName_aaAd7)
  = ((\ __pathException_aaAd4
        -> Path
             __pathStack_aaAcZ
             __pathStackHt_aaAd0
             __pathBlockId_aaAd1
             __pathRetVal_aaAd2
             __pathException_aaAd4
             __pathMemory_aaAd5
             __pathAssertions_aaAd6
             __pathName_aaAd7)
     <$> (_f_aaAcY __pathException'_aaAd3))
{-# INLINE pathException #-}
pathMemory ::
  forall term_aaA1u. Lens' (Path' term_aaA1u) (Memory term_aaA1u)
pathMemory
  _f_aaAd8
  (Path __pathStack_aaAd9
        __pathStackHt_aaAda
        __pathBlockId_aaAdb
        __pathRetVal_aaAdc
        __pathException_aaAdd
        __pathMemory'_aaAde
        __pathAssertions_aaAdg
        __pathName_aaAdh)
  = ((\ __pathMemory_aaAdf
        -> Path
             __pathStack_aaAd9
             __pathStackHt_aaAda
             __pathBlockId_aaAdb
             __pathRetVal_aaAdc
             __pathException_aaAdd
             __pathMemory_aaAdf
             __pathAssertions_aaAdg
             __pathName_aaAdh)
     <$> (_f_aaAd8 __pathMemory'_aaAde))
{-# INLINE pathMemory #-}
pathName ::
  forall term_aaA1u. Lens' (Path' term_aaA1u) PathDescriptor
pathName
  _f_aaAdi
  (Path __pathStack_aaAdj
        __pathStackHt_aaAdk
        __pathBlockId_aaAdl
        __pathRetVal_aaAdm
        __pathException_aaAdn
        __pathMemory_aaAdo
        __pathAssertions_aaAdp
        __pathName'_aaAdq)
  = ((\ __pathName_aaAdr
        -> Path
             __pathStack_aaAdj
             __pathStackHt_aaAdk
             __pathBlockId_aaAdl
             __pathRetVal_aaAdm
             __pathException_aaAdn
             __pathMemory_aaAdo
             __pathAssertions_aaAdp
             __pathName_aaAdr)
     <$> (_f_aaAdi __pathName'_aaAdq))
{-# INLINE pathName #-}
pathRetVal ::
  forall term_aaA1u.
  Lens' (Path' term_aaA1u) (Maybe (Value term_aaA1u))
pathRetVal
  _f_aaAds
  (Path __pathStack_aaAdt
        __pathStackHt_aaAdu
        __pathBlockId_aaAdv
        __pathRetVal'_aaAdw
        __pathException_aaAdy
        __pathMemory_aaAdz
        __pathAssertions_aaAdA
        __pathName_aaAdB)
  = ((\ __pathRetVal_aaAdx
        -> Path
             __pathStack_aaAdt
             __pathStackHt_aaAdu
             __pathBlockId_aaAdv
             __pathRetVal_aaAdx
             __pathException_aaAdy
             __pathMemory_aaAdz
             __pathAssertions_aaAdA
             __pathName_aaAdB)
     <$> (_f_aaAds __pathRetVal'_aaAdw))
{-# INLINE pathRetVal #-}
pathStack ::
  forall term_aaA1u. Lens' (Path' term_aaA1u) [CallFrame term_aaA1u]
pathStack
  _f_aaAdC
  (Path __pathStack'_aaAdD
        __pathStackHt_aaAdF
        __pathBlockId_aaAdG
        __pathRetVal_aaAdH
        __pathException_aaAdI
        __pathMemory_aaAdJ
        __pathAssertions_aaAdK
        __pathName_aaAdL)
  = ((\ __pathStack_aaAdE
        -> Path
             __pathStack_aaAdE
             __pathStackHt_aaAdF
             __pathBlockId_aaAdG
             __pathRetVal_aaAdH
             __pathException_aaAdI
             __pathMemory_aaAdJ
             __pathAssertions_aaAdK
             __pathName_aaAdL)
     <$> (_f_aaAdC __pathStack'_aaAdD))
{-# INLINE pathStack #-}
pathStackHt :: forall term_aaA1u. Lens' (Path' term_aaA1u) Int
pathStackHt
  _f_aaAdM
  (Path __pathStack_aaAdN
        __pathStackHt'_aaAdO
        __pathBlockId_aaAdQ
        __pathRetVal_aaAdR
        __pathException_aaAdS
        __pathMemory_aaAdT
        __pathAssertions_aaAdU
        __pathName_aaAdV)
  = ((\ __pathStackHt_aaAdP
        -> Path
             __pathStack_aaAdN
             __pathStackHt_aaAdP
             __pathBlockId_aaAdQ
             __pathRetVal_aaAdR
             __pathException_aaAdS
             __pathMemory_aaAdT
             __pathAssertions_aaAdU
             __pathName_aaAdV)
     <$> (_f_aaAdM __pathStackHt'_aaAdO))
{-# INLINE pathStackHt #-}
-- src/Verifier/Java/Common.hs:1:1: Splicing declarations
--     makeLenses ''Memory
--   ======>
-- src/Verifier/Java/Common.hs:442:1-19
memClassObjects ::
  forall term_a3sz. Lens' (Memory term_a3sz) (Map ClassName Ref)
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
  Lens' (Memory term_a3sz) (Map ClassName InitializationStatus)
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
cfClass :: forall term_a3sy. Lens' (CallFrame term_a3sy) ClassName
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
