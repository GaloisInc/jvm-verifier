{- |
Module           : $Header$
Description      : Shared types and utility functions for JSS
Stability        : stable
Point-of-contact : acfoltzer
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Verifier.Java.Common
  ( Simulator(SM)
  , runSM
  , dumpCtrlStk

  -- , LSSOpts(LSSOpts, optsErrorPathDetails)
  -- , defaultLSSOpts

  , State(State)
  , codebase
  , backend
  , ctrlStk
  -- , fnOverrides
  , verbosity
  -- , evHandlers
  , errorPaths
  -- , lssOpts
  -- , pathCounter
  -- , aigOutputs
  , ppState
  , modifyCS

  , CS
  , initialCtrlStk
  -- , isFinished
  -- , getCurrentPath
  -- , modifyPath
  -- , modifyCurrentPathM
  -- , pushCallFrame
  -- , addCtrlBranch
  -- , jumpCurrentPath
  -- , returnCurrentPath
  -- , markCurrentPathAsError


  , BlockId

  , Path
  , Path'
  -- , pathFuncSym
  -- , pathCB
  -- , pathName
  -- , pathRegs
  -- , pathMem
  -- , pathAssertions
  -- , addPathAssertion
  -- , ppPath
  -- , ppPathLoc

  , CallFrame
  , currentCallFrame
  , ppCallFrameSrcLoc  

  , FailRsn(FailRsn)
  -- , ppFailRsn

  , JavaException
  , ppJavaException

  -- , ErrorPath(EP, epRsn, epPath)
  , InternalExc(ErrorPathExc, UnknownExc)
  -- , SEH( SEH
  --      , onPostOverrideReg
  --      , onPreStep
  --      , onPostStep
  --      , onMkGlobTerm
  --      , onPreGlobInit
  --      , onPostGlobInit
  --      )
  -- , ppTuple
  ) where

import Prelude hiding (EQ, GT, LT)
import qualified Prelude as P

import Control.Applicative (Applicative, (<$>), pure, (<*>))
import qualified Control.Arrow as CA
import Control.Exception (Exception)
import Control.Lens hiding (Path)
import Control.Monad.Error
import Control.Monad.State hiding (State)

import Data.Array (Array, elems)
import qualified Data.Foldable as DF
import Data.Int (Int32)
import Data.List (intercalate)
import Data.List.Lens
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable)
import Data.Word (Word32)

import Text.PrettyPrint

import Language.JVM.Common (ppType)
import Data.JVM.Symbolic.AST
import Execution (AtomicValue(..), JSValue)
import Verifier.Java.Backend
import Verifier.Java.Codebase 
import Verifier.Java.Utils

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
    -- ^ Auxiliary data structures for tracking execution and merging of
    -- multiple paths within a single frame.  Currently, there is a 1-1
    -- correspondence between each MergeFrame and its corresponding CallFrame (i.e.,
    -- the Java activation record).
  , _nextPSS           :: PathDescriptor
    -- ^ Name supply for unique path state selectors
  , _strings           :: !(Map String Ref)
  , _nextRef           :: !Word32 -- ^ Next index for constant ref.
  , _verbosity         :: Int
  , _simulationFlags   :: SimulationFlags
  , _backend           :: Backend sbe
  , _errorPaths        :: [ErrorPath sbe]
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
  -- | Continuation after finishing a path for the @else@ of a branch;
  -- starts running the @then@ branch
  = BranchRunTrue (SBETerm sbe) -- ^ Branch condition
                  (Path sbe)      -- ^ Path to run for @then@ branch
  -- | Continuation after finishing a path for the @then@ of a branch;
  -- merges with finished @else@ path
  | BranchMerge   (SBETerm sbe) -- ^ Assertions before merge
                  (SBETerm sbe) -- ^ Branch condition
                  (Path sbe)      -- ^ Completed @else@ branch

-- | First-order continuations for the symbolic simulation.
data SimCont sbe
  -- | Empty continuation: there are no remaining paths to run
  = EmptyCont
  -- | Handle part of a branch, then continue
  | HandleBranch MergePoint         -- ^ merge point of this branch
                 (BranchAction sbe) -- ^ action to take at merge point
                 (SimCont sbe)      -- ^ next continuation

-- | A control stack 'CS' is a stack of first-order continuations. It
-- represents either a computation with no work remaining, or a pair
-- of the current path and its continuation.
data CS sbe 
  -- | A completed computation, potentially with a successful result path
  = CompletedCS (Maybe (Path sbe))
  -- | An active computation with remaining continuations
  | ActiveCS (Path sbe)    -- ^ The active path
             (SimCont sbe) -- ^ Continuation once current path finishes

initialCtrlStk :: Backend sbe -> IO (CS sbe)
initialCtrlStk sbe = do
  true <- termBool sbe True
  let p = Path { _pathStack = []
               , _pathStackHt = 0
               , _pathException = Nothing
               , _pathMemory = emptyMemory
               , _pathAssertions = true
               , _pathName = 0
               , _pathStartingPC = 0
               , _pathBreakpoints = S.empty
               , _pathInsnCount = 0
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
  , _memScalarArrays   :: !(Map Ref (term, term))
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
      _cfClass   :: !String                               
      -- ^ Name of the class containing the current method
    , _cfMethod  :: !Method
      -- ^ The current method
    , _cfBlockId :: !BlockId
      -- ^ The current basic block
    , _cfPC      :: !PC
      -- ^ The current program counter
    , _cfLocals  :: !(Map LocalVariableIndex (Value term)) 
      -- ^ The current local variables (<http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-2.html#jvms-2.6.1>)
    , _cfOpds    :: ![Value term]                          
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

makeLenses ''State
makeLenses ''Path'
makeLenses ''Memory
makeLenses ''CallFrame
makeLenses ''ErrorPath
makeLenses ''JavaException

-- | Manipulate the control stack
modifyCS :: (CS sbe -> CS sbe) -> (State sbe m -> State sbe m)
modifyCS = over ctrlStk

-- | Get top call frame from a path
currentCallFrame :: Path' term -> Maybe (CallFrame term)
currentCallFrame p = p^.pathStack^?_head

-- | Get operand stack of top call frame from a path
currentOpds :: Path' term -> Maybe [Value term]
currentOpds p = view cfOpds <$> currentCallFrame p

-- | Called at symbolic return instructions and jumps to new basic
-- blocks to check whether it is time to merge a path and move on to
-- the next continuation. There are three cases:
-- 
--   1. There are no more call frames on the stack, and the
--   continuation is empty. This leaves us with a completed control
--   stack containing the current path.
-- 
--   2. We've reaced the current path's 'MergePoint' , so the current
--   continuation is complete. Depending on the type of continuation,
--   we either move on to a different path, or merge the current path
--   with an already-finished path before continuing.
-- 
--   3. The current path's merge point does not indicate the current
--   location, so we continue with the same path and continuation.
mergeNextCont :: MonadIO m
              => Backend sbe
              -> Path sbe
              -> SimCont sbe
              -> Simulator sbe m (CS sbe)
-- 1.
mergeNextCont _ p EmptyCont | 0 == p^.pathStackHt =
  return (CompletedCS (Just p))
-- 2.
mergeNextCont sbe p (HandleBranch point ba h) 
  | p `atMergePoint` point =
  case ba of
    BranchRunTrue c tp -> do
      true <- liftIO $ termBool sbe True
      let tp' = tp & pathAssertions .~ true
      let ba' = BranchMerge (tp^.pathAssertions) c p
      return $ ActiveCS tp' (HandleBranch point ba' h) 
    BranchMerge a c pf -> do
      let assertions = p^.pathAssertions 
          (Just cf1, Just cf2) = (currentCallFrame p, currentCallFrame pf)
      mergedCallFrame <- mergeCallFrames (p^.pathAssertions) cf1 cf2
      mergedMemory <- mergeMemories assertions (p^.pathMemory) (pf^.pathMemory)
      mergedAssertions <- 
          liftIO $ termIte sbe c (p^.pathAssertions) (pf^.pathAssertions)
      a' <- liftIO $ termAnd sbe a mergedAssertions
      let p' = p & pathStack._head .~ mergedCallFrame
                 & pathMemory      .~ mergedMemory
                 & pathAssertions  .~ a'
      -- recur in case multiple continuations have the same merge point
      mergeNextCont sbe p' h
-- 3.
mergeNextCont _ p h = return (ActiveCS p h)

-- | Is the given path at its 'MergePoint'?
atMergePoint :: Path' term -> MergePoint -> Bool
p `atMergePoint` point = case point of
  ReturnPoint n -> n == p^.pathStackHt
  PostdomPoint n b -> 
    n == p^.pathStackHt && Just b == (view cfBlockId <$> currentCallFrame p)

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
      mergeTup (i1, v1) (i2, v2) = 
        (,) <$> (liftIO $ termIte sbe assertions i1 i2)
            <*> (liftIO $ termIte sbe assertions v1 v2)
  mergedSFields <- mergeBy (mergeValues sbe assertions) sFields1 sFields2
  mergedIFields <- mergeBy (mergeValues sbe assertions) iFields1 iFields2
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
  sbe <- use backend
  let CallFrame class1 method1 _bb1 _pc1 locals1 opds1 = cf1
      CallFrame class2 method2 _bb2 _pc2 locals2 opds2 = cf2
  assert (class1  == class2)
  assert (method1 == method2)
  -- pcs may differ if paths merge at different return insts
  mergedLocals <- mergeBy (mergeValues sbe assertions) locals1 locals2
  mergedOpds <- zipWithM (mergeValues sbe assertions) opds1 opds2
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
            => Backend sbe
            -> SBETerm sbe 
            -> Value (SBETerm sbe) 
            -> Value (SBETerm sbe) 
            -> Simulator sbe m (Value (SBETerm sbe)) 
mergeValues sbe assertions x y = mergeV x y
  where abort = fail . render
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

--------------------------------------------------------------------------------
-- Pretty printers

ppStk :: [CallFrame term] -> Doc
ppStk [] = text "(empty stack)"
ppStk fs = vcat . map ppCallFrameSrcLoc $ fs

ppJavaException :: JavaException term -> Doc
ppJavaException (JavaException (Ref _ (ClassType nm)) frms) =
  "Exception of type" <+> text (slashesToDots nm)
  $+$ ppStk frms
ppJavaException (JavaException r frms) =
  "Unknown exception type" <+> ppRef r
  $+$ ppStk frms

ppCallFrameSrcLoc :: CallFrame term -> Doc
ppCallFrameSrcLoc (CallFrame c me _bb pc _vars _stack) =
  text c <> text "." <> text mn <> parens (text c <> text ".java:" <> text (maybe "?" show mloc))
  where
    (mloc, mn) = (`sourceLineNumberOrPrev` pc) CA.&&& methodName $ me

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

instance LogMonad (Simulator sbe m) where
  getVerbosity = use verbosity
  setVerbosity = assign verbosity

ppState :: State sbe m -> Doc
ppState s = hang (text "state" <+> lbrace) 2 (ppCtrlStk (s^.backend) (s^.ctrlStk)) $+$ rbrace

ppPath :: Backend sbe -> Path sbe -> Doc
ppPath sbe p =
  case currentCallFrame p of
    Just cf -> 
      text "Path #"
      <>  integer (p^.pathName)
      <>  brackets ( text (cf^.cfClass) <> "." <> ppMethod (cf^.cfMethod)
                   <> "/" <> ppBlockId (cf^.cfBlockId)
                   )
      <>  colon
      $+$ nest 2 (text "Locals:"   $+$ nest 2 (ppLocals sbe (cf^.cfLocals)))
      $+$ nest 2 (text "Operands:" $+$ nest 2 (ppOpds sbe (cf^.cfOpds)))
    Nothing ->
      text "Path #" <> integer (p^.pathName) <> colon <+> "stopped"

ppLocals :: Backend sbe 
         -> Map LocalVariableIndex (Value (SBETerm sbe))
         -> Doc
ppLocals sbe = braces . commas . M.elems . M.mapWithKey ppPair
  where ppPair idx val = int (fromIntegral idx) <+> "=>" <+> ppValue sbe val

ppOpds :: Backend sbe -> [Value (SBETerm sbe)] -> Doc
ppOpds sbe = brackets . commas . map (ppValue sbe)

ppCtrlStk :: Backend sbe -> CS sbe -> Doc
ppCtrlStk sbe (CompletedCS mp) =
  maybe (text "All paths failed") (ppPath sbe) mp
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
  banners $ show $ ppCtrlStk sbe cs

