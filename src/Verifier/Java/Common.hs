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

module Verifier.Java.Common where

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

import Data.JVM.Symbolic.AST
import Execution (AtomicValue(..), JSValue(..))
import Verifier.Java.Backend
import Verifier.Java.Codebase (Codebase, FieldId(..), LocalVariableIndex, Method(..), MethodKey(..), PC, Type(..), methodName, slashesToDots, sourceLineNumberOrPrev)

import Verinf.Utils.LogMonad

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
  -- | Merge at any @return@ statement at the given stack height; flag
  -- specifies whether the function returns a value.
  = ReturnPoint Int Bool
  -- | Merge at the given postdominator node at the given stack height.
  | PostdomPoint Int BlockId

-- | Actions to take when a path reaches the merge point of a branched
-- computation. This type is essentially the environment of the
-- 'SimCont' "closure".
data BranchAction sbe
  -- | Continuation after finishing a path for the @else@ of a branch;
  -- starts running the @then@ branch
  = BranchRunTrue (MonadTerm sbe) -- ^ Branch condition
                  (Path sbe)      -- ^ Path to run for @then@ branch
  -- | Continuation after finishing a path for the @then@ of a branch;
  -- merges with finished @else@ path
  | BranchMerge   (MonadTerm sbe) -- ^ Assertions before merge
                  (MonadTerm sbe) -- ^ Branch condition
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
               , _pathInitialization = M.empty
               , _pathStaticFields = M.empty
               , _pathInstanceFields = M.empty
               , _pathScalarArrays = M.empty
               , _pathRefArrays = M.empty
               , _pathClassObjects = M.empty
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

type Path sbe = Path' (MonadTerm sbe)

data Path' term = Path {
    _pathStack          :: ![CallFrame term]
    -- ^ the current JVM call stack
  , _pathStackHt        :: !Int
    -- ^ the current call frames count
  , _pathException      :: !(Maybe (JavaException term))
    -- ^ the exception thrown on this path, if any  
  --- TODO: These fields should probably be extracted into a memory type like in LSS
  , _pathInitialization :: !(Map String InitializationStatus)
    -- ^ the initialization status of classes on this path
  , _pathStaticFields   :: !(Map FieldId (Value term))
    -- ^ static field values on this path
  , _pathInstanceFields :: !(Map InstanceFieldRef (Value term))
    -- ^ instance field values on this path
  , _pathScalarArrays   :: !(Map Ref (term, term))
    -- ^ integer and long array values (floating point not supported)
  , _pathRefArrays      :: !(Map Ref (Array Int32 Ref))
    -- ^ reference array values
  , _pathClassObjects   :: !(Map String Ref)
    -- ^ java.lang.Class objects for this path
  --- end TODO
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

-- | A JVM call frame
data CallFrame term
  = CallFrame {
      _cfClass  :: !String                               
      -- ^ Name of the class containing the current method
    , _cfMethod :: !Method                               
      -- ^ The current method
    , _cfPC     :: !PC
      -- ^ The current program counter
    , _cfLocals :: !(Map LocalVariableIndex (Value term)) 
      -- ^ The current local variables (<http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-2.html#jvms-2.6.1>)
    , _cfOpds   :: ![Value term]                          
      -- ^ The current operand stack
    }
    deriving (Eq, Show)

data InitializationStatus
  = Started
  | Erroneous
  | Initialized
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
  deriving (Show)

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

returnMerge :: forall sbe m . MonadIO m
            => Backend sbe
            -> Path sbe
            -> SimCont sbe
            -> Simulator sbe m (CS sbe)
returnMerge _ p EmptyCont | 0 == p^.pathStackHt =
  return (CompletedCS (Just p))

returnMerge sbe p (HandleBranch info@(ReturnPoint n hasVal) act h) 
    | n == p^.pathStackHt =
  case act of
    BranchRunTrue c tp -> do
      true <- liftIO $ termBool sbe True
      undefined 
      let tp' = tp & pathAssertions .~ true
      let act' = BranchMerge (tp^.pathAssertions) c p
      return $ ActiveCS tp' (HandleBranch info act' h) 
    BranchMerge a c pf -> do
      let assertions = p^.assertions 
          (Just cf1, Just cf2) = (currentCallFrame p, currentCallFrame pf)
      mergedCallFrame <- mergeCallFrames (p^.pathAssertions) cf1 cf2
      undefined
{-      -- Merge memory
      mergedMemory <- sbeRunIO sbe $ memMerge sbe c (pathMem p) (pathMem pf)
      -- Merge assertions
      mergedAssertions <- sbeRunIO sbe $
        applyIte sbe i1 c (pathAssertions p) (pathAssertions pf)
      a' <- sbeRunIO sbe $ applyAnd sbe a mergedAssertions
      let p' = p { pathRegs = mergedRegs
                 , pathMem = mergedMemory
                 , pathAssertions = a'
                 }
      returnMerge sbe p' h-}
returnMerge _ p h = return (ActiveCS p h)

mergeCallFrames :: MonadIO m
                => MonadTerm sbe
                -> CallFrame (MonadTerm sbe)
                -> CallFrame (MonadTerm sbe)
                -> Simulator sbe m (CallFrame (MonadTerm sbe))
mergeCallFrames assertions cf1 cf2 = do
  let CallFrame class1 method1 pc1 locals1 opds1 = cf1
      CallFrame class2 method2 pc2 locals2 opds2 = cf2
  assert (class1  == class2)
  assert (method1 == method2)
  -- pcs may differ if paths merge at different return insts
  mergedLocals <- mergeBy locals1 locals2 (mergeValues assertions)
  mergedOpds <- zipWithM (mergeValues assertions) opds1 opds2
  return $ cf2 & cfLocals .~ mergedLocals
               & cfOpds   .~ mergedOpds

-- | Merge the map elements (given via the selector 'sel') common to both
-- states via the given action 'mrg', and then union in elements unique to
-- each state as well (via the left-biased map union operator)
mergeBy :: (Functor m, Monad m, Ord k)
        => Map k a
        -> Map k a
        -> (a -> a -> m a)
        -> m (Map k a)
mergeBy m1 m2 mrg = leftUnion <$> merged
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
            => MonadTerm sbe 
            -> Value (MonadTerm sbe) 
            -> Value (MonadTerm sbe) 
            -> Simulator sbe m (Value (MonadTerm sbe)) 
mergeValues assertions x y = mergeV x y
  where abort = fail . render
        t1 <-> t2 = do 
          sbe <- use backend
          liftIO $ termIte sbe assertions t1 t2
        mergeV (IValue v1) (IValue v2)             = IValue <$> v1 <-> v2
        mergeV (LValue v1) (LValue v2)             = LValue <$> v1 <-> v2
        mergeV x@(FValue v1) y@(FValue v2) = do
          if (isNaN v1 && isNaN v2 || v1 == v2)
            then return x
            else abort $ "Attempt to merge two concrete non-NaN unequal floats:"
                 <+> ppValue' x <+> "and" <+> ppValue' y
        mergeV x@(DValue v1) y@(DValue v2) = do
          if (isNaN v1 && isNaN v2 || v1 == v2)
            then return x
            else abort $ "Attempt to merge two concrete non-NaN unequal doubles: "
                 <+> ppValue' x <+> "and" <+> ppValue' y
        mergeV x@(RValue NullRef) (RValue NullRef) = return x
        mergeV x@(RValue (Ref r1 ty1)) y@(RValue (Ref r2 ty2)) = do
          when (r1 /= r2) $
            abort $ "References differ when merging:" 
            <+> ppValue' x <+> "and" <+> ppValue' y
          assert (ty1 == ty2)
          return x
        mergeV x y = 
          abort $ "Unsupported or mismatched type when merging values:"
          <+> ppValue' x <+> "and" <+> ppValue' y

-- FIXME with rest of pretty printers
ppValue' = undefined

--------------------------------------------------------------------------------
-- Pretty printers

{-
instance PrettyTerm term => Show (Path term) where
  show st =
    let dispMapBy :: Map k a -> ((k, a) -> String) -> String
        x `dispMapBy` showItem = (multi . map showItem . M.toList)  x
        x `dispBy` showItem    = (multi . map showItem . DF.toList) x
        multi lns              = pad ++ intercalate pad lns ++ "\n"
        pad                    = "\n" ++ replicate 4 ' '
    in
      ppPSS (pathStSel st) ++ ":\n"
      ++
      "  frames         : "
      ++ (if null $ frames st
            then "(none)\n"
            else frames st `dispBy` ppFrame
         )
      ++
      "  instance fields: "
      ++ (if M.null $ instanceFields st
            then "(none)\n"
            else instanceFields st `dispMapBy` \((r, fldId), v) ->
                   "(" ++ ppRefId r
                   ++ "::"
                   ++ fieldIdClass fldId
                   ++ ")."
                   ++ fieldIdName fldId
                   ++ " => "
                   ++ ppValue v
         )
      ++
      "  static fields  : "
      ++ (if M.null (staticFields st)
            then "(none)\n"
            else
              let f (fldId, v) = fieldIdClass fldId
                                 ++ "."
                                 ++ fieldIdName fldId
                                 ++ " => "
                                 ++ ppValue v
              in
                staticFields st `dispMapBy` f
         )
      ++
      "  arrays         : "
      ++ (if M.null $ arrays st
            then "(none)\n"
            else arrays st `dispMapBy` \(k,(l,v)) -> 
                   show k ++ " : " ++ prettyTerm l ++ " = " ++ prettyTerm v
         )
      ++
      "  refArrays      : "
      ++ (if M.null $ refArrays st
            then "(none)\n"
            else refArrays st `dispMapBy` \(r,rs) ->
                   ppRef r
                   ++ " => [ "
                   ++ intercalate ", " (map ppRefId $ elems rs)
                   ++ " ]"
         )
      ++
  --    "  assumptions    : " ++ ppSymTerm (psAssumptions state) ++ "\n"
  --    ++
      "  pathException  : " ++ ppPathException (pathException st)
      ++
      "  starting PC    : " ++ show (startingPC st)
      ++
      "  instr count    : " ++ show (insnCount st)
-}

ppStk :: [CallFrame term] -> Doc
ppStk [] = text "(empty stack)"
ppStk fs = vcat . map ppCallFrameSrcLoc $ fs

ppJavaException :: JavaException term -> Doc
ppJavaException (JavaException (Ref _ (ClassType nm)) frms) =
  text "Exception of type" <+> text (slashesToDots nm)
  $+$ ppStk frms

ppSimulatorExc JavaException{}     = error "Malformed JavaException"

ppPathException :: Maybe term -> String
ppPathException Nothing = "no exception"
ppPathException exc     = undefined -- ppSimulatorExc <$> exc

ppCallFrame :: PrettyTerm term => CallFrame term -> String
ppCallFrame (CallFrame c me pc lvars stack) =
  "CallFrame (" ++ c ++ "." ++ (methodKeyName $ methodKey me) ++ "): PC " ++ show pc
      ++ "\n      Ls:\n        "
        ++ intercalate "\n        "
             (map (\(l,v) -> "Local #" ++ show l ++ ": " ++ ppValue v) $ M.assocs lvars)
      ++ "\n      Stk:\n        "
        ++ intercalate "\n        "
             (map ppValue stack)

ppCallFrameSrcLoc :: CallFrame term -> Doc
ppCallFrameSrcLoc (CallFrame c me pc _vars _stack) =
  text c <> text "." <> text mn <> parens (text c <> text ".java:" <> text (maybe "?" show mloc))
  where
    (mloc, mn) = (`sourceLineNumberOrPrev` pc) CA.&&& methodName $ me

ppValue :: PrettyTerm term => Value term -> String
ppValue (IValue st) = prettyTerm st
ppValue (LValue st) = prettyTerm st
ppValue (RValue r)  = show r
ppValue (FValue f)  = show f
ppValue (DValue d)  = show d
ppValue (AValue a)  = "Address " ++ show a

ppRef :: Ref -> String
ppRef NullRef    = "null"
ppRef (Ref n ty) = show n ++ "::" ++ show ty

ppRefId :: Ref -> String
ppRefId NullRef   = "null"
ppRefId (Ref n _) = show n

ppPSS :: PathDescriptor -> String
ppPSS n = "Path #" ++ show n

ppMethod :: Method -> Doc
ppMethod = text . methodKeyName . methodKey

instance LogMonad (Simulator sbe m) where
  getVerbosity = use verbosity
  setVerbosity = assign verbosity

ppState :: State sbe m -> Doc
ppState s = hang (text "state" <+> lbrace) 2 (s^.ctrlStk.to ppCS) $+$ rbrace

ppCS :: CS sbe -> Doc
ppCS cs = undefined

ppPath :: Path term -> Doc
ppPath = undefined