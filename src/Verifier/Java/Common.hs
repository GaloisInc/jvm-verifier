{- |
Module           : $Header$
Description      : Shared types and utility functions for JSS
Stability        : stable
Point-of-contact : acfoltzer
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Verifier.Java.Common where

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
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable)
import Data.Word (Word32)

import Text.PrettyPrint

import Execution (AtomicValue(..), JSValue(..))
import Verifier.Java.Backend
import Verifier.Java.Codebase (Codebase, FieldId(..), LocalVariableIndex, Method(..), MethodKey(..), PC, Type(..), methodName, slashesToDots, sourceLineNumberOrPrev)

import Verinf.Utils.LogMonad

-- | A Simulator is a monad transformer around a symbolic backend
newtype Simulator sbe m a = 
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

-- | First-order continuations for the symbolic simulation.
data SimCont sbe
  -- | Empty continuation: there are no remaining paths to run
  = EmptyCont
  -- | Continuation after finishing a path for the @else@ of a branch;
  -- starts running the @then@ branch
  | BranchRunTrue (MonadTerm sbe) -- ^ Branch condition
                  (Path sbe)      -- ^ Path to run for @then@ branch
                  (SimCont sbe)   -- ^ Next continuation
  -- Continuation after finishing a path for the @then@ of a branch;
  -- merges with finished @else@ path
  | BranchMerge   (MonadTerm sbe) -- ^ Assertions before merge
                  (MonadTerm sbe) -- ^ Branch condition
                  (Path sbe)      -- ^ Completed @else@ branch
                  (SimCont sbe)   -- ^ Next continuation

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
               , _pathAssumptions = true
               , _pathAssertions = []
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
  , _pathException      :: !(Maybe term)
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
  , _pathAssumptions    :: !term
    -- ^ facts assumed to be true on this path
  , _pathAssertions     :: ![(String,term)]
    -- ^ facts that are asserted to be true in this path (in reverse order).
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

data FinalResult term
  = ReturnVal !(Value term)
  | Breakpoint !PC
  | Exc !(JavaException term)
  | Terminated
  | Aborted
  | Unassigned
  deriving (Eq, Show)

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
  NullRef `compare` NullRef = EQ
  NullRef `compare` _ = LT
  _ `compare` NullRef = GT
  (Ref x _) `compare` (Ref y _) = x `compare` y

makeLenses ''State
makeLenses ''Path'
makeLenses ''CallFrame
makeLenses ''ErrorPath
makeLenses ''JavaException

-- | Manipulate the control stack
modifyCS :: (CS sbe -> CS sbe) -> (State sbe m -> State sbe m)
modifyCS = over ctrlStk

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

ppFinalResult :: PrettyTerm term => FinalResult term -> String
ppFinalResult (ReturnVal rv)  = ppValue rv
ppFinalResult (Breakpoint pc) = "breakpoint{" ++ show pc ++ "}"
ppFinalResult (Exc exc)       = ppSimulatorExc exc
ppFinalResult Terminated      = "(void)"
ppFinalResult Aborted         = "Aborted"
ppFinalResult Unassigned      = "Unassigned"

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