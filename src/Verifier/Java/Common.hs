{- |
Module           : $Header$
Description      : Shared types and utility functions for JSS
Stability        : stable
Point-of-contact : acfoltzer
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Verifier.Java.Common where

import Control.Applicative (Applicative, (<$>), pure, (<*>))
import qualified Control.Arrow as CA
import Control.Exception (Exception)
import Control.Lens hiding (Path)

import Data.Array (Array, elems)
import qualified Data.Foldable as DF
import Data.Int (Int32)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word32)

import Text.PrettyPrint

import Execution (AtomicValue(..), JSValue(..))
import Verifier.Java.Backend (Backend, MonadTerm, PrettyTerm(..))
import Verifier.Java.Codebase (Codebase, FieldId(..), LocalVariableIndex, Method(..), MethodKey(..), PC, Type(..), methodName, slashesToDots, sourceLineNumberOrPrev)

import Verinf.Utils.CatchMIO
import Verinf.Utils.IOStateT
import Verinf.Utils.LogMonad

-- A Simulator is a monad transformer around a symbolic backend m
newtype Simulator sym a = SM { runSM :: StateT (State sym) IO a }
  deriving (CatchMIO, Functor, Monad, MonadIO, MonadState (State sym))

instance Applicative (Simulator sym) where
  pure      = return
  af <*> aa = af >>= flip (<$>) aa

data State sym = State {
    _codebase          :: !Codebase
  , _instanceOverrides :: !(Map (String, MethodKey) (Ref -> [Value' sym] -> Simulator sym ()))
    -- ^ Maps instance method identifiers to a function for executing them.
  , _staticOverrides   :: !(Map (String, MethodKey) ([Value' sym] -> Simulator sym ()))
    -- ^ Maps static method identifiers to a function for executing them.
  , _ctrlStk           :: CtrlStk sym
    -- ^ Auxiliary data structures for tracking execution and merging of
    -- multiple paths within a single frame.  Currently, there is a 1-1
    -- correspondence between each MergeFrame and its corresponding Frame (i.e.,
    -- the Java activation record).
  , _nextPSS           :: PathDescriptor
    -- ^ Name supply for unique path state selectors
  , _strings           :: !(Map String Ref)
  , _nextRef           :: !Word32 -- ^ Next index for constant ref.
  , _verbosity         :: Int
  , _simulationFlags   :: SimulationFlags
  , _backend           :: Backend sym
  , _errorPaths        :: [ErrorPath sym]
  }

type Value term = AtomicValue Double Float term term Ref
type Value' sym = JSValue (Simulator sym)

-- Address in heap
data Ref
  = NullRef
  | Ref !Word32 !Type
  deriving (Show)

newtype CtrlStk sym = CtrlStk { _mergeFrames :: [MergeFrame sym] }

emptyCtrlStk :: CtrlStk sym
emptyCtrlStk = CtrlStk []

type PathDescriptor = Integer

data SimulationFlags =
  SimulationFlags { alwaysBitBlastBranchTerms :: Bool }
  deriving Show

defaultSimFlags :: SimulationFlags
defaultSimFlags = SimulationFlags { alwaysBitBlastBranchTerms = False }

newtype ErrorPath sym = EP { epPath :: SymPath sym }

type SymPath sym = Path (MonadTerm sym)

data Path term = Path {
    _frames          :: ![Frame term]
  , _finalResult     :: !(FinalResult term)
  , _pathException   :: !(Maybe term)
  , _initialization  :: !(Map String InitializationStatus)
  , _staticFields    :: !(Map FieldId (Value term))
  , _instanceFields  :: !(Map InstanceFieldRef (Value term))
  -- | Maps integer and long array to current value.
  , _arrays          :: !(Map Ref (term, term))
  -- | Maps reference array to current value.
  , _refArrays       :: !(Map Ref (Array Int32 Ref))
    -- | Facts that may be assumed to be true in this path.
  , _psAssumptions   :: !term
    -- | Facts that are asserted to be true in this path (in reverse order).
  , _psAssertions    :: ![(String,term)]
  , _pathStSel       :: !PathDescriptor
  , _classObjects    :: !(Map String Ref)
  -- | The program counter where this path state started.
  , _startingPC      :: !PC
  , _breakpoints     :: !(Set (String, MethodKey, PC))
  -- ^ Breakpoint locations. REVISIT: might want to have a map in
  -- state from (String, MethodKey) to Map PC (..., ...), and then
  -- this map could just be Map PC (..., ...), and would get set every
  -- time we modify the frame list.
  , _insnCount       :: !Int
  -- ^ The number of instructions executed so far on this path.
  }

data MergeFrame sym
  = ExitMergeFrame (ExitFrame sym)
  | PostdomMergeFrame (PostdomFrame sym)
  | ReturnMergeFrame (ReturnFrame sym)

data ExitFrame sym = ExitFrame {
       _efMergedState  :: MergedState sym
     , _efPending      :: [SymPath sym]     
     }  

data PostdomFrame sym = PostdomFrame { 
       _pdfMergedState :: MergedState sym
     , _pdfPending     :: [SymPath sym]
     , _pdfMethod      :: Method
     }

data ReturnFrame sym = ReturnFrame {
       _rfMethod        :: Method
     , _rfFrame         :: Frame sym        -- ^ Call frame for path when it arrives.
     , _rfIsVoid        :: Bool             -- ^ Whether to store a return value afterwards
     , _rfNormalState   :: MergedState sym  -- ^ Merged state after function call return.
     , _rfPending       :: [SymPath sym]
     }

data MergedState sym 
  = EmptyState sym
  | PathState (SymPath sym)

data Frame term
  = Call {
      _frmClass  :: !String                               -- Name of current class that
                                                          -- we are in.
    , _frmMethod :: !Method                               -- Method we are running in
    , _frmPC     :: !PC                                   -- Current PC
    , _frmLocals :: !(Map LocalVariableIndex (Value term)) -- Local variable map
    , _frmOpds   :: ![Value term]                          -- Operand stack
    }
    deriving (Eq, Show)

data FinalResult term
  = ReturnVal !(Value term)
  | Breakpoint !PC
  | Exc !(SimulatorExc term)
  | Terminated
  | Aborted
  | Unassigned
  deriving (Eq, Show)

data InitializationStatus
  = Started
  | Erroneous
  | Initialized
  deriving (Eq, Ord, Show)

type InstanceFieldRef = (Ref, FieldId)

-- | Our exception data structure; used to track errors intended to be exposed
-- to the user (via the SimExtErr ctor) as well as tracking and propagating Java
-- exceptions.
data SimulatorExc term
  = SimExtErr
    { simExtErrMsg       :: String
    , simExtErrVerbosity :: Int -- ^ simulator verbosity @ raise point
    , simExtErrResults   :: Map PathDescriptor (FinalResult term, [Frame term])
    }
  | JavaException
    { excRef    :: Ref          -- ^ the java.lang.Exception instance
    , excFrames :: [Frame term] -- ^ stack trace @ raise point
    }
  deriving (Show,Typeable)

instance Eq (SimulatorExc m) where
  e1@JavaException{} == e2@JavaException{} = excRef e1 == excRef e2
  _ == _ = False

-- | Allow SimulatorM exceptions to be raised via Control.Exception.throw
instance (Show term, Typeable term) => Exception (SimulatorExc term)


instance Eq Ref where
  NullRef == NullRef = True
  (Ref x _) == (Ref y _) = x == y
  _ == _ = False

instance Ord Ref where
  NullRef `compare` NullRef = EQ
  NullRef `compare` _ = LT
  _ `compare` NullRef = GT
  (Ref x _) `compare` (Ref y _) = x `compare` y

makeLenses ''ExitFrame
makeLenses ''PostdomFrame
makeLenses ''ReturnFrame
makeLenses ''State
makeLenses ''CtrlStk
makeLenses ''Path
makeLenses ''Frame

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

ppStk :: [Frame term] -> String
ppStk [] = "(empty stack)"
ppStk fs = concatMap ((\s -> "  at " ++ s ++ "\n") . ppFrameSrcLoc) fs

ppSimulatorExc :: SimulatorExc term -> String
ppSimulatorExc (JavaException (Ref _ (ClassType nm)) frms) =
  "Exception of type " ++ slashesToDots nm ++ "\n" ++ ppStk frms
ppSimulatorExc (SimExtErr msg _ _) = msg
ppSimulatorExc JavaException{}     = error "Malformed SimulatorExc"

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

ppFrame :: PrettyTerm term => Frame term -> String
ppFrame (Call c me pc lvars stack) =
  "Frame (" ++ c ++ "." ++ (methodKeyName $ methodKey me) ++ "): PC " ++ show pc
      ++ "\n      Ls:\n        "
        ++ intercalate "\n        "
             (map (\(l,v) -> "Local #" ++ show l ++ ": " ++ ppValue v) $ M.assocs lvars)
      ++ "\n      Stk:\n        "
        ++ intercalate "\n        "
             (map ppValue stack)

ppFrameSrcLoc :: Frame term -> String
ppFrameSrcLoc (Call c me pc _vars _stack) =
  c ++ "." ++ mn ++ "(" ++ c ++ ".java:" ++ maybe "?" show mloc ++ ")"
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

-- TODO: better pretty-printing
ppMergeFrame :: forall sym. MergeFrame sym -> Doc
ppMergeFrame mf = case mf of
  ExitMergeFrame ef ->
    text "MF(Exit):"
    $+$ nest 2 (ef^.efPending.to ppPendingPaths)
  PostdomMergeFrame pdf ->
    text "MF(Pdom|" <> pdf^.pdfMethod.to ppMethod <> text "):"
    $+$ nest 2 (pdf^.pdfMergedState.to (mpath ""))
    $+$ nest 2 (pdf^.pdfPending.to ppPendingPaths)
  ReturnMergeFrame rf ->
    text "MF(Retn):" $+$ nest 2 rest
      where
        rest = text "Normal" <+> text "~>" <+> rf^.rfMethod.to ppMethod <> colon
               $+$ nest 2 (rf^.rfNormalState.to (mpath "no normal-return merged state set"))
               $+$ rf^.rfPending.to ppPendingPaths
  where
    mpath :: String -> MergedState sym -> Doc
    mpath str (EmptyState _) = parens $ text ("Merged: " ++ str)
    mpath _ (PathState p) = ppPath p
    ppPendingPaths :: [Path term] -> Doc
    ppPendingPaths pps =
      text "Pending paths:"
      $+$ nest 2 (if null pps then text "(none)" else vcat (map ppPath pps))

instance LogMonad (Simulator sym) where
  getVerbosity = use verbosity
  setVerbosity = assign verbosity

ppState :: State sym -> Doc
ppState s = hang (text "state" <+> lbrace) 2 (s^.ctrlStk.to ppCtrlStk) $+$ rbrace

ppCtrlStk :: CtrlStk sym -> Doc
ppCtrlStk cs =
  hang (text "CS" <+> lbrace) 2 (vcat (map ppMergeFrame (cs^.mergeFrames))) $+$ rbrace


ppPath :: Path term -> Doc
ppPath = undefined

{-
instance (PrettyTerm (MonadTerm sym)) => Show (State sym) where
  show s = intercalate "\n" (["{ begin ctrlstk "] ++ (map ppMergeFrame . mergeFrames $ ctrlStk s) ++ ["end pathstates }"])
             ++ "\nNext Ref: " ++ show (nextRef s)
-}

mfGetting e _ _ (ExitMergeFrame ef)     = view e ef
mfGetting _ p _ (PostdomMergeFrame pdf) = view p pdf
mfGetting _ _ r (ReturnMergeFrame rf)   = view r rf

mfSetting e _ _ (ExitMergeFrame ef)     v = ExitMergeFrame    (set e v ef)
mfSetting _ p _ (PostdomMergeFrame pdf) v = PostdomMergeFrame (set p v pdf)
mfSetting _ _ r (ReturnMergeFrame rf)   v = ReturnMergeFrame  (set r v rf)

class HasPending t s | t -> s where
  pending :: Simple Lens t [SymPath s]

instance HasPending (ExitFrame sym) sym where
  pending = efPending

instance HasPending (PostdomFrame sym) sym where
  pending = pdfPending

instance HasPending (ReturnFrame sym) sym where
  pending = rfPending

instance HasPending (MergeFrame sym) sym where
  pending = lens view' set'
    where view' = mfGetting pending pending pending
          set'  = mfSetting pending pending pending

class HasMergedState t s | t -> s where
  mergedState :: Simple Lens t (MergedState s)

instance HasMergedState (ExitFrame sym) sym where
  mergedState = efMergedState

instance HasMergedState (PostdomFrame sym) sym where
  mergedState = pdfMergedState

instance HasMergedState (ReturnFrame sym) sym where
  mergedState = rfNormalState

instance HasMergedState (MergeFrame sym) sym where
  mergedState = lens view' set'
    where view' = mfGetting mergedState mergedState mergedState
          set'  = mfSetting mergedState mergedState mergedState