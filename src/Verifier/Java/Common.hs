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
{-# LANGUAGE ExistentialQuantification #-}
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
import Data.Typeable (Typeable)
import Data.Word (Word32)

import Text.PrettyPrint

import Execution (AtomicValue(..), JSValue(..))
import Verifier.Java.Backend (Backend, MonadTerm, PrettyTerm(..))
import Verifier.Java.Codebase (Codebase, FieldId(..), LocalVariableIndex, Method(..), MethodKey(..), PC, Type(..), methodName, slashesToDots, sourceLineNumberOrPrev)

import Verinf.Utils.LogMonad

-- A Simulator is a monad transformer around a symbolic backend
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
    -- correspondence between each MergeFrame and its corresponding Frame (i.e.,
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

type PathDescriptor = Integer

data SimulationFlags =
  SimulationFlags { alwaysBitBlastBranchTerms :: Bool }
  deriving Show

defaultSimFlags :: SimulationFlags
defaultSimFlags = SimulationFlags { alwaysBitBlastBranchTerms = False }

type SymPath sbe = Path (MonadTerm sbe)

data Path term = Path {
    _frame          :: !(Frame term)
  , _finalResult    :: !(FinalResult term)
  , _pathException  :: !(Maybe term)
  , _initialization :: !(Map String InitializationStatus)
  , _staticFields   :: !(Map FieldId (Value term))
  , _instanceFields :: !(Map InstanceFieldRef (Value term))
  -- | Maps integer and long array to current value.
  , _arrays         :: !(Map Ref (term, term))
  -- | Maps reference array to current value.
  , _refArrays      :: !(Map Ref (Array Int32 Ref))
    -- | Facts that may be assumed to be true in this path.
  , _psAssumptions  :: !term
    -- | Facts that are asserted to be true in this path (in reverse order).
  , _psAssertions   :: ![(String,term)]
  , _pathStSel      :: !PathDescriptor
  , _classObjects   :: !(Map String Ref)
  -- | The program counter where this path state started.
  , _startingPC     :: !PC
  , _breakpoints    :: !(Set (String, MethodKey, PC))
  -- ^ Breakpoint locations. REVISIT: might want to have a map in
  -- state from (String, MethodKey) to Map PC (..., ...), and then
  -- this map could just be Map PC (..., ...), and would get set every
  -- time we modify the frame list.
  , _insnCount      :: !Int
  -- ^ The number of instructions executed so far on this path.
  }

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
    { _excRef    :: Ref          -- ^ the java.lang.Exception instance
    , _excFrames :: [Frame term] -- ^ stack trace @ raise point
    }
  deriving (Show,Typeable)

instance Eq (JavaException term) where
  e1@JavaException{} == e2@JavaException{} = _excRef e1 == _excRef e2

-- | Allow SimulatorM exceptions to be raised via Control.Exception.throw
instance (Show term, Typeable term) => Exception (JavaException term)


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
makeLenses ''Path
makeLenses ''Frame
makeLenses ''ErrorPath
makeLenses ''JavaException

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

ppStk :: [Frame term] -> Doc
ppStk [] = text "(empty stack)"
ppStk fs = vcat . map ppFrameSrcLoc $ fs

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

ppFrame :: PrettyTerm term => Frame term -> String
ppFrame (Call c me pc lvars stack) =
  "Frame (" ++ c ++ "." ++ (methodKeyName $ methodKey me) ++ "): PC " ++ show pc
      ++ "\n      Ls:\n        "
        ++ intercalate "\n        "
             (map (\(l,v) -> "Local #" ++ show l ++ ": " ++ ppValue v) $ M.assocs lvars)
      ++ "\n      Stk:\n        "
        ++ intercalate "\n        "
             (map ppValue stack)

ppFrameSrcLoc :: Frame term -> Doc
ppFrameSrcLoc (Call c me pc _vars _stack) =
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