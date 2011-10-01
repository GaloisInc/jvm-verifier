{- |
Module           : $Header$
Description      : The symbolic simulation engine for Java
Stability        : stable
Point-of-contact : jstanley
-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Simulation
  ( module Execution
  , SimulatorExc(..)
  , FinalResult(..)
  , Frame(..)
  , InitializationStatus(..)
  , InstanceFieldRef
  , PathDescriptor
  , Ref(..)
  , State
  , SimulationFlags(..)
  , Node
  , Value
  , cIntValue
  , defaultSimFlags
  , dumpMergeFrames
  -- * SimulationMonad core operations
  , Simulator
  , assume
  , dumpPathStateCount
  , dumpPathStates
  , getInitializationStatus
  , getInstanceFieldValue
  , getStaticFieldValue
  , runSimulator
  , runSimulator'
  , setInitializationStatus
  -- * PathState operations.
  , PathState(..)
  , getPathState
  , getPathStateByName
  , getPSS
  , withPathState
  -- * Generic reference operations
  , genRef
  , getType
  -- * Array operations
  , getArrayLength
  , getArrayValue
  , getIntArray
  , getRefArray
  , getSymbolicArray
  , newIntArray
  , newLongArray
  , newSymbolicArray
  , setSymbolicArray
  , updateSymbolicArray
  -- * Misc
  , liftSymbolic
  , overrideInstanceMethod
  , overrideStaticMethod
  , ppFinalResult
  , ppSimulatorExc
  , ppValue
  , setVerbosity
  , simExcHndlr
  , simExcHndlr'
--  , verbosity
  , withVerbosity
  -- * Breakpoints
  , registerBreakpoints
  , resumeBreakpoint
  )
where

import qualified Control.Arrow as CA
import Control.Arrow (second)
import Control.Applicative
import qualified Control.Exception as CE
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Reader
import Data.Array
import qualified Data.Foldable as DF
import Data.Char
import Data.Int
import Data.List hiding (intersectBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable hiding (typeOf)
import qualified Data.Vector.Storable as SV
import Data.Word
import Prelude hiding (catch)
import System.IO (hFlush, hPutStr, stderr, stdout)

import Analysis.CFG (ppInst)
import Execution
import JavaParser
import JavaParser.Common
import Utils
import Utils.Common

import Verinf.Symbolic hiding (defaultValue, (&&&), (|||))
import Verinf.Utils.CatchMIO
import Verinf.Utils.IOStateT
import Verinf.Utils.LogMonad

data InitializationStatus
  = Started
  | Erroneous
  | Initialized
  deriving (Eq, Ord, Show)

-- Address in heap
data Ref
  = NullRef
  | Ref !Word32 !Type
  deriving (Show)

type Value term = AtomicValue Double Float term term Ref
type Value' sym = JSValue (Simulator sym)

data Frame term
  = Call {
      frmClass  :: !String                               -- Name of current class that
                                                         -- we are in.
    , frmMethod :: !Method                               -- Method we are running in
    , frmPC     :: !PC                                   -- Current PC
    , frmLocals :: !(Map LocalVariableIndex (Value term)) -- Local variable map
    , frmOpds   :: ![Value term]                          -- Operand stack
    }
    deriving (Eq, Show)

-- | @MergeFrame@s help track nested breakpoints and execution/merging of paths.
data MergeFrame sym
  = MergeFrame {
      finishedPaths :: ![PathDescriptor]
      -- ^ Paths in this frame that have either terminated (i.e., returned
      -- normally or raised an exception) or have hit a breakpoint
    , nextPaths     :: ![(PathDescriptor, ResumeAction sym)]
      -- ^ Paths in this frame that are still being executed; the current path
      -- is always the head of nextPaths.  The second tuple component is the
      -- "resume" action along the associated path -- if Nothing, normal
      -- execution along the path occurs starting at the path state's current
      -- PC.  If Just, that action occurs when the path begins (or resumes)
      -- execution.  This is how we deal with "partial execution" of JVM
      -- instructions.
    , breakpoint    :: Breakpoint
    }

data Breakpoint
  = CallExit
  | Unsupported
  deriving (Eq, Show)

data ResumeAction sym
  = NextInst
  | CustomRA
    { _craDesc :: String
    , _craAct  :: Simulator sym (ResumeAction sym)
    }

instance Show (ResumeAction sym) where
  show NextInst          = "(RA: next inst)"
  show (CustomRA desc _) = desc

type InstanceFieldRef = (Ref, FieldId)

data FinalResult term
  = ReturnVal !(Value term)
  | Breakpoint !PC
  | Exc { getExc :: !(SimulatorExc term) }
  | Terminated
  | Aborted
  | Unassigned
  deriving (Eq, Show)

data PathState term = PathState {
    frames          :: ![Frame term]
  , finalResult     :: !(FinalResult term)
  , initialization  :: !(Map String InitializationStatus)
  , staticFields    :: !(Map FieldId (Value term))
  , instanceFields  :: !(Map InstanceFieldRef (Value term))
  -- | Maps integer and long array to current value.
  , arrays          :: !(Map Ref (Int32, term))
  -- | Maps reference array to current value.
  , refArrays       :: !(Map Ref (Array Int32 Ref))
  , psAssumptions   :: !term
  , pathStSel       :: !PathDescriptor
  , classObjects    :: !(Map String Ref)
  -- | The program counter where this path state started.
  , startingPC      :: !PC
  , breakpoints     :: !(Set (String, MethodKey, PC))
  -- ^ Breakpoint locations. REVISIT: might want to have a map in
  -- state from (String, MethodKey) to Map PC (..., ...), and then
  -- this map could just be Map PC (..., ...), and would get set every
  -- time we modify the frame list.
  , insnCount       :: !Int
  -- ^ The number of instructions executed so far on this path.
  }

data State sym = State {
    codebase          :: !Codebase
  , instanceOverrides :: !(Map (String, MethodKey) (Ref -> [Value' sym] -> Simulator sym ()))
    -- ^ Maps instance method identifiers to a function for executing them.
  , staticOverrides   :: !(Map (String, MethodKey) ([Value' sym] -> Simulator sym ()))
    -- ^ Maps static method identifiers to a function for executing them.
  , pathStates        :: !(Map PathDescriptor (PathState (MonadTerm sym)))
    -- ^ Simulation state for each distinct control flow path
  , mergeFrames       :: ![MergeFrame sym]
    -- ^ Auxiliary data structures for tracking execution and merging of
    -- multiple paths within a single frame.  Currently, there is a 1-1
    -- correspondence between each MergeFrame and its corresponding Frame (i.e.,
    -- the Java activation record).
  , nextPSS           :: PathDescriptor
    -- ^ Name supply for unique path state selectors
  , strings           :: !(Map String Ref)
  , nextRef           :: !Word32 -- ^ Next index for constant ref.
  , verbosity         :: Int
  , simulationFlags   :: SimulationFlags
  }

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
  deriving (Show, Typeable)

instance Eq (SimulatorExc m) where
  e1@JavaException{} == e2@JavaException{} = excRef e1 == excRef e2
  _ == _ = False

-- | Allow SimulatorM exceptions to be raised via Control.Exception.throw
instance (Show term, Typeable term) => Exception (SimulatorExc term)

-- A Simulator is a monad transformer around a symbolic backend m
newtype Simulator sym a = SM { runSM :: StateT (State sym) sym a }
  deriving (CatchMIO, Functor, Monad, MonadIO, MonadState (State sym))

instance MonadIO sym => HasCodebase (Simulator sym) where
  getCodebase    = gets codebase

instance MonadIO sym => LogMonad (Simulator sym) where
  getVerbosity   = gets verbosity
  setVerbosity v = modify $ \s -> s { verbosity = v}

instance Monad sym => Applicative (Simulator sym) where
  pure      = return
  af <*> aa = af >>= flip (<$>) aa

liftSymbolic :: Monad sym => sym a -> Simulator sym a
liftSymbolic = SM . lift

data SimulationFlags =
  SimulationFlags { alwaysBitBlastBranchTerms :: Bool }
  deriving Show

defaultSimFlags :: SimulationFlags
defaultSimFlags = SimulationFlags { alwaysBitBlastBranchTerms = False }

runSimulator' ::
  forall sym a.
  ( AigOps sym
  )
  => SimulationFlags -> Codebase -> Simulator sym a -> sym a
runSimulator' flags cb m = evalStateT (runSM go) (newSimState flags cb)
  where
    go = stdOverrides >> m `catchMIO` \(e :: CE.SomeException) -> do
           let h :: (Show t, Typeable t, t ~ MonadTerm sym) => Maybe (SimulatorExc t)
               h = CE.fromException e
           case h of
             Just (e'@(SimExtErr msg _ _)) -> do
               CE.throw $
                 e'{ simExtErrMsg =
                       "An error has occurred in the symbolic simulator:\n"
                       ++ msg
                       -- TODO: put some contact info here?
                   }
             Just e'@(JavaException{}) -> do
               error $ "runSimulator internal: uncaught Java exception: " ++ show (excRef e')
             _ -> do
               CE.throw e

-- | Execute a simulation action; propagate any exceptions that occur so that
-- clients can deal with them however they wish.
runSimulator ::
  forall sym a.
  ( AigOps sym
  )
  => Codebase -> Simulator sym a -> sym a
runSimulator cb m = runSimulator' defaultSimFlags cb m

newSimState :: ConstantInjection (MonadTerm sym) =>
               SimulationFlags -> Codebase -> State sym
newSimState flags cb = State
  { codebase          = cb
  , instanceOverrides = M.empty
  , staticOverrides   = M.empty
  , pathStates        =
      -- NB: Always start with one path state; startNewPath adds more
      M.singleton (PSS 0) $ PathState
        { frames          = []
        , finalResult     = Unassigned
        , initialization  = M.empty
        , staticFields    = M.empty
        , instanceFields  = M.empty
        , arrays          = M.empty
        , refArrays       = M.empty
        , psAssumptions   = mkCBool True
        , pathStSel       = PSS 0
        , classObjects    = M.empty
        , startingPC      = 0
        , breakpoints     = S.empty
        , insnCount       = 0
        }
  , mergeFrames = [mergeFrame [(PSS 0, NextInst)]]
  , nextPSS         = PSS 1
  , strings         = M.empty
  , nextRef         = 1
  , verbosity       = 1
  , simulationFlags = flags
  }

-- | Override behavior of simulator when it encounters a specific instance
-- method to perform a user-definable action.
-- Note: Fails if the method has already been overridden.
overrideInstanceMethod :: MonadIO sym =>
                          String
                       -> MethodKey
                       -> (Ref -> [Value (MonadTerm sym)] -> Simulator sym ())
                       -> Simulator sym ()
overrideInstanceMethod cName mKey action = do
  s <- get
  let key = (cName, mKey)
  when (key `M.member` instanceOverrides s) $ do
    fail $ "Method " ++ cName ++ "." ++ methodKeyName mKey  ++ " is already overridden."
  put s { instanceOverrides = M.insert key action (instanceOverrides s) }

-- | Override behavior of simulator when it encounters a specific static
-- method to perform a user-definable action.
-- Note: Fails if the method has already been overridden.
overrideStaticMethod ::
  ( CatchMIO sym
  , Show (MonadTerm sym)
  , Typeable (MonadTerm sym)
  )
  => String
  -> MethodKey
  -> ([Value (MonadTerm sym)] -> Simulator sym ())
  -> Simulator sym ()
overrideStaticMethod cName mKey action = do
  s <- get
  let key = (cName, mKey)
  when (key `M.member` staticOverrides s) $ do
    abort $ "Method " ++ cName ++ "." ++ methodKeyName mKey ++ " is already overridden."
  put s { staticOverrides = M.insert key action (staticOverrides s) }

-- | Register all predefined overrides for builtin native implementations and
-- the com.galois.symbolic.Symbolic API.
stdOverrides :: (AigOps sym) => Simulator sym ()
stdOverrides = do
  --------------------------------------------------------------------------------
  -- Instance method overrides

  mapM_ (\(cn, key, impl) -> overrideInstanceMethod cn key impl)
    [ printlnMthd "()V"
    , printlnMthd "(Z)V"
    , printlnMthd "(C)V"
    , printlnMthd "([C)V"
    , printlnMthd "(D)V"
    , printlnMthd "(F)V"
    , printlnMthd "(I)V"
    , printlnMthd "(J)V"
    , printlnMthd "(Ljava/lang/Object;)V"
    , printlnMthd "(Ljava/lang/String;)V"
    , printMthd   "(Z)V"
    , printMthd   "(C)V"
    , printMthd   "([C)V"
    , printMthd   "(D)V"
    , printMthd   "(F)V"
    , printMthd   "(I)V"
    , printMthd   "(J)V"
    , printMthd   "(Ljava/lang/Object;)V"
    , printMthd   "(Ljava/lang/String;)V"
    , appendIntegralMthd "(I)Ljava/lang/StringBuilder;"
    , appendIntegralMthd "(J)Ljava/lang/StringBuilder;"
    -- java.io.BufferedOutputStream.flush
    , ( "java/io/BufferedOutputStream"
      , makeMethodKey "flush" "()V"
      , \_ _ -> liftIO $ hFlush stdout
      )
    -- java.lang.Runtime.gc
    , ( "java/lang/Runtime"
      , makeMethodKey "gc" "()V"
      -- Should we implement a garbage collector? ;)
      , \_ _ -> return ()
      )
    -- java.lang.Throwable.fillInStackTrace
    -- REVISIT: We may want to correctly populate the Throwable instance,
    -- instead of this just being a pass-through.
    , ( "java/lang/Throwable"
      , makeMethodKey "fillInStackTrace" "()Ljava/lang/Throwable;"
      , \this _ -> pushValue (RValue this)
      )
    -- java.lang.Class.isArray
    , ( "java/lang/Class"
      , makeMethodKey "isArray" "()Z"
      , \this _ -> pushValue =<< classNameIsArray <$> getClassName this
      )
    -- java.lang.Class.isPrimitive
    , ( "java/lang/Class"
      , makeMethodKey "isPrimitive" "()Z"
      , \this _ -> pushValue =<< classNameIsPrimitive <$> getClassName this
      )
    -- java.lang.Class.getComponentType
    , ( "java/lang/Class"
      , makeMethodKey "getComponentType" "()Ljava/lang/Class;"
      , \this _ -> do
          nm <- getClassName this
          pushValue =<< RValue
                        <$> if classNameIsArray' nm
                            then getClassObject (tail nm)
                            else return NullRef
      )
    -- java.lang.class.getClassLoader -- REVISIT: This implementation makes it so
    -- that custom class loaders are not supported.
    , ( "java/lang/Class"
      , makeMethodKey "getClassLoader" "()Ljava/lang/ClassLoader;"
      , \_ _ -> pushValue (RValue NullRef)
      )
    -- java.lang.String.intern -- FIXME (must reconcile reference w/ strings map)
    , ( "java/lang/String"
      , makeMethodKey "intern" "()Ljava/lang/String;"
      , \this _ -> pushValue =<< RValue <$> (refFromString =<< drefString this)
      )
    ]

  --------------------------------------------------------------------------------
  -- Static method overrides

  mapM_ (\(cn, key, impl) -> overrideStaticMethod cn key impl)
    [ -- Java.lang.System.arraycopy
      let arrayCopyKey =
            makeMethodKey "arraycopy"
              "(Ljava/lang/Object;ILjava/lang/Object;II)V"
      in
        ( "java/lang/System"
        , arrayCopyKey
        , \opds -> do
            let nativeClass = "com/galois/core/NativeImplementations"
            cl <- lookupClass nativeClass
            let Just methodImpl = cl `lookupMethod` arrayCopyKey
            pushStaticMethodCall nativeClass methodImpl opds
        )
      -- java.lang.Float.floatToRawIntBits: override for invocation by
      -- java.lang.Math's static initializer
    , ( "java/lang/Float"
      , makeMethodKey "floatToRawIntBits" "(F)I"
      , \args -> case args of
                   [FValue flt] -> do
                     when (flt /= (-0.0 :: Float)) $
                       abort "floatToRawIntBits: overridden for -0.0f only"
                     pushValue (IValue $ mkCInt 32 0x80000000)
                   _ -> abort "floatToRawIntBits: called with incorrect arguments"
      )
      -- java.lang.Double.doubleToRawLongBits: override for invocation by
      -- java.lang.Math's static initializer
    , ( "java/lang/Double"
      , makeMethodKey "doubleToRawLongBits" "(D)J"
      , \args -> case args of
                   [DValue dbl] -> do
                     when (dbl /= (-0.0 :: Double)) $
                       abort "doubltToRawLongBits: overriden -0.0d only"
                     pushValue (LValue $ mkCInt 64 0x8000000000000000)
                   _ -> abort "floatToRawIntBits: called with incorrect arguments"
      )
      -- Set up any necessary state for the native methods of various
      -- classes. At the moment, nothing is necessary.
    , ( "java/lang/Class"
      , makeMethodKey "registerNatives" "()V"
      , \_ -> return ()
      )
    , ( "java/lang/ClassLoader"
      , makeMethodKey "registerNatives" "()V"
      , \_ -> return ()
      )
    , ( "java/lang/Thread"
      , makeMethodKey "registerNatives" "()V"
      , \_ -> return ()
      )
    , ( "java/lang/Class"
      , makeMethodKey "desiredAssertionStatus0" "(Ljava/lang/Class;)Z"
      , \_ -> pushValue (IValue $ mkCInt 32 1)
      )
    , ( "java/lang/Class"
      , makeMethodKey "getPrimitiveClass" "(Ljava/lang/String;)Ljava/lang/Class;"
      , \args -> case args of
                   [RValue strRef@(Ref _ (ClassType "java/lang/String"))] -> do
                     -- NB: I've guessed on the correct class names for
                     -- primitive types.  Instantiating java.class.Class with
                     -- the type descriptor name seems to work, though, at least
                     -- for the jdk invocations that arise via executing the
                     -- ashes suite.  We should probably REVISIT this as well as
                     -- consider a real reflection infrastructure. [js 04 Nov
                     -- 2010]

                     ms <- lookupStringRef strRef
                     (pushValue . RValue <=< getClassObject) $ case ms of
                       Just "byte"    -> "B"
                       Just "short"   -> "S"
                       Just "int"     -> "I"
                       Just "long"    -> "J"
                       Just "float"   -> "F"
                       Just "double"  -> "D"
                       Just "boolean" -> "Z"
                       Just "char"    -> "C"
                       Just _         -> error "getPrimitiveClass: unsupported type string"
                       Nothing        -> error "getPrimitiveClass: could not resolve string reference"
                   _ -> abort "getPrimitiveClass: called with incorrect arguments"
      )
    , ( "java/io/FileInputStream", makeMethodKey "initIDs" "()V", \ _ -> return () )
    , ( "java/io/FileOutputStream", makeMethodKey "initIDs" "()V", \ _ -> return () )
    , ( "java/io/RandomAccessFile", makeMethodKey "initIDs" "()V", \ _ -> return () )
    , ( "java/io/ObjectStreamClass", makeMethodKey "initNative" "()V", \ _ -> return () )
    , ( "java/security/AccessController"
      , makeMethodKey "doPrivileged" "(Ljava/security/PrivilegedAction;)Ljava/lang/Object;"
      , \args -> case args of
                   [RValue a@(Ref _ (ClassType cn))] ->
                     invokeInstanceMethod
                     cn
                     (makeMethodKey "run" "()Ljava/lang/Object;")
                     a []
                   _ -> abort "doPrivileged called with incorrect arguments"
      )

      --------------------------------------------------------------------------------
      -- fresh vars & path info

      -- TODO: Make the overriden functions be total and handle errors if, e.g.,
      -- user passes a symbolic value where a concrete one is expected
      -- (getPathDescriptors, for instance).

    , sym "freshByte" "(B)B"    $ \_ -> freshByte `pushAs` IValue
    , sym "freshInt" "(I)I"     $ \_ -> freshInt  `pushAs` IValue
    , sym "freshBoolean" "(Z)Z" $ \_ -> freshInt  `pushAs` IValue
    , sym "freshLong" "(J)J"    $ \_ -> freshLong `pushAs` LValue

    , sym "freshByteArray" "(I)[B" $ \[IValue (intVal -> Just n)] ->
        pushByteArr =<< replicateM n (liftSymbolic freshByte)

    , sym "freshIntArray" "(I)[I" $ \[IValue (intVal -> Just n)] ->
        pushIntArr =<< replicateM n (liftSymbolic freshInt)

    , sym "freshLongArray" "(I)[J" $ \[IValue (intVal -> Just n)] ->
        pushLongArr =<< replicateM n (liftSymbolic freshLong)

    , sym "getPathDescriptors" "(Z)[I" $ \[IValue (getSVal -> Just includeExc)] ->
        let filterExc PathState{ finalResult = fr } =
              if includeExc /= 0
                then True
                else case fr of Exc{} -> False; _ -> True
        in pushIntArr
             =<< mapM (liftSymbolic . termInt . fromIntegral . unPSS)
               =<< (M.keys . M.filter filterExc <$> gets pathStates)

      --------------------------------------------------------------------------------
      -- evalAig

      -- symbolic byte AIG output -> concrete byte
    , sym "evalAig" "(B[Lcom/galois/symbolic/CValue;)B" $ \[IValue out, RValue cvArr] ->
        evalAigBody (take 8) assertAllInts unIValue IValue out cvArr

      -- symbolic int AIG output -> concrete int
    , sym "evalAig" "(I[Lcom/galois/symbolic/CValue;)I" $ \[IValue out, RValue cvArr] ->
        evalAigBody id assertAllInts unIValue IValue out cvArr

      -- symbolic long AIG output -> concrete long
    , sym "evalAig" "(J[Lcom/galois/symbolic/CValue;)J" $ \[LValue out, RValue cvArr] ->
        evalAigBody id assertAllLongs unLValue LValue out cvArr

      -- symbolic byte array AIG output -> concrete byte array
    , sym "evalAig" "([B[Lcom/galois/symbolic/CValue;)[B" $
        evalAigArrayBody 8 assertAllInts getByteArray unIValue pushByteArr

      -- symbolic int array AIG output -> concrete int array
    , sym "evalAig" "([I[Lcom/galois/symbolic/CValue;)[I" $
        evalAigArrayBody 32 assertAllInts getIntArray unIValue pushIntArr

      -- symbolic long array AIG output -> long array
    , sym "evalAig" "([J[Lcom/galois/symbolic/CValue;)[J" $
        evalAigArrayBody 64 assertAllLongs getLongArray unLValue pushLongArr

      --------------------------------------------------------------------------------
      -- writeAiger

    , sym "writeAiger" "(Ljava/lang/String;Z)V"  $ \([RValue fnameRef, IValue out]) ->
        writeAigerBody (SV.take 1) fnameRef [out]
    , sym "writeAiger" "(Ljava/lang/String;B)V"  $ \([RValue fnameRef, IValue out]) ->
        writeAigerBody id fnameRef [out]
    , sym "writeAiger" "(Ljava/lang/String;I)V"  $ \([RValue fnameRef, IValue out]) ->
        writeAigerBody id fnameRef [out]
    , sym "writeAiger" "(Ljava/lang/String;J)V"  $ \([RValue fnameRef, LValue out]) ->
        writeAigerBody id fnameRef [out]
    , sym "writeAiger" "(Ljava/lang/String;[B)V" $ \([RValue fnameRef, RValue outs]) ->
        writeAigerBody id fnameRef =<< (`getByteArray` outs) =<< getPSS
    , sym "writeAiger" "(Ljava/lang/String;[I)V" $ \([RValue fnameRef, RValue outs]) ->
        writeAigerBody id fnameRef =<< (`getIntArray` outs) =<< getPSS
    , sym "writeAiger" "(Ljava/lang/String;[J)V" $ \([RValue fnameRef, RValue outs]) ->
        writeAigerBody id fnameRef =<< (`getLongArray` outs) =<< getPSS

      --------------------------------------------------------------------------------
      -- debugging

    , dbg "trace" "(I)V" $ \[v@IValue{}] -> dbugM $ ppValue v
    , dbg "trace" "(Ljava/lang/String;I)V" $ \[RValue msgRef, v@IValue{}] -> do
        mmsg <- lookupStringRef msgRef
        case mmsg of
          Just msg -> dbugM $ "(JSS) " ++ msg ++ ": " ++ ppValue v
          _ -> error "Symbolic.Debug.trace expects interned message strings"
    , dbg "trace" "(J)V" $ \[v@LValue{}] -> dbugM $ ppValue v
    , dbg "trace" "(Ljava/lang/String;J)V" $ \[RValue msgRef, v@LValue{}] -> do
        mmsg <- lookupStringRef msgRef
        case mmsg of
          Just msg -> dbugM $ "(JSS) " ++ msg ++ ": " ++ ppValue v
          _ -> error "Symbolic.Debug.trace expects interned message strings"

    , dbg "abort" "()V"          $ \_ -> abort "Abort explicitly triggered (via JAPI)."
    , dbg "dumpPathStates" "()V" $ \_ -> dumpPathStates
    , dbg "setVerbosity" "(I)V"  $ \[IValue (intVal -> Just v)] ->
        setVerbosity v
    , dbg "eval" "(I[Lcom/galois/symbolic/CValue;)I" $ \[IValue _out, RValue _cvArr] -> do
        error "debug dag eval / bitblast integrity checker NYI"
    ]
  where
    intVal = fmap fromInteger . getSVal
    abortWhenMultiPath msg = do
      finished <- snd <$> splitFinishedPaths
      when (length finished > 1) $ do
        dbugM $ "Error: " ++ msg ++ ": There are multiple non-exception paths.  Displaying all path states:"
        dumpPathStates
        abort $ msg ++ ": only permitted when there's one non-exception path state."

    sym meth md f = (symbolicCN, makeMethodKey meth md, f)
    dbg meth md f = (symbolicCN ++ "$Debug", makeMethodKey meth md, f)
    symbolicCN    = "com/galois/symbolic/Symbolic"
    m `pushAs` f  = pushValue =<< f <$> liftSymbolic m
    pushByteArr   = pushValue . RValue <=< newIntArray byteArrayTy
    pushIntArr    = pushValue . RValue <=< newIntArray intArrayTy
    pushLongArr   = pushValue . RValue <=< newLongArray longArrayTy
    printlnMthd t = ( "java/io/PrintStream"
                    , makeMethodKey "println" t
                    , \_ args -> printStream True (t == "(C)V") args
                    )
    printMthd t   = ( "java/io/PrintStream"
                    , makeMethodKey "print" t
                    , \_ args -> printStream False (t == "(C)V") args
                    )

    -- | Allows the user to append pretty-printed renditions of symbolic
    -- ints/longs as strings; however, we should REVISIT this.  Concatenation of
    -- the typical form form ("x = " + x) where x is symbolic is currently very
    -- inefficient since the concrete string representation of x is still
    -- executed through many array operations in the {Abstract,}StringBuilder
    -- implementations and so forth.  This leads to the odd situation where:
    --
    -- System.out.print("x = ");
    -- System.out.println(x);
    --
    -- is vastly more efficient than the equivalent concatenating version.
    appendIntegralMthd t =
      let cn = "java/lang/StringBuilder"
      in
        ( cn
        , makeMethodKey "append" t
        , \this [st] -> do
            let redir = makeMethodKey "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;"
                warn  = dbugM $
                  "Warning: string concatenation of symbolic variables is "
                    ++ "very inefficient in this release. \n  Consider using "
                    ++ "'System.out.print(\"x = \"); System.out.println(x);'"
                    ++ "\n  instead of 'System.out.println(\"x = \" + x); "
                    ++ "also see Symbolic.Debug.trace()."
            case st of
              IValue (getSVal -> Just{}) -> return ()
              IValue (getUVal -> Just{}) -> return ()
              IValue (getBool -> Just{}) -> return ()
              LValue (getSVal -> Just{}) -> return ()
              LValue (getUVal -> Just{}) -> return ()
              LValue (getBool -> Just{}) -> return ()
              _ -> warn
            sr        <- refFromString (ppValue st)
            Just meth <- (`lookupMethod` redir) <$> lookupClass cn
            runInstanceMethodCall cn meth this [RValue sr]
        )

    assertAllInts xs = do
      when (any (\x -> case x of IValue{} -> False; _ -> True) xs) $
        abort "JAPI: expected CValue operands to represent type int "
      return xs

    assertAllLongs xs = do
      when (any (\x -> case x of LValue{} -> False; _ -> True) xs) $
        abort "JAPI: expected CValue operands to represent type long "
      return xs

    evalAigBody f chkInps dtor ctor out cvArr = do
      abortWhenMultiPath "AIG evaluation (scalar)"
      cinps <- map (fromJust . termConst . dtor)
               <$> (chkInps =<< getInputs cvArr)
      evalAigIntegral f cinps out `pushAs` ctor

    evalAigArrayBody w chkInps getArr dtor push [RValue outs, RValue cvArr] = do
      abortWhenMultiPath "AIG evaluation (array)"
      pd      <- getPSS
      ins     <- chkInps =<< getInputs cvArr
      outLits <- getArr pd outs
      push =<< liftSymbolic
                 (evalAigArray w
                               (map (fromJust . termConst . dtor) ins)
                               outLits
                 )
    evalAigArrayBody _ _ _ _ _ _ = error "invalid evalAigArrayBody parameters"

    writeAigerBody f fnameRef outs = do
      abortWhenMultiPath "AIGER write"
      mfn <- lookupStringRef fnameRef
      case mfn of
        Nothing -> abort $ "writeAiger filename parameter does "
                         ++ "not refer to a constant string"
        Just fn -> liftSymbolic
                   $ writeAigToFile fn . SV.concat
                       =<< mapM (fmap (f . toLsbfV) . getVarLit) outs

    getInputs cvArr = do
      cvalRefs <- getPSS >>= (`getRefArray` cvArr)
      forM cvalRefs $ \r -> do
        mty <- typeOf r
        case mty of
          Nothing -> abort "Null reference encountered in concrete value array"
          Just ty
            | ty == ClassType ciCN -> unboxCV ciCN r "()I"
            | ty == ClassType czCN -> unboxCV czCN r "()Z"
            | ty == ClassType cjCN -> unboxCV cjCN r "()J"
            | ty == ClassType cbCN -> unboxCV cbCN r "()B"
            | otherwise ->
                abort $ "evalAig concrete value operand type is not supported"
      where
        cbCN              = "com/galois/symbolic/CValue$CByte"
        ciCN              = "com/galois/symbolic/CValue$CInt"
        czCN              = "com/galois/symbolic/CValue$CBool"
        cjCN              = "com/galois/symbolic/CValue$CLong"
        unboxCV cn ref td = do
          invokeInstanceMethod cn (makeMethodKey "getValue" td) ref []
          runFrame
          popValue

--------------------------------------------------------------------------------
-- PathState and State selector functions

-- | A "path state selector" (exported as opaque PathDescriptor)
newtype PSS a       = PSS{ unPSS :: a } deriving (Bounded, Eq, Functor, Ord, Show)
type PathDescriptor = PSS Int

onCurrPath :: MonadIO sym => ResumeAction sym -> Simulator sym ()
onCurrPath ra = modifyMF $ \mf ->
  mf { nextPaths = headf (nextPaths mf) $ \(path, _) -> (path, ra) }

-- | Split the current path (by duplicating it from the current path state), and
-- set the new path to be the next path executed after the current path.
onNewPath :: (MonadIO sym, PrettyTerm (MonadTerm sym)) =>
             ResumeAction sym -> Simulator sym ()
onNewPath ra = do
  ps     <- getPathState
  mf     <- getMergeFrame
  when (null (nextPaths mf)) $
    CE.assert (case finalResult ps of Breakpoint{} -> True ; _ -> False) $ return ()
  newPSS <- gets nextPSS
  let newPS = ps{ finalResult = Unassigned
                , pathStSel   = newPSS
                , insnCount   = 0
                }
  newPS `seq`
    modify $ \s ->
      s { nextPSS     = succ <$> newPSS
        , pathStates  = M.insert newPSS newPS (pathStates s)
        , mergeFrames = headf (mergeFrames s) $ \mf' ->
            mf'{ nextPaths = nextPaths mf' ++ [(newPSS, ra)] }
        }
  whenVerbosity (>3) $ do
    dbugM $ "Generated new path state: " ++ show newPSS
    dumpPathStates
  return ()

onResumedPath :: (MonadIO sym, PrettyTerm (MonadTerm sym)) =>
                 PathState (MonadTerm sym) -> ResumeAction sym -> Simulator sym ()
onResumedPath ps ra = do
  newPSS <- gets nextPSS
  case frames ps of
    f : _ -> do
      let newPS = ps { finalResult = Unassigned
                     , pathStSel   = newPSS
                     , startingPC  = frmPC f
                     , insnCount   = 0
                     }
      newPS `seq`
        modify $ \s ->
          s { nextPSS     = succ <$> newPSS
            , pathStates  = M.insert newPSS newPS (pathStates s)
            -- Note: there may be an issue here that the mergeFrames
            -- at the time of resumption is not compatible with what
            -- it was when encountering the breakpoint.
            , mergeFrames = headf (mergeFrames s) $ \mf ->
                mf{ nextPaths = nextPaths mf ++ [(newPSS, ra)] }
            }
      whenVerbosity (>3) $ do
        dbugM $ "Generated new path state: " ++ show newPSS
        dumpPathStates
      return ()
    _ -> error "onResumedPath called on PathState with empty call stack"

isPathFinished :: PathState m -> Bool
isPathFinished ps =
  case frames ps of
    [] -> True
    (f : _) -> S.member (frmClass f, methodKey (frmMethod f), frmPC f)
                        (breakpoints ps) &&
               -- Because breakpoints are typically used to cut loops,
               -- a path involing a breakpoint may start and end at
               -- the same instruction. So we don't want to terminate
               -- it as soon as it starts.
               (insnCount ps /= 0)
  || case finalResult ps of
       Unassigned -> False
       _          -> True

-- | Add assumption to current path.
assume :: AigOps sym => MonadTerm sym -> Simulator sym ()
assume v = do
  v' <- return v &&& psAssumptions <$> getPathState
  modifyPathState $ \ps -> ps{ psAssumptions = v' }

getPSS :: MonadIO sym => Simulator sym PathDescriptor
getPSS = pathStSel <$> getPathState

-- | Run the given computation using the path referent of the given path
-- descriptor.  Assumes that no paths are already finished and that the given
-- path is the only path upon which execution of the computation occurs.
withPathState :: MonadIO sym =>
                 PathDescriptor
              -> (PathState (MonadTerm sym) -> Simulator sym a)
              -> Simulator sym a
withPathState pd f = do
  oldMergeFrames <- gets mergeFrames
  modifyMF $ \mf -> mf{ finishedPaths = [], nextPaths = [(pd, NextInst)]}
  rslt <- f =<< getPathState
  modify $ \s -> s{ mergeFrames = oldMergeFrames }
  return rslt

getPathState :: MonadIO sym => Simulator sym (PathState (MonadTerm sym))
getPathState = do
  mf <- getMergeFrame
  let getIt pss = getPathStateByName pss >>= \ps ->
                    CE.assert (pathStSel ps == pss) (return ps)
  case mf of
    MergeFrame _ paths _ -> case paths of
      [] -> do
        -- Support access to path states on already-terminated paths, as our
        -- test driver code does in some places, but ensure that the
        -- finished path is unambiguous (i.e., there's only one after
        -- filtering out exception paths).

        finished <- snd <$> splitFinishedPaths
        case finished of
          [pss] -> getIt pss
          []    -> error $ "getPathState: no next path or finished paths "
                         ++ "in the active merge frame"
          _     -> error $ "getPathState: no next path and have multiple "
                         ++ " finished paths (merge failure?)"
      ((pss,_):_) -> getIt pss

getMergeFrame :: MonadIO sym => Simulator sym (MergeFrame sym)
getMergeFrame = do
  mfs <- gets mergeFrames
  case mfs of
    []     -> error "getMergeFrame: no merge frames"
    (mf:_) -> return mf

-- | Returns the number of nextPaths remaining; if @collapseOnAllFinished@ is
-- True, then if there are no nextPaths remaining, all finished paths will
-- migrate into the previous merge frame and the current merge frame is popped.
terminateCurrentPath :: MonadIO sym => Bool -> Simulator sym Int
terminateCurrentPath collapseOnAllFinished = do
  pss <- getPSS
  mf' <- getMergeFrame

  whenVerbosity (>=5) $ do
    dbugM $ "Terminating path " ++ show pss ++ " in current MF, and dumping merge frames:"
    dumpMergeFrames

  CE.assert (let (ps,_):_ = nextPaths mf' in ps == pss) $ return ()
  CE.assert ((elem pss $ map fst (nextPaths mf'))
             && not (elem pss $ finishedPaths mf')) $ return ()

  -- Move the current PSS from 'nextPaths' to 'finishedPaths'
  modifyMF $ \mf ->
    mf{ finishedPaths = pss : finishedPaths mf
      , nextPaths     = drop 1 $ nextPaths mf
      }

  remaining <- length . nextPaths <$> getMergeFrame

  when (collapseOnAllFinished && remaining == 0) $ modify $ \s ->
    s{ mergeFrames = case mergeFrames s of
      [_]      -> mergeFrames s
      (mf:mfs) -> CE.assert (null . nextPaths $ mf) $
                    headf mfs $ \mf'' ->
                      mf''{ finishedPaths = finishedPaths mf
                                            ++ finishedPaths mf''
                          }
      _ -> error "Empty MergeFrame list in terminateCurrentPath"
     }
  return remaining
    <* (getMergeFrame >>= \_mf ->
          whenVerbosity (>=4) $ do
            dbugM $ "terminateCurrentPath (post): dumping merge frames"
            dumpMergeFrames
       )


-- | Register breakpoints at each of a list of (class, method, PC)
-- tuples.
registerBreakpoints :: MonadIO sym
                    => [(String, MethodKey, PC)]
                    -> Simulator sym ()
registerBreakpoints bkpts =
  modifyPathState (\ps -> ps { breakpoints = S.fromList bkpts })

-- | Set the appropriate finalResult if we've stopped at a breakpoint.
-- assertion.
handleBreakpoints :: MonadIO sym => Simulator sym ()
handleBreakpoints = do
  ps <- getPathState
  case frames ps of
    f : _ ->
      when (S.member (frmClass f, methodKey (frmMethod f), frmPC f)
             (breakpoints ps)) $
        modifyPathState (setFinalResult . Breakpoint . frmPC $ f)
    _ -> return ()

resumeBreakpoint :: (MonadIO sym, PrettyTerm (MonadTerm sym))
                 => PathState (MonadTerm sym)
                 -> Simulator sym ()
resumeBreakpoint ps = onResumedPath ps NextInst

getPathStateByName :: MonadIO sym =>
                      PathDescriptor -> Simulator sym (PathState (MonadTerm sym))
getPathStateByName n = do
  mps <- lookupPathStateByName n
  case mps of
    Nothing -> error $ "internal: pss not in pathstate list: " ++ show n
    Just ps -> return ps

lookupPathStateByName :: MonadIO sym =>
                         PathDescriptor
                      -> Simulator sym (Maybe (PathState (MonadTerm sym)))
lookupPathStateByName n = M.lookup n . pathStates <$> get

putPathState :: MonadIO sym =>
                PathState (MonadTerm sym) -> Simulator sym ()
putPathState ps = ps `seq` do
  pss <- getPSS
  -- never permit selector tweaking
  CE.assert (pathStSel ps == pss) $ return ()
  modify $ \s -> s{ pathStates = M.insert pss ps (pathStates s) }

modifyPathState :: MonadIO sym =>
                   (PathState (MonadTerm sym) -> PathState (MonadTerm sym))
                -> Simulator sym ()
modifyPathState f = f <$> getPathState >>= putPathState

-- | (updateFrame fn) replaces the top frame (head f) in the current
-- path state with (fn (head f)).
updateFrame :: MonadIO sym =>
               (Frame (MonadTerm sym) -> Frame (MonadTerm sym))
            -> Simulator sym ()
updateFrame fn = {-# SCC "uF" #-} do
  ps <- getPathState
  case frames ps of
    top : rest -> do
      let newTop = fn top
          ps' = newTop `seq` ps{ frames = newTop : rest }
      ps' `seq` putPathState ps'
    _ -> error "internal: updateFrame: frame list is empty"

-- (pushFrame st m vals) pushes frame to the stack.
pushFrame :: MonadIO sym =>
             String
          -> Method
          -> [Value (MonadTerm sym)]
          -> Simulator sym ()
pushFrame name method vals = do
  -- Allow an unambiguous finished path to be "reactivated" whenever a new frame
  -- is pushed.
--   dumpFrameInfo
  modifyMF $ \mf ->
    case (nextPaths mf, finishedPaths mf) of
      ([], [unambig]) -> mf { finishedPaths = []
                            , nextPaths     = [(unambig, NextInst)]
                            }
      _ -> mf

  -- Push the new Java frame record
  modifyPathState $ \ps ->
    ps{ frames = Call name method 0 localMap [] : frames ps }

  -- Push the "merge frame" record
  modify $ \s -> s { mergeFrames = pushMF (mergeFrames s) }
  where
    localMap = M.fromList (snd $ foldl setupLocal (0, []) vals)
    setupLocal (n, acc) v@LValue{} = (n + 2, (n, v) : acc)
    setupLocal (n, acc) v@DValue{} = (n + 2, (n, v) : acc)
    setupLocal (n, acc) v          = (n + 1, (n, v) : acc)
    pushMF (currMF:mfs) =
      -- Migrate the currently executing path out of the active merge frame and
      -- and onto the merge frame for the new call.
      case nextPaths currMF of
        ((currPath,_):rest) ->
          mergeFrame [(currPath, NextInst)]
          : currMF{ nextPaths = rest }
          : mfs
        _ -> error "pushFrame: no current path"
    pushMF _ = error "pushFrame: empty MergeFrame stack"

-- | Returns a new reference value with the given type, but does not
-- initialize and fields or array elements.
-- Note: Assumes class for reference has already been initialized.
genRef :: MonadIO sym => Type -> Simulator sym Ref
genRef tp = do
  s <- get
  let r = nextRef s
  put s { nextRef = r + 1 }
  return $ Ref r tp

-- | Returns initialization status of class
getInitializationStatus :: MonadIO sym =>
                           String -> Simulator sym (Maybe InitializationStatus)
getInitializationStatus cName = do
  ps <- getPathState
  return $ M.lookup cName (initialization ps)

-- | Sets initialization status of class with given name.
setInitializationStatus :: MonadIO sym =>
                           String -> InitializationStatus -> Simulator sym ()
setInitializationStatus cName status =
  modifyPathState $ \ps ->
    ps { initialization = M.insert cName status (initialization ps) }

setFinalResult :: FinalResult term -> PathState term -> PathState term
setFinalResult fr ps = ps { finalResult = fr }

popFrameImpl :: MonadIO sym => Bool -> Simulator sym ()
popFrameImpl isNormalReturn = do
  ps <- getPathState
  let (Call cName method _ _ _):_ = frames ps
  -- dbugM $ "popFrame: popping activation record for " ++ cName ++ "." ++ (methodName method)
  putPathState
    $ case methodName method of
        "<clinit>" ->
          ps { frames = (tail . frames) ps
             , initialization = M.insert cName
                                (if isNormalReturn then Initialized else Erroneous)
                                (initialization ps)
             }
        _ -> ps { frames = (tail . frames) ps }

-- Returns number of frames in current state.
frameCount :: MonadIO sym => Simulator sym Int
frameCount = length . frames <$> getPathState

runFrame :: AigOps sym => Simulator sym ()
runFrame = do
--   dbugM $ "======================= runFrame ======================"
  initialCount <- frameCount
  let loop = do curCount <- frameCount
                if curCount < initialCount
                  then return ()
                  else do
                    stepCommon loop (return ())
  loop

-- REVISIT: it may make sense to make this custom initialization
-- dynamic, and have a table of custom-initialized classes in the
-- simulator state.
runCustomClassInitialization :: AigOps sym => Class -> Simulator sym ()
runCustomClassInitialization cl =
  case cname of
    "java/lang/System" -> do
      initializeClass pstreamName
      outStream <- RValue `liftM` genRef (ClassType pstreamName)
      errStream <- RValue `liftM` genRef (ClassType pstreamName)
      setStaticFieldValue (FieldId cname "out" pstreamType) outStream
      setStaticFieldValue (FieldId cname "err" pstreamType) errStream
    _ -> return ()
  where cname = className cl
        pstreamName = "java/io/PrintStream"
        pstreamType = ClassType pstreamName

-- REVISIT: it may make sense for this to be dynamic
skipInit :: String -> Bool
skipInit cname = cname `elem` [ "java/lang/System"
                              , "java/io/Reader"
                              , "java/io/InputStreamReader"
                              ]

--------------------------------------------------------------------------------
-- Symbolic simulation semantics

type instance JSDouble (Simulator sym) = Double
type instance JSFloat  (Simulator sym) = Float
type instance JSInt    (Simulator sym) = MonadTerm sym
type instance JSLong   (Simulator sym) = MonadTerm sym
type instance JSRef    (Simulator sym) = Ref
type instance JSBool   (Simulator sym) = MonadTerm sym
type instance JSRslt   (Simulator sym) = [(PathDescriptor, FinalResult (MonadTerm sym))]

instance (AigOps sym) => JavaSemantics (Simulator sym) where
  -- Control functions {{{1

  isFinished = and . map isPathFinished . M.elems . pathStates <$> get

  fork (getBool -> Just True) trueM _   = trueM
  fork (getBool -> Just False) _ falseM = falseM
  fork v trueM falseM                   = do
    let splitPaths = do
          v' <- bNot v
          onNewPath  $ CustomRA "falseM" $ assume v' >> falseM >> return NextInst
          onCurrPath $ CustomRA "trueM"  $ assume v  >> trueM  >> return NextInst

    -- NB: We permit explicit bitblasting when the 'alwaysBitBlastBranchTerms'
    -- flag is true.  This can help addresses some symbolic termination
    -- problems, e.g. the 'recursive multiplier' problem (see the mul2 function
    -- in test/src/support/PathStateMerges.java).

    blast <- gets $ alwaysBitBlastBranchTerms . simulationFlags
    if blast
      then do
        be <- liftSymbolic $ getBitEngine
        LV lv           <- liftSymbolic $ getVarLit v
        v'              <- CE.assert (SV.length lv == 1) $ return $ SV.head lv
--         dbugM $ "v = \n" ++ prettyTerm v
        case () of () | v' == beTrue be  -> trueM
                      | v' == beFalse be -> falseM
                      | otherwise    -> splitPaths
      else do
        splitPaths

  -- | Resolve the monadic condition to exactly one of its branches
  singleForkM condM trueM falseM = do
    cond <- condM
    case getBool cond of
      Just True  -> trueM
      Just False -> falseM
      _ -> error "singleForkM: Failed to resolve to single branch"

  -- Returns one result per control flow path ; if all paths yielded Java
  -- exceptions, we die and inform the user.
  getResult = do
    rslts <- M.map (finalResult CA.&&& frames) <$> gets pathStates
    let rs = map (second fst) (M.assocs rslts)
    when (all isExc $ map snd rs) $ Simulation.throwExternal (msgs rs) rslts
    return rs
    where
      -- TODO: Append the result of e.toString() or somesuch when printing an
      -- exception e
      msgs rs     =
        "All execution paths yielded exceptions.\n"
        ++ concatMap
             (\(pd, r) -> ppExcMsg (length rs > 1) pd (ppSimulatorExc $ getExc r))
             rs

  -- Returns current class name
  getCurrentClassName = do
    ps <- getPathState
    case frames ps of
      Call cl _ _ _ _ : _ -> return cl
      _ -> error "Frames are empty"

  getCurrentMethod = do
    ps <- getPathState
    case frames ps of
      Call _ m _ _ _ : _ -> return m
      _ -> error "internal: getCurrentMethod called outside a valid call frame."

  getPc = do
    ps <- getPathState
    case frames ps of
      Call _ _ pc _ _ : _ -> return pc
      _ -> error "internal: getPc called outside a valid call frame"

  setPc newPc = updateFrame (\(Call cl m _pc locals s) -> Call cl m newPc locals s)

  initializeClass name = do
    ps <- getPathState
    case M.lookup name (initialization ps) of
      Nothing -> do
        cl <- lookupClass name
        let initializeField f =
              let fieldId = FieldId name (fieldName f) (fieldType f)
              in case fieldConstantValue f of
                  Just (Double v) ->
                    setStaticFieldValue fieldId . DValue =<< dConst v
                  Just (Float v) ->
                    setStaticFieldValue fieldId . FValue =<< fConst v
                  Just (Integer v) ->
                    setStaticFieldValue fieldId . IValue =<< iConst v
                  Just (Long v) ->
                    setStaticFieldValue fieldId . LValue =<< lConst v
                  Just (String v) ->
                    setStaticFieldValue fieldId . RValue =<< refFromString v
                  Nothing ->
                    if fieldIsStatic f
                      then setStaticFieldValue fieldId (defaultValue (fieldType f))
                      else return ()
                  Just tp -> error $ "Unsupported field type" ++ show tp
        mapM_ initializeField $ classFields cl
        case cl `lookupMethod` (MethodKey "<clinit>" [] Nothing) of
          Just method -> do
            setInitializationStatus name Started
            unless (skipInit name) $ do

              pushFrame name method []
              runFrame
          Nothing -> return ()
        runCustomClassInitialization cl
        setInitializationStatus name Initialized
      Just Erroneous -> do
        createAndThrow "java/lang/NoClassDefFoundError"
      Just Started -> return ()
      Just Initialized -> return ()

  -- Pushes an instance method call.

  pushInstanceMethodCall cName method objectref operands = do
    overrides <- gets instanceOverrides
    case M.lookup (cName,methodKey method) overrides of
      Just a -> a objectref operands
      Nothing ->
        if methodIsNative method then
          error $ "Unsupported native instance method "
                  ++ show (methodKey method) ++ " in " ++ cName
                  ++ " (this: " ++ show objectref ++ ")"
        else do
          -- dbugM $ "pushFrame: (instance) method call to " ++ cName ++ "." ++ methodName method
          pushFrame cName method (RValue objectref : operands)

  runInstanceMethodCall cName method objectref operands = do
    pushInstanceMethodCall cName method objectref operands
    runFrame

  pushStaticMethodCall cName method operands = do
    let mk = methodKey method
    overrides <- gets staticOverrides
    case M.lookup (cName, mk) overrides of
      Just a -> a operands
      Nothing ->
        if methodIsNative method then
          error $ "Unsupported native static method " ++ show mk ++ " in " ++ cName
        else do
          when (cName == "com/galois/symbolic/Symbolic") $
            expectOverride "Symbolic" mk
          when (cName == "com/galois/symbolic/Symbolic$Debug") $
            expectOverride "Symbolic$Debug" mk
          -- dbugM $ "pushFrame: (static) method call to " ++ cName ++ "." ++ methodName method
          pushFrame cName method operands
    where
      expectOverride cn mk =
        error $ "expected static override for " ++ cn ++ "."
              ++ methodName method ++ unparseMethodDescriptor mk

  execReturn Nothing = do
    (Call cn (methodName -> mn) _ _ _):_ <- frames <$> getPathState
    popFrameImpl True
    unless ("<clinit>" == mn || "<init>" == mn || "init" == mn) $ do
      ps <- getPathState
      when (isPathFinished ps) $ putPathState ps{ finalResult = Terminated }
    doMerge cn mn
  execReturn (Just val) = do
    (Call cn (methodName -> mn) _ _ _):_ <- frames <$> getPathState
    popFrameImpl True
    ps <- getPathState
    if isPathFinished ps
      then putPathState ps{ finalResult = ReturnVal val }
      else pushValue val
    doMerge cn mn

  -- Discard exception states
  throw ref = CE.throw =<< JavaException ref . frames <$> getPathState

  fatal = abort

  -- Negate a Boolean value
  bNot = liftSymbolic . applyBNot

  -- (x &&& y) returns logical and
  mx &&& my = do
    x <- mx
    case getBool x of
      Just True -> my
      Just False -> return x
      _ -> do
        y <- my
        case getBool y of
          Just True -> return x
          Just False -> return y
          _ -> liftSymbolic $ applyBAnd x y

  -- Conversions
  floatFromDouble  = return . fromRational . toRational
  intFromDouble    = return . mkCInt 32 . truncate
  longFromDouble   = return . mkCInt 64 . truncate
  doubleFromFloat  = return . fromRational . toRational
  intFromFloat     = return . mkCInt 32 . truncate
  longFromFloat    = return . mkCInt 64 . truncate
  doubleFromInt  i =
    case getSVal i of
      Just n -> return (fromIntegral n)
      _ -> error "cannot convert symbolic int to double"
  floatFromInt   i =
    case getSVal i of
      Just n -> return (fromIntegral n)
      _ -> error "cannot convert symbolic int to float"
  doubleFromLong l =
    case getSVal l of
      Just n -> return (fromIntegral n)
      _ -> error "cannot convert symbolic long to double"
  floatFromLong  l =
    maybe (error "cannot convert symbolic long to float")
          (return . fromInteger)
          (getSVal l)



  -- Double operations
  dAdd  x y = return $ x + y
  dCmpg x y = return $ compareFloat x y

  dCmpl x y = return $ compareFloat x y
  dConst    = return
  dDiv  x y = return $ x / y
  dMul  x y = return $ x * y
  dNeg      = return . negate
  dRem  x y = return $ floatRem x y
  dSub  x y = return $ x - y

  -- Float operations
  fAdd  x y = return $ x + y
  fCmpg x y = return $ compareFloat x y
  fCmpl x y = return $ compareFloat x y
  fConst    = return
  fDiv  x y = return $ x / y
  fMul  x y = return $ x * y
  fNeg      = return . negate
  fRem  x y = return $ floatRem x y
  fSub  x y = return $ x - y

  -- Integer functions {{{1
  iAdd  x y = liftSymbolic $ applyAdd x y
  iAnd  x y = liftSymbolic $ applyIAnd x y
  iConst    = return . mkCInt 32 . toInteger
  iDiv  x y = liftSymbolic $ applySignedDiv x y
  iEq   x y = liftSymbolic $ applyEq x y
  iLeq  x y = liftSymbolic $ applySignedLeq x y
  iMul  x y = liftSymbolic $ applyMul x y
  iNeg  x   = liftSymbolic $ applyNeg x
  iOr   x y = liftSymbolic $ applyIOr x y
  iRem  x y = liftSymbolic $ applySignedRem x y
  iShl  x y = liftSymbolic $ applyShl x =<< applyTrunc 5 y
  iShr  x y = liftSymbolic $ applyShr x =<< applyTrunc 5 y
  iSub  x y = liftSymbolic $ applySub x y
  iUshr x y = liftSymbolic $ applyUshr x =<< applyTrunc 5 y
  iXor  x y = liftSymbolic $ applyIXor x y

  ------------------------------------------------------------------------------
  -- operations on longs
  lAdd x y = liftSymbolic $ applyAdd x y
  lAnd x y = liftSymbolic $ applyIAnd x y


  lCmp x y = liftSymbolic $ do
    eqXY   <- applyEq x y
    ltXY   <- applySignedLt x y
    negRes <- applyIte ltXY (mkCInt 32 (-1)) (mkCInt 32 1)
    applyIte eqXY (mkCInt 32 0) negRes

  lConst    = return . mkCInt 64 . toInteger
  lEq x y   = liftSymbolic $ applyEq x y
  lDiv x y  = liftSymbolic $ applySignedDiv x y
  lMul x y  = liftSymbolic $ applyMul x y
  lNeg x    = liftSymbolic $ applyNeg x
  lOr   x y = liftSymbolic $ applyIOr x y
  lRem  x y = liftSymbolic $ applySignedRem x y
  lShl  x y = liftSymbolic $ applyShl x =<< applyTrunc 6 y
  lShr  x y = liftSymbolic $ applyShr x =<< applyTrunc 6 y
  lSub  x y = liftSymbolic $ applySub x y
  lUshr x y = liftSymbolic $ applyUshr x =<< applyTrunc 6 y
  lXor  x y = liftSymbolic $ applyIXor x y

  --------------------------------------------------------------------------------
  -- Conversions
  longFromInt x = liftSymbolic $ applySignedExt 64 x
  intFromLong x = liftSymbolic $ applyTrunc 32 x

  -- (arrayLength ref) return length of array at ref.
  arrayLength ref = do
    pd  <- getPSS
    fmap (mkCInt 32 . toInteger) $ getArrayLength pd ref


  -- (newMultiArray tp len) returns a reference to a multidimentional array with
  -- type tp and len = [len1, len2, ...] where len1 equals length of first
  -- dimention len2 equals length of second dimension and so on.  Note: Assumes
  -- all integer values are nonnegative.
  newMultiArray tp []
    | isRValue tp = return NullRef
  newMultiArray tp@(ArrayType eltType) [getSVal -> Just cnt]
    | (isIValue eltType) || (eltType == LongType) = do
      ref <- genRef tp
      let width = if eltType == LongType then 64 else 32
      arr <- liftSymbolic $
        symbolicArrayFromList (SymInt (constantWidth width)) $
          replicate (fromIntegral cnt) (mkCInt width 0)
      modifyPathState $ \ps ->
        ps { arrays = M.insert ref (fromIntegral cnt, arr) (arrays ps) }
      return ref
    | eltType == DoubleType =
        abort "Floating point arrays (e.g., double) are not supported"
    | eltType == FloatType  =
        abort "Floating point arrays (e.g., float) are not supported"
  newMultiArray tp@(ArrayType eltTp) ((getSVal -> Just cnt) : rest) = do
    ref <- genRef tp
    values <- replicateM (fromIntegral cnt) (newMultiArray eltTp rest)
    let arr = listArray (0, fromIntegral cnt-1) values
    modifyPathState $ \ps ->
      ps { refArrays = M.insert ref arr (refArrays ps) }
    return ref
  newMultiArray _ _ = abort "Cannot create array with symbolic size"

  newObject name = do
   initializeClass name
   ref <- genRef (ClassType name)
   -- Set fields to default value
   fields <- classFields <$> lookupClass name
   modifyPathState $ \ps ->
     ps { instanceFields =
            foldl' (\fieldMap f ->
                      let fid = FieldId name (fieldName f) (fieldType f)
                          val = defaultValue (fieldType f)
                       in val `seq` M.insert (ref,fid) val fieldMap)
                   (instanceFields ps)
                   fields
        }
   return ref

  isValidEltOfArray elt arr = do
    if elt == NullRef
      then return (mkCBool True)
      else do
        ArrayType arrayTy <- getType arr
        elTy              <- getType elt
        mkCBool <$> isSubtype elTy arrayTy

  hasType ref tp = mkCBool <$> ((`isSubtype` tp) =<< getType ref)

  typeOf NullRef    = return Nothing
  typeOf (Ref _ ty) = return (Just ty)

  -- Retuns predicate indicating super class of ref has given type.
  superHasType ref tp = do
    ClassType refClassname <- getType ref
    cl                     <- lookupClass refClassname
    mkCBool
      <$> case superClass cl of
            Just super -> isSubtype (ClassType super) (ClassType tp)
            Nothing    -> return False

  -- (rEq x y) returns boolean formula that holds if x == y.
  rEq x y = return $ mkCBool $ x == y

  -- rNull returns node representing null pointer.
  rNull = return NullRef

  -- Returns reference for given string constant.
  -- NOTE: Assumes string comes from constant pool of an initialized class.
  refFromString val = do
    s <- get
    case M.lookup val (strings s) of
      Just ref -> return ref
      Nothing  -> do
        ref <- newString val
        modify $ \s' -> s'{ strings = M.insert val ref (strings s') }
        return ref

  getClassObject cname = do
    ps <- getPathState
    case M.lookup cname (classObjects ps) of
      Just ref -> return ref
      Nothing -> do
        ref <- newClass cname
        modifyPathState $ \ps' ->
          ps' { classObjects = M.insert cname ref (classObjects ps') }
        return ref

  -- Heap related functions {{{1

  -- Pushes value of field onto stack.
  -- NOTE: Assumes ref is not null.
  pushInstanceFieldValue r fieldId = do
    ps <- getPathState
    case M.lookup (r, fieldId) (instanceFields ps) of
      Just value -> pushValue value
      -- Some code seems to depend on instance fields being
      -- initialized to their default value.
      Nothing    ->
        if isFloatType (fieldIdType fieldId)
          then (error $ "internal: unassigned floating-point instance field " ++
                       show fieldId ++ " in " ++ show r)
          else (pushValue $ defaultValue $ fieldIdType fieldId)

  -- Pushes value of field onto stack.
  -- NOTE: Assumes ref is not null.
  pushStaticFieldValue fieldId = do
    ps <- getPathState
    case M.lookup fieldId (staticFields ps) of
      Just value -> pushValue $ value
      Nothing    ->
        if isFloatType (fieldIdType fieldId)
          then (error $ "internal: unassigned static floating-point field " ++
                        show fieldId)
          else (pushValue $ defaultValue $ fieldIdType fieldId)

  -- (pushArrayValue ref index) pushes the value of the array at index to the stack.
  -- NOTE: Assumes that ref is a valid array and index is a valid index in array.
  pushArrayValue r idx = do
    pd  <- getPSS
    val <- getArrayValue pd r idx
    pushValue val

  setArrayValue r idx (IValue val) = updateSymbolicArray r $ \arr ->
    applySetArrayValue arr idx val
  setArrayValue r idx (LValue val) = updateSymbolicArray r $ \arr ->
    applySetArrayValue arr idx val
  setArrayValue r (getSVal -> Just i) (RValue v) = do
    ps <- getPathState
    let updateFn arr = Just (arr // [(fromIntegral i,v)])
        ps'          = ps{ refArrays = M.update updateFn r (refArrays ps) }
    refArrays ps' M.! r `seq` putPathState ps'

  setArrayValue _ _ (RValue _) = do
    abort "Cannot update reference arrays at symbolic indices"
  setArrayValue _ _ _ =
    error "internal: invalid setArrayValue parameters (array type/elem mismatch?)"

  setInstanceFieldValue r fieldId v = modifyPathState $ \ps ->
      ps{ instanceFields = v `seq` M.insert (r,fieldId) v (instanceFields ps) }

  setStaticFieldValue fieldId v = modifyPathState $ \ps ->
    ps{ staticFields = M.insert fieldId v (staticFields ps) }

  -- Pop value off top of stack.
  popValue = do
    ps <- getPathState
    when (null $ frames ps) $ error $ "Bad path state: no frame:\n " ++ show ps
    case frames ps of
      Call st m pc lvars (top : vals) : tailFrames
          -> do putPathState ps{ frames = Call st m pc lvars vals : tailFrames }
                return top
      _ -> error $ "Unexpected path state (empty operand stack):\n " ++ show ps

  -- Push value onto top of stack.
  pushValue val = updateFrame (\(Call st m pc lvars stack) -> Call st m pc lvars (val : stack))

  -- Local variable functions {{{1
  getLocal i = do
    ps <- getPathState
    case frames ps of
      _cl@(Call _ _ _ lvars _) : _ -> do
        case M.lookup i lvars of
          Just lcl -> return lcl
          Nothing -> do
            dbugM $ unlines
              $ "stack dump:" : [ show (c,methodKey m, pc,vm,st)
                                | Call c m pc vm st <- frames ps
                                ]
            error "Undefined local"
      _ -> error "Frames are empty"

  setLocal i v = do
    ps <- getPathState
    let Call st m pc lvars stack : rest = frames ps
    putPathState ps { frames = Call st m pc (M.insert i v lvars) stack : rest
                    }

  printStream nl _ []       = liftIO $ (if nl then putStrLn else putStr) "" >> hFlush stdout
  printStream nl binary [x] = do
    let putStr' s = liftIO $ (if nl then putStrLn else putStr) s >> hFlush stdout
    case x of
      IValue (getSVal -> Just v) -> if binary then putStr' [chr $ fromEnum v]
                                              else putStr' $ show v
      v@IValue{} -> putStr' $ ppValue v

      LValue (getSVal -> Just v) -> putStr' $ show v
      v@LValue{} -> putStr' $ ppValue v
      FValue f -> putStr' (show f)
      DValue d -> putStr' (show d)
      RValue r -> do
        ms <- lookupStringRef r
        case ms of
          Just str  -> putStr' str
          Nothing   -> do
            let key = makeMethodKey "toString" "()Ljava/lang/String;"
            dynBind "java/lang/Object" key r $ \cName -> do
              invokeInstanceMethod cName key r []
              runFrame
              putStr' =<< drefString =<< unRValue <$> popValue
      _ -> abort $ "Unable to display values of type other than "
                 ++ "int, long, and reference to constant string"
  printStream _ _ _ = abort $ "Unable to print more than one argument"

  die = abort

  doStep = do
    done <- isPathFinished <$> getPathState
    let term = terminateCurrentPath True >> return ()
    if done
      then handleBreakpoints >> term
      else stepCommon doStep term

stepCommon :: AigOps sym => Simulator sym a -> Simulator sym a -> Simulator sym a
stepCommon onOK onException = do
  method <- getCurrentMethod
  pc     <- getPc
  let inst    = lookupInstruction method pc
      dbugFrm = whenVerbosity (>=5) $ do
                  frms <- frames <$> getPathState
                  case frms of
                    []      -> dbugM $ "  (no frame)"
                    (frm:_) -> dbugM ("  " ++ ppFrame frm)
      count = modifyPathState $ \ps -> ps { insnCount = insnCount ps + 1 }
  whenVerbosity (>=2) $ do
    pss <- getPSS
    dbugM $ "Executing (" ++ show pss ++ ") " ++ show pc ++ ": " ++ ppInst inst

  do mf <- getMergeFrame
     case mf of
       MergeFrame _ paths _ -> case paths of
         [] -> error "stepCommon: no next path in merge frame"
         ((_,ra):_) -> case ra of
           NextInst           -> step inst >> count -- Run normally
           CustomRA _desc act -> onCurrPath =<< act -- Run overridden sequence
     dbugFrm
     onOK
  `catchMIO` \e ->
    case CE.fromException e of
      Just e' -> do
        -- The stepper has thrown an exception, so mark this path terminated
        modifyPathState (setFinalResult $ Exc e') >> onException
      _ -> CE.throw e

--------------------------------------------------------------------------------
-- Path state merging

doMerge :: AigOps sym => String -> String -> Simulator sym ()
doMerge _clNm _methNm = do
  topMF <- getMergeFrame
  case breakpoint topMF of
    CallExit -> do
      remainingPaths <- terminateCurrentPath False
                        -- ^ don't collpase frames, because we need to continue
                        -- execution on the merged path
      when (remainingPaths == 0) $ do
        mergeFinishedPaths

        -- Set the current path of the previous merge frame to the merged path
        -- of the active merge frame, and pop the active merge frame.  We expect
        -- that there should be only one finished path (that didn't raise an
        -- exception or hit a breakpoint) in the active merge frame.  This
        -- happens so that execution continues on the merged path.
        (excbps, finished) <- splitFinishedPaths
        case finished of
          [path] -> do
            modify $ \s ->
              s { mergeFrames = case mergeFrames s of
                    (_:mfs) -> headf mfs $ \mf ->
                                 mf{ finishedPaths = excbps ++ finishedPaths mf
                                   , nextPaths     = (path, NextInst) : nextPaths mf
                                   }
                    _ -> error "doMerge: malformed merge frame stack"
                }
--             -- DBUG
--             pathCond <- psAssumptions <$> getPathState
--             dbugM $ "Finished merging " ++ clNm ++ "." ++ methNm
--             dbugM $ "Final merged path condition is: " ++ ppSymTermDflt pathCond
--             dumpPathStates
--             -- DBUG
          _ -> error $ "doMerge: expected top merge frame "
                     ++ "to have exactly one (non-exception/bp) finished path"
        whenVerbosity (>=5) $ do
          dbugM $ "doMerge: post-merge merge frames:"
          dumpMergeFrames
    _ -> error "doMerge: unsupported breakpoint type"

mergeFinishedPaths :: AigOps sym => Simulator sym ()
mergeFinishedPaths = do
  topMF <- getMergeFrame
  when (length (nextPaths topMF) > 0) $ do
    error "mergeFinishedPaths: not all paths have finished executing"

  case finishedPaths topMF of
    []    -> error "mergeFinishedPaths: no finished paths"
    [_]   -> return ()
    _paths -> do
      -- Paths that have thrown an exception or are at a breakpoint are never
      -- merge candidates, so they are partitioned out here before attempting
      -- any merging.
      (excbps, paths')      <- splitFinishedPaths
      (unmergable, mmerged) <- foldM mrg ([], Nothing) paths'
      if null unmergable
        then do
          case mmerged of
            Nothing -> -- This should be unreachable
              error "mfp: no unmergable states and no merged states?"
            Just merged ->
              modifyMF $ \mf ->
                mf { finishedPaths = merged : excbps }
        else do
          -- Any paths that were merged should no longer be present in the
          -- finished paths list of the active merge frame, save the single
          -- merged state.  Unmergable states (and exception states) remain
          -- there.
          modifyMF $ \mf ->
            mf{ finishedPaths = unmergable ++ excbps ++ maybe [] (:[]) mmerged }
      return ()
  where
    -- Merge the path state given by pss0 into the path state given by pss1.
    mrg :: AigOps sym
        => ([PSS Int], Maybe (PSS Int))
        -> PSS Int
        -> Simulator sym ([PSS Int], Maybe (PSS Int))
    mrg (unmergable, mpss1) pss0 =
      case mpss1 of
        Nothing   -> return (unmergable, Just pss0)
        Just pss1 -> do
          s0  <- getPathStateByName pss0
          s1  <- getPathStateByName pss1
          ms2 <- merge s0 s1
          case ms2 of
            Nothing -> return (pss0 : pss1 : unmergable, Nothing)
            Just s2 -> do
              CE.assert (pathStSel s1 == pathStSel s2) $ return ()
              whenVerbosity (>=3) $ do
                dbugM $ "Merging " ++ show pss0 ++ " and " ++ show pss1
              -- Update the path state map with the merged path state
              modify $ \s ->
                s{ pathStates = M.update (const $ Just s2) pss1
                              $ M.delete pss0
                              $ pathStates s
                 }
              return (unmergable, Just $ pathStSel s2)

data MergePrecond
  = ArraySizesMatch
  | RefArraysEqual
  | StackFramesAlign
    deriving Show

merge :: forall sym. (AigOps sym)
      => PathState (MonadTerm sym)
      -> PathState (MonadTerm sym)
      -> Simulator sym (Maybe (PathState (MonadTerm sym)))
merge from@PathState{ finalResult = fromFR } to@PathState{ finalResult = toFR } = do
  -- A path that has thrown an exception should not have been identified as a
  -- merge candidate!
  CE.assert (not (isExc fromFR) && not (isExc toFR)) $ return ()

  mviols  <- checkPreconds
  mergeOK <- case mviols of
               Nothing    -> return True
               Just viols -> do
                 -- We should never be actually merging without the
                 -- preconditions satisfied, so this is an error.  However,
                 -- since there's sometimes no harm in not merging, this can
                 -- be softened to a warning if we discover that our merge
                 -- point selection approach isn't working well enough.
                 error $ "Violated path state merge preconditions: " ++ show viols
  if mergeOK
    then do
      whenVerbosity (>=4) $ do
        cnt <- gets $ M.size . pathStates
        dbugM $ "merge: "
                ++ show (unPSS $ pathStSel from)
                ++ " => "
                ++ show (unPSS $ pathStSel to)
                ++ " (" ++ show cnt ++ " states before merge)"
        whenVerbosity (>=5) $ do
          dbugM $ "From state:" ++ show from
          dbugM $ "To state:" ++ show to

      -- Actually do the merge
      (\fr pc ar rar iflds sflds frms ->
         Just to{ finalResult    = fr
                , psAssumptions  = pc
                , arrays         = ar
                , refArrays      = rar
                , instanceFields = iflds
                , staticFields   = sflds
                , frames         = frms
                }
       ) <$> mergeRVs
         <*> pathConds
         <*> mergeArrays
         <*> mergeRefArrays
         <*> mergeInstanceFields
         <*> mergeStaticFields
         <*> mergeFrms
    else do
      return Nothing
  where
    t <-> f = do
      -- NB: The term equality check below is about correctness as well as
      -- performance.  If we don't do this, we can obtain terms that are
      -- "superfluously" symbolic, which can make code that should trivially
      -- terminate (e.g., because its termination conditions are concrete) no
      -- longer do so.
      if t == f
        then return t
        else liftSymbolic $ applyIte (psAssumptions from) t f
    --
    same f              = f from == f to
    pathConds           = return (psAssumptions from) ||| return (psAssumptions to)
    mergeRefArrays      = return $ refArrays from `M.union` refArrays to
    mergeArrays         = arrays `mergeBy` \(l1,a1) (l2,a2) -> do
                            CE.assert (l1 == l2) $ (,) l1 <$> a1 <-> a2
    mergeInstanceFields = instanceFields `mergeBy` mergeV
    mergeStaticFields   = staticFields `mergeBy` mergeV
    mergeFrms           = do
      -- Merge the top frame of both states.
      case (frames from, frames to) of
        ([], [])                -> return []
        (frm1 : rest, frm2 : _) -> do
          locals <- ((\(f:_) -> frmLocals f) . frames) `mergeBy` mergeV
          opds   <- mapM (uncurry mergeV) (frmOpds frm1 `zip` frmOpds frm2)
          return $ frm2{ frmLocals = locals, frmOpds = opds} : rest
        _ -> error "frame mismatch (uncaught merge precondition violation?)"
    --
    mergeRVs =
      CE.assert (not (isBkpt fromFR || isBkpt toFR)) $
      maybe (finalResult to) id
      <$> case (fromFR, toFR) of
            (ReturnVal v1, ReturnVal v2) ->
              Just . ReturnVal <$> (v1 `mergeV` v2)
            _ -> return Nothing
    --
    mergeV (IValue v1) (IValue v2)             = IValue <$> v1 <-> v2
    mergeV (LValue v1) (LValue v2)             = LValue <$> v1 <-> v2

    mergeV x@(FValue v1) (FValue v2) = do
      let b = isNaN v1 && isNaN v2 || v1 == v2
      if b
       then return x
       else abort $ "Attempt to merge two concrete non-NaN unequal floats: "
                  ++ show v1 ++ " and " ++ show v2

    mergeV x@(DValue v1) (DValue v2) = do
      let b = isNaN v1 && isNaN v2 || v1 == v2
      if b
       then return x
       else abort $ "Attempt to merge two concrete non-NaN unequal doubles: "
                  ++ show v1 ++ " and " ++ show v2

    mergeV x@(RValue NullRef) (RValue NullRef) = return x

    mergeV x@(RValue (Ref r1 ty1)) y@(RValue (Ref r2 ty2)) = do
      when (r1 /= r2) $
        abort $ "References differ when merging: " ++ show x ++ " and " ++ show y
      CE.assert (ty1 == ty2) $ return ()
      return x

    mergeV x y =
      let f (RValue (Ref _ ty)) = "a reference of type " ++ show ty
          f v                   = show v
      in
        abort $ "Unsupported or mismatched type when merging values:\n"
                ++ "  tried to merge " ++ f x ++ " and " ++ f y
    --
    -- Merge the map elements (given via the selector 'sel') common to both
    -- states via the given action 'mrg', and then union in elements unique to
    -- each state as well (via the left-biased map union operator)
    mergeBy :: forall k a. (Ord k)
            => (PathState (MonadTerm sym) -> Map k a)
            -> (a -> a -> Simulator sym a)
            -> Simulator sym (Map k a)
    mergeBy sel mrg = leftUnion <$> merged
      where
        -- Use the left-biasing of M.union to prefer the key/value mappings in
        -- 'x', and then take everything else in the selected 'from' and 'to'
        -- maps.
        leftUnion x = x `M.union` sel from `M.union` sel to
        merged      =
          DF.foldrM
            (\(k, v1, v2) acc -> flip (M.insert k) acc <$> mrg v1 v2)
            M.empty
            (M.intersectionWithKey (\k v1 v2 -> (k, v1, v2)) (sel from) (sel to))
    --
    intersectBy selector combineFn =
      M.intersectionWith combineFn (selector from) (selector to)
    --
    checkPreconds =
      return
      $ (\xs -> case xs of [] -> Nothing ; _ -> Just xs)
      $ map fst . filter (not . snd)
      $
      [ -- 1) The array maps in the intersection of the path states have the
        -- same length (pairwise).
        ( ArraySizesMatch
        , DF.and $ M.intersectionWith (\x y  -> fst x == fst y)
                     (arrays from) (arrays to)
        )

        -- 2) The refArray maps in the intersection of the path states are the
        -- same
      , ( RefArraysEqual
          -- NB: Since we don't support symbolic references, we don't permit the
          -- contents of arrays of references to differ over branches.  E.g., if
          -- 'b' is symbolic, then
          --
          --   Foo[] fs = new Foo[1];
          --   if (b)
          --     fs[0] = new Foo();
          --   else
          --     fs[0] = new Foo();
          --
          -- is not permitted.
          --
          -- To enforce this, one of the merge preconditions is that the values
          -- in the intersection of thw two refArrays maps are strictly equal.
          --
          -- However, it's entirely possible for array references themselves to
          -- be introduced on some branches and not others, e.g.
          --
          -- Foo[] f;
          --
          -- if (b) {
          --   f = null;
          -- }
          -- else
          --   f = new Foo[12];
          --
          -- When the local variables are merged for this stack frame, f will be
          -- bound to (pathCond ? null : new Foo[12]), and the refArrays map for
          -- the else branch will contain an entry for the array of Foo refs.
          -- At the merge point, we want the merged refArrays map to contain
          -- that entry, so we union the two maps.

        , DF.and $ refArrays `intersectBy` (==)
        )

      , -- 3) Both PathStates have the same sequence of stack frames (same
        -- method, PC, operand stack, etc.)
        ( StackFramesAlign
        , and [ same (length . frames)
              , let opdTypesMatch DValue{} DValue{} = True
                    opdTypesMatch FValue{} FValue{} = True
                    opdTypesMatch IValue{} IValue{} = True
                    opdTypesMatch LValue{} LValue{} = True
                    opdTypesMatch RValue{} RValue{} = True
                    opdTypesMatch AValue{} AValue{} = True
                    opdTypesMatch _ _ = False
                in
                  and $ (`map` (frames from `zip` frames to)) $
                    \(Call c1 m1 pc1 _ opds1, Call c2 m2 pc2 _ opds2) ->
                      and [ c1 == c2
                          , methodKey m1 == methodKey m2
                          , pc1 == pc2
                            -- ^ PCs differ -> both frames at a return instruction
                          , length opds1 == length opds2
                          , and $ map (uncurry opdTypesMatch) $
                              opds1 `zip` opds2
                          ]
              ]
        )
      ]

--------------------------------------------------------------------------------
-- Misc utilities

simExcHndlr' ::
  forall sym.
  ( MonadIO sym
  , Show (MonadTerm sym)
  , Typeable (MonadTerm sym)
  )
  => Bool -> String -> CE.SomeException -> sym [Bool]
simExcHndlr' suppressOutput failMsg exc = do
  let h :: (Show t, Typeable t, t ~ MonadTerm sym) => Maybe (SimulatorExc t)
      h = CE.fromException exc
  case h of
    Just (SimExtErr msg _ _) -> do
      unless suppressOutput $ liftIO $ hPutStr stderr msg
      return [False]
    _ -> error $ failMsg ++ ": " ++ show exc

simExcHndlr ::
  forall sym.
  ( MonadIO sym
  , Show (MonadTerm sym)
  , Typeable (MonadTerm sym)
  )
  => String -> CE.SomeException -> sym [Bool]
simExcHndlr = simExcHndlr' True

_interactiveBreak :: MonadIO m => String -> m ()
_interactiveBreak msg = liftIO $ do
  putStrLn $ "*** " ++ msg ++ " (continue y/n): "
  inp <- getLine
  when (inp == "n") $ error "interactive break: forced early termination"

isExc :: FinalResult term -> Bool
isExc Exc{}   = True
isExc Aborted = True
isExc _       = False

isBkpt :: FinalResult term -> Bool
isBkpt Breakpoint{} = True
isBkpt _            = False

_isNormal :: FinalResult term -> Bool
_isNormal Terminated  = True
_isNormal ReturnVal{} = True
_isNormal _           = False

-- | Partition the top merge frame's finished paths into those paths which are
-- in either an exception or breakpoint state, and those which are not
splitFinishedPaths :: MonadIO sym => Simulator sym ([PathDescriptor], [PathDescriptor])
splitFinishedPaths = partitionMFOn ((\r -> isExc r || isBkpt r) . finalResult)

-- | Partition the top merge frame's finished paths into those paths which match
-- a predicate and those which do not.
partitionMFOn :: MonadIO sym
              => (PathState (MonadTerm sym) -> Bool)
              -> Simulator sym ([PathDescriptor], [PathDescriptor])
partitionMFOn f = do
  psts <- gets pathStates
  let passes p = maybe (error "expected PSS entry") f (M.lookup p psts)
  partition passes . finishedPaths <$> getMergeFrame

-- | Modify the active merge frame
modifyMF :: MonadIO sym =>
            (MergeFrame sym -> MergeFrame sym) -> Simulator sym ()
modifyMF f = modify $ \s -> s{ mergeFrames = headf (mergeFrames s) f }

-- | Create a new merge frame
mergeFrame :: [(PathDescriptor, ResumeAction sym)] -> MergeFrame sym
mergeFrame paths = MergeFrame [] paths CallExit

newString :: AigOps sym => String -> Simulator sym Ref
newString s = do
  -- It'd be preferable to use createInstance here, but the amount of
  -- infrastructure needed to create strings via the Java runtime is significant
  -- (thread local variables, character encodings, builtin unsafe operations,
  -- etc.), so we cheat and just forcibly set the (private) instance fields.
  -- We'll want want to REVISIT this in the future.

  chars <- liftSymbolic (mapM (termInt . fromIntegral . fromEnum) s)
  arr   <- newIntArray charArrayTy chars

  initializeClass "java/lang/String"
  ref <- genRef stringTy
  setInstanceFieldValue
    ref
    (FieldId "java/lang/String" "value" charArrayTy)
    (RValue arr)
  arrayOffset <- liftSymbolic $ termInt 0
  setInstanceFieldValue
    ref
    (FieldId "java/lang/String" "offset" IntType)
    (IValue arrayOffset)
  alen <- liftSymbolic $ termInt $ fromIntegral (length s)
  setInstanceFieldValue
    ref
    (FieldId "java/lang/String" "count" IntType)
    (IValue alen)
  return ref

-- | Extract the string from the given reference to a java.lang.String
-- contains concrete characters.
drefString :: AigOps sym => Ref -> Simulator sym String
drefString strRef = do
  Just ty       <- typeOf strRef
  CE.assert (ty == stringTy) $ return ()

  iflds <- instanceFields <$> getPathState
  let lkup   = (`M.lookup` iflds) . (,) strRef
      fldIds = [ FieldId "java/lang/String" "value"  (ArrayType CharType)
               , FieldId "java/lang/String" "count"  IntType
               , FieldId "java/lang/String" "offset" IntType
               ]
  case mapMaybe lkup fldIds of
    [RValue arrRef, IValue cnt, IValue off] -> do
      chars <- getPSS >>= \pd -> getIntArray pd arrRef
      when (any (not . isJust . termConst) $ cnt:off:chars) $
        abort "Unable to dereference symbolic strings"
      let cvt = fromIntegral . intFromConst . fromJust . termConst
      return $ take (cvt cnt) $ drop (cvt off) $ map (toEnum . cvt) chars
    _ -> error "Invalid field name/type for java.lang.String instance"

-- | Create an instance of the @Class@ class, if it doesn't already exist.
newClass :: AigOps sym => String -> Simulator sym Ref
newClass cname = do
  -- As with String above, creating a proper Class object is tricky,
  -- and intimately tied with things like reflection. We probably want
  -- to REVISIT this, as well.
  initializeClass "java/lang/Class"
  ref <- genRef (ClassType "java/lang/Class")
  str <- newString cname
  setInstanceFieldValue ref (FieldId "java/lang/Class" "name" stringTy) (RValue str)
  return ref

-- | Obtain the string value of the name field for the given instance of class
-- @Class@
getClassName :: AigOps sym => Ref -> Simulator sym String
getClassName classRef@(Ref _ (ClassType "java/lang/Class")) = do
  pd <- getPSS
  drefString =<< unRValue <$> getInstanceFieldValue pd classRef
                                (FieldId "java/lang/Class" "name" stringTy)
getClassName _ = error "getClassName: wrong argument type"

-- | Returns (the Value) 'true' if the given class name represents an array class
-- (using java.lang.Class naming conventions)
classNameIsArray :: ConstantInjection term => String -> Value term
classNameIsArray s = IValue $ mkCInt 32 $ if classNameIsArray' s then 1 else 0

classNameIsArray' :: String -> Bool
classNameIsArray' ('[':_) = True
classNameIsArray' _       = False

-- | Returns (the Value) 'true' if the given class name represents a primtive
-- type (using java.lang.Class naming conventions)
classNameIsPrimitive :: ConstantInjection term => String -> Value term
classNameIsPrimitive s = IValue $ mkCInt 32 $ if classNameIsPrimitive' s then 1 else 0

classNameIsPrimitive' :: String -> Bool
classNameIsPrimitive' (ch:[]) = ch `elem` ['B','S','I','J','F','D','Z','C']
classNameIsPrimitive' _       = False

lookupStringRef :: MonadIO sym => Ref -> Simulator sym (Maybe String)
lookupStringRef r =
  lookup r. map (\(a,b) -> (b,a)) . M.assocs <$> gets strings

-- Array Operations {{{1
-- | Create a new symbolic array with the given type, length and initial value.
-- TODO: Identify appropriate error to throw if type is not an array of IValues or
-- LValues or cnt is negative.
newSymbolicArray :: MonadIO sym =>
                    Type -> Int32 -> MonadTerm sym -> Simulator sym Ref
newSymbolicArray tp@(ArrayType eltType) cnt arr =
  CE.assert ((isIValue eltType || eltType == LongType) && cnt >= 0) $ do
    r <- genRef tp
    modifyPathState $ \ps ->
      ps { arrays = M.insert r (cnt, arr) (arrays ps) }
    return r
newSymbolicArray _ _ _ = error "internal: newSymbolicArray called with invalid type"

-- | Returns length and symbolic value associated with array reference,
-- and nothing if this is not an array reference.
getSymbolicArray :: MonadIO sym =>
                    Ref -> Simulator sym (Maybe (Int32, MonadTerm sym))
getSymbolicArray r = do
  ps <- getPathState
  return $ M.lookup r (arrays ps)

-- | Sets integer or long array to use using given update function.
-- TODO: Revisit what error should be thrown if ref is not an array reference.
setSymbolicArray :: MonadIO sym => Ref -> MonadTerm sym -> Simulator sym ()
setSymbolicArray r arr = updateSymbolicArray r (\_ -> return arr)

-- | Updates integer or long array using given update function.
-- TODO: Revisit what error should be thrown if ref is not an array refence.
updateSymbolicArray :: MonadIO sym =>
                       Ref
                    -> (MonadTerm sym -> sym (MonadTerm sym))
                    -> Simulator sym ()
updateSymbolicArray r modFn = do
  ps <- getPathState
  let (len,arr) = maybe (error "internal: reference is not a symbolic array") id
                      $ M.lookup r (arrays ps)
  newArr <- liftSymbolic $ modFn arr
  putPathState ps{ arrays = M.insert r (len, newArr) (arrays ps) }

-- | @newIntArray arTy terms@ produces a reference to a new array of type
-- @arTy@ and populated with given @terms@.
newIntArray :: WordMonad sym => Type -> [MonadTerm sym] -> Simulator sym Ref
newIntArray tp@(ArrayType eltType) values
  | isIValue eltType = do
    arr <- liftSymbolic $
      symbolicArrayFromList int32Type values
    newSymbolicArray tp (safeCast (length values)) arr
newIntArray _ _ = error "internal: newIntArray called with invalid type"

-- | @newLongArray arTy terms@ produces a reference to a new array of type
-- @arTy@ and populated with given @terms@.
newLongArray :: WordMonad sym =>
                Type -> [MonadTerm sym] -> Simulator sym Ref
newLongArray tp@(ArrayType LongType) values = do
  arr <- liftSymbolic $
    symbolicArrayFromList int64Type values
  newSymbolicArray tp (safeCast (length values)) arr
newLongArray _ _ = error "internal: newLongArray called with invalid type"

-- | Returns array length at given index in path.
getArrayLength :: AigOps sym => PathDescriptor -> Ref -> Simulator sym Int32
getArrayLength pd ref = do
  ps <- getPathStateByName pd
  ArrayType tp <- getType ref
  return $
    if isRValue tp then
      let Just arr = M.lookup ref (refArrays ps)
      in 1 + snd (bounds arr)
    else
      let Just (len,_) = M.lookup ref (arrays ps)
      in len

-- | Returns value in array at given index.
getArrayValue :: AigOps sym
              => PathDescriptor
              -> Ref
              -> MonadTerm sym
              -> Simulator sym (Value' sym)
getArrayValue pd r idx = do
  ps <- getPathStateByName pd
  ArrayType tp <- getType r
  if isRValue tp
    then do
      let Just arr = M.lookup r (refArrays ps)
      case getSVal idx of
        Just i -> return $ RValue (arr ! fromInteger i)
        _ -> abort "Not supported: symbolic indexing into arrays of references."
    else if tp == LongType
      then do
        let Just (_,rslt) = M.lookup r (arrays ps)
        fmap LValue $ liftSymbolic $ applyGetArrayValue rslt idx
      else do
        CE.assert (isIValue tp) $ return ()
        let Just (_,rslt) = M.lookup r (arrays ps)
        fmap IValue $ liftSymbolic $ applyGetArrayValue rslt idx

-- | Returns values in byte array at given reference.
getByteArray :: AigOps sym =>
                PathDescriptor -> Ref -> Simulator sym [MonadTerm sym]
getByteArray pd ref = do
  a <- getIntArray pd ref
  liftSymbolic $ mapM (applyTrunc 8) a

-- | Returns values in an array at a given reference, passing them through the
-- given projection
getArray :: AigOps sym
         => (Value' sym -> a)
         -> PathDescriptor
         -> Ref
         -> Simulator sym [a]
getArray f pd ref = do
  len <- getArrayLength pd ref
  forM [0..len-1] $ liftM f . getArrayValue pd ref <=< liftSymbolic . termInt

-- | Returns values in integer array at given reference.
getIntArray ::AigOps sym
            => PathDescriptor -> Ref -> Simulator sym [MonadTerm sym]
getIntArray = getArray unIValue

-- | Returns values in the long array at given reference.
getLongArray :: AigOps sym
             => PathDescriptor -> Ref -> Simulator sym [MonadTerm sym]
getLongArray = getArray unLValue

-- | Returns elements in reference array at given reference.
getRefArray :: AigOps sym
            => PathDescriptor -> Ref -> Simulator sym [Ref]
getRefArray = getArray unRValue

-- Misc operations {{{1

-- | Returns type of reference and throws null pointer exception if reference is null.
getType :: AigOps sym => Ref -> Simulator sym Type
getType NullRef    = throwNullPtrExc
getType (Ref _ tp) = return tp

getInstanceFieldValue :: MonadIO sym =>
                         PathDescriptor
                      -> Ref
                      -> FieldId
                      -> Simulator sym (Value (MonadTerm sym))
getInstanceFieldValue pd ref fldId = do
  ps <- getPathStateByName pd
  case M.lookup (ref, fldId) (instanceFields ps) of
    Just v   -> return v
    Nothing  -> error $ "getInstanceFieldValue: instance field " ++
                        show fldId ++ " does not exist"

getStaticFieldValue :: WordMonad sym =>
                       PathDescriptor
                    -> FieldId
                    -> Simulator sym (Value (MonadTerm sym))
getStaticFieldValue pd fldId = do
  ps <- getPathStateByName pd
  cl <- lookupClass $ fieldIdClass fldId
  case M.lookup fldId (staticFields ps) of
    Just v  -> return v
    Nothing -> CE.assert (validStaticField cl) $
                 return $ defaultValue $ fieldIdType fldId
  where
    validStaticField cl =
      maybe False (\f -> fieldIsStatic f && fieldType f == fieldIdType fldId)
      $ find (\fld -> fieldName fld == fieldIdName fldId)
      $ classFields cl

-- Returns default value for objects with given type.
defaultValue :: ConstantInjection term => Type -> Value term
defaultValue (ArrayType _tp) = RValue NullRef
defaultValue    BooleanType  = IValue $ mkCInt 32 0
defaultValue       ByteType  = IValue $ mkCInt 32 0
defaultValue       CharType  = IValue $ mkCInt 32 0
defaultValue (ClassType _st) = RValue NullRef
defaultValue     DoubleType  = DValue 0.0
defaultValue      FloatType  = FValue 0.0
defaultValue        IntType  = IValue $ mkCInt 32 0
defaultValue       LongType  = LValue $ mkCInt 64 0
defaultValue      ShortType  = IValue $ mkCInt 32 0

dumpPathStates :: (MonadIO sym, PrettyTerm (MonadTerm sym)) =>
                  Simulator sym ()
dumpPathStates = do
  psts <- gets pathStates
  dbugM "-- begin pathstates --"
  forM_ (M.elems psts) $ \ps -> dbugM (show ps)
  dbugM "-- end pathstates --"
  dumpMergeFrames

dumpPathStateCount :: MonadIO sym => Simulator sym ()
dumpPathStateCount = do
  (sz, mx) <- gets ((M.size CA.&&& foldr max minBound . M.keys) . pathStates)
  dbugM $ "pathstates size is: " ++ show sz ++ ", max pss is: " ++ show mx

dumpMergeFrames :: MonadIO sym => Simulator sym ()
dumpMergeFrames = do
  mfs <- gets mergeFrames
  dbugM $ "== MergeFrames from top to bottom = \n  "
          ++ intercalate "\n  " (map ppMergeFrame mfs)
  dbugM "---"

_dumpFrameInfo :: MonadIO sym => Simulator sym ()
_dumpFrameInfo = do
  psts <- gets pathStates
  dbugM $ "frame info: # pathstates = " ++ show (M.size psts) ++ ", max depth = "
        ++ show (foldr max 0 $ map (length . frames) (M.elems psts))

-- | Fatal error: kill all paths but the offending path (for exception
-- propagation) and provide some additional information to the user.

abort :: (CatchMIO sym, Show (MonadTerm sym), Typeable (MonadTerm sym)) =>
         String -> Simulator sym a
abort msg = do
  whenVerbosity (>=5) $ dbugM $ "abort invoked w/ msg:\n--\n" ++ msg ++ "\n--\n"
  -- TODO: For debugging, save info about other paths states here, and report

  -- It's possible that getPathState can fail here, but report the abort message
  -- anyway instead of reporting the getPathState failure.
  mps <- Right `fmap` getPathState `catchMIO` \(e :: CE.SomeException) ->
           return $ Left $ show e
  case mps of
    Left _gpsExcStr -> error msg
    Right ps@PathState{ pathStSel = pss, frames = frms } -> do
      modify $ \s -> s{ pathStates = M.singleton pss ps{ finalResult = Aborted} }
      let showFrms = length frms > 0
      Simulation.throwExternal
        (msg ++ "\n" ++ ppExcMsg showFrms pss (if showFrms then ppStk frms else ""))
        (M.singleton pss (Aborted, frms))

throwExternal ::
  ( MonadIO sym
  , Show (MonadTerm sym)
  , Typeable (MonadTerm sym)
  )
  => String
  -> Map PathDescriptor (FinalResult (MonadTerm sym), [Frame (MonadTerm sym)])
  -> Simulator sym a
throwExternal msg rslts = do
  v <- gets verbosity
  CE.throw $ SimExtErr msg v rslts

ppExcMsg :: Bool -> PathDescriptor -> String -> String
ppExcMsg showPathId (PSS n) s =
  (if showPathId then "(On path " ++ show n ++ "):\n" else "") ++  s

--------------------------------------------------------------------------------
-- Pretty printers

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
ppPSS (PSS n) = "Path #" ++ show n

ppMergeFrame :: MergeFrame sym -> String
ppMergeFrame MergeFrame{ finishedPaths = fp, nextPaths = np } =
  "MergeFrame{ finishedPaths = " ++ show fp
  ++ ", nextPaths = " ++ show np
  ++ "}"

--------------------------------------------------------------------------------
-- Instances

instance Eq Ref where
  NullRef == NullRef = True
  (Ref x _) == (Ref y _) = x == y
  _ == _ = False

instance Ord Ref where
  NullRef `compare` NullRef = EQ
  NullRef `compare` _ = LT
  _ `compare` NullRef = GT
  (Ref x _) `compare` (Ref y _) = x `compare` y

instance (PrettyTerm (MonadTerm sym)) => Show (State sym) where
  show s = intercalate "\n" (["{ begin pathstates: "] ++ (map show $ M.elems $ pathStates s) ++ ["end pathstates }"])
             ++ "\nNext Ref: " ++ show (nextRef s)

instance PrettyTerm term => Show (PathState term) where
  show state =
    let dispMapBy :: (forall k a. Map k a -> ((k, a) -> String) -> String)
        x `dispMapBy` showItem = (multi . map showItem . M.toList)  x
        x `dispBy` showItem    = (multi . map showItem . DF.toList) x
        multi lns              = pad ++ intercalate pad lns ++ "\n"
        pad                    = "\n" ++ replicate 4 ' '
    in
      ppPSS (pathStSel state) ++ ":\n"
      ++
      "  frames         : "
      ++ (if null $ frames state
            then "(none)\n"
            else frames state `dispBy` ppFrame
         )
      ++
      "  instance fields: "
      ++ (if M.null $ instanceFields state
            then "(none)\n"
            else instanceFields state `dispMapBy` \((r, fldId), v) ->
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
      ++ (if M.null (staticFields state)
            then "(none)\n"
            else
              let f (fldId, v) = fieldIdClass fldId
                                 ++ "."
                                 ++ fieldIdName fldId
                                 ++ " => "
                                 ++ ppValue v
              in
                staticFields state `dispMapBy` f
         )
      ++
      "  arrays         : "
      ++ (if M.null $ arrays state
            then "(none)\n"
            else arrays state `dispMapBy` \(k,(l,v)) -> show k ++ " : " ++ show l ++ " = " ++ prettyTerm v
         )
      ++
      "  refArrays      : "
      ++ (if M.null $ refArrays state
            then "(none)\n"
            else refArrays state `dispMapBy` \(r,rs) ->
                   ppRef r
                   ++ " => [ "
                   ++ intercalate ", " (map ppRefId $ elems rs)
                   ++ " ]"
         )
      ++
  --    "  assumptions    : " ++ ppSymTerm (psAssumptions state) ++ "\n"
  --    ++
      "  finalResult    : " ++ ppFinalResult (finalResult state)
      ++
      "  starting PC    : " ++ show (startingPC state)
      ++
      "  instr count    : " ++ show (insnCount state)


