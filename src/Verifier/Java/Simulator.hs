{- |
Module           : $Header$
Description      : JVM Symbolic Simulatior implementation
Stability        : stable
Point-of-contact : jstanley, acfoltzer

Implementation of the JVM Symbolic Simulator. This module exposes an
interface that allows clients to execute JVM semantics, yielding
symbolic results. The primary interface is the combination of
'runStaticMethod' and 'runSimulator', which allows the specification
of an entrypoint to a computation.

From an implementor's perspective, the bulk of this module is an
instance of 'Execution.JavaSemantics.JavaSemantics' for the simulator
types defined in "Verifier.Java.Common".

-}

{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- for the JavaSemantics instance of Simulator
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Verifier.Java.Simulator
  ( -- * Simulator
    Simulator (SM)
  , SEH(..)
    -- ** Simulator control
  , runDefSimulator
  , runSimulator
  , runStaticMethod
  , getProgramReturnValue
  , getProgramFinalMem
  , getProgramErrorPaths
  , setSEH
  , defaultSEH
  , withSBE
    -- ** Low-level control
  , run
  , execInstanceMethod
  , execStaticMethod
    -- * Method overriding
  , overrideInstanceMethod
  , overrideStaticMethod
    -- * Heap operations
    -- ** Array operations
  , getArrayLength
  , getArrayValue
  , getByteArray
  , getIntArray
  , getLongArray
  , getRefArray
  , getSymbolicArray
  , newIntArray
  , newLongArray
  , newSymbolicArray
  , setSymbolicArray
  , updateSymbolicArray
    -- ** Fields and Strings
  , lookupStringRef
  , getStaticFieldValue
  , getInstanceFieldValue
    -- * Dynamic binding
  , dynBind
  , dynBindSuper
    -- * Pretty-printing and debugging 
  , dbugM
  , abort  
  , prettyTermSBE
    -- * Re-exported modules
  , module Execution.JavaSemantics
  , module Verifier.Java.Backend
  , module Verifier.Java.Common
  , module Verifier.Java.Codebase
  ) where

import Prelude hiding (EQ, LT, GT)

import Control.Applicative hiding (empty)
import Control.Lens hiding (act)
import Control.Monad
import Control.Monad.Error
import Control.Monad.State (evalStateT, get, put)

import Data.Array
import Data.Char
import Data.Int
import Data.List (find, foldl')
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V

import System.IO (hFlush, stdout)

import Text.PrettyPrint

import Language.JVM.CFG
import Data.JVM.Symbolic.AST
import Data.JVM.Symbolic.Translation

import Execution.JavaSemantics
import qualified Execution.Stepper as Stepper
import Language.JVM.Common
import Language.JVM.Parser
import Verifier.Java.Codebase hiding (lookupClass)
import Verifier.Java.Backend
import Verifier.Java.Common hiding (getCurrentClassName, getCurrentMethod)
import qualified Verifier.Java.Common as Common
import Verifier.Java.Utils

instance LogMonad (Simulator sbe m) where
  getVerbosity = use verbosity
  setVerbosity = assign verbosity

-- | Run a static method with the given arguments. Class, method name,
-- and method type are given in JVM internal format, e.g.,
-- @\"com\/example\/App\" \"main\" \"([Ljava\/lang\/String;)V\"@. Returns all
-- successful paths, along with return values if present.
runStaticMethod :: MonadSim sbe m
                => String      -- ^ Class name
                -> String      -- ^ Method name
                -> String      -- ^ Method type
                -> [Value (SBETerm sbe)] -- ^ Arguments
                -> Simulator sbe m [(Path sbe, Maybe (Value (SBETerm sbe)))]
runStaticMethod cName mName mType args = do
  let methodKey = makeMethodKey mName mType
  pushStaticMethodCall cName methodKey args Nothing
  run
  cs <- use ctrlStk
  when (not $ isFinished cs) 
    $ fail "runStaticMethod: unexpected active control stack after executing method"
  case currentPath cs of
    Just p -> do
      rv <- getProgramReturnValue
      return [(p, rv)]
    Nothing -> return []--    Nothing -> fail "runStaticMethod: all paths failed"

-- | Low-level function. If you do not know you need to call this, you
-- likely do not need to! This is the main loop of the simulator which
-- inspects the control stack and applies one continuation at a time
-- until the program is complete.
run :: forall sbe m . MonadSim sbe m => Simulator sbe m ()
run = do
  cs <- use ctrlStk
  if isResumed cs then do
    dbugM' 5 "run: subcomputation finished, resuming outer computation"
    let ResumedCS p cs' = cs
        Just suspPath = currentPath cs'
        p' = p & pathStack   .~ suspPath^.pathStack
               & pathStackHt .~ suspPath^.pathStackHt
       --        & pathRetVal  .~ Nothing
    case modifyPath (const p') cs' of
      Just cs'' -> ctrlStk .= cs''
      Nothing   -> fail "all paths failed in resumed computation"
  else if isFinished cs then
    case currentPath cs of
      Just (p :: Path sbe) -> do
        -- Normal program termination on at least one path.
        -- Report termination info at appropriate verbosity levels; also,
        -- inform user about error paths when present and optionally dump
        -- them.
        whenVerbosity (>=5) $ dumpCtrlStk
        whenVerbosity (>=6) $ dumpMemory "run"
        whenVerbosity (>=2) $ do
          dbugM "run terminating normally: found valid exit frame"
          case p^.pathRetVal of
            Nothing -> dbugM "Program had no return value."
            Just rv -> dbugValue "Program returned value" rv
          numErrs <- uses errorPaths length
          showEPs <- use printErrPaths
          when (numErrs > 0 && showEPs) $ do
            dbugM $ showErrCnt numErrs
            dumpErrorPaths
      Nothing -> do
        -- All paths ended in errors.
        showEPs <- use printErrPaths
        if showEPs then
          tellUser "All paths yielded errors!" >> dumpErrorPaths
        else
          tellUser "All paths yielded errors! To see details, use --errpaths."
  else do
    (p :: Path sbe) <- case currentPath cs of
                         Just p -> return p
                         Nothing -> fail "impossible"
    sbe <- use backend
    dbugM' 5 . render $ "run:" <+> ppPath sbe p
    flip catchError handleError $ do
      let Just bid = p^.pathBlockId
      when (pathAssertedFalse sbe p) $
        errorPath $ FailRsn $ "This path is infeasible"

      cf <- case currentCallFrame p of
              Just cf -> return cf
              Nothing -> fail "empty call stack"
      let method = cf^.cfMethod
          cName  = cf^.cfClass
      cb <- use codebase
      whenVerbosity (>= 6) $ do
         liftIO $ dumpSymASTs cb cName
      symBlocks <- do 
         mblocks <- liftIO $ lookupSymbolicMethod cb cName (methodKey method)
         case mblocks of
           Just blocks -> return blocks
           _ -> fail . render $ "unable to symbolically translate" 
                <+> text cName <+> ppMethod method
      case lookupSymBlock bid symBlocks of
        Just symBlock -> do
          dbugM' 6 . render $ "run:" <+> ppSymBlock symBlock
          runInsns $ sbInsns symBlock
        Nothing -> fail . render 
          $ "invalid basic block" <+> ppBlockId bid 
          <+> "for method" <+> ppMethod method
    run
  where
    lookupSymBlock :: BlockId -> [SymBlock] -> Maybe SymBlock
    lookupSymBlock bid = find $ \SymBlock { sbId } -> bid == sbId
    handleError (ErrorPathExc _rsn s) = do
      -- errorPath ensures that the simulator state provided in the
      -- exception data is correct for the next invocation of run,
      -- so overwrite the current state here.
      put s
    handleError e = throwError e
    showErrCnt x
      | x == 1    = "Encountered errors on exactly one path. Details below."
      | otherwise = "Encountered errors on " ++ show x ++ " paths.  Details below."
    dumpErrorPaths = do
        dbugM $ replicate 80 '-'
        eps <- use errorPaths
        forM_ eps $ \ep -> do
          let p = ep^.epPath
          sbe <- use backend
          dbugM $ "Error reason        : " ++ show (ppFailRsn (ep^.epRsn))
          dbugM $ "Error path state    :\n" ++ show (nest 2 $ ppPath sbe p)
          -- whenVerbosity (>= 3) $ do
          --   dbugM "Error path memory: "
          -- withSBE (\s -> memDump s (pathMem p) Nothing)
          when (length eps > 1) $ dbugM "--"
        dbugM $ replicate 80 '-'

-- | Return true if the path has asserted false to be true, and therefore we
-- can call errorPath on it.
pathAssertedFalse :: Backend sbe -> Path' (SBETerm sbe) -> Bool
pathAssertedFalse sbe p = asBool sbe (p^.pathAssertions) == Just False

lookupOverride :: M.Map (String, MethodKey) a 
               -> String 
               -> MethodKey
               -> Simulator sbe m (Maybe a)
lookupOverride overrides cName key = do
  cb <- use codebase
  cl <- lookupClass cName
  tgts <- map className <$> liftIO (supers cb cl)
  let mtgts = [ M.lookup (cName', key) overrides | cName' <- tgts ]
  case catMaybes mtgts of
    [] -> return Nothing
    (ovr:_) -> return (Just ovr)

lookupStaticOverride :: String 
                     -> MethodKey
                     -> Simulator sbe m (Maybe (StaticOverride sbe m))
lookupStaticOverride cName key = do
  override <- use staticOverrides
  lookupOverride override cName key

-- | Invoke a static method in the current simulator state. If there
-- is no basic block to return to, this is either a top-level
-- invocation, or a special case like an overridden method.
pushStaticMethodCall :: MonadSim sbe m
                     => String
                     -> MethodKey
                     -> [Value (SBETerm sbe)]
                     -> Maybe BlockId
                     -> Simulator sbe m ()
pushStaticMethodCall cName key args mRetBlock = do
  initializeClass cName
  let locals = setupLocals args
  override <- lookupStaticOverride cName key
  case override of
    Just f -> do f args
                 case mRetBlock of
                   Just retBlock -> setCurrentBlock retBlock
                   Nothing -> return ()
    Nothing -> do
      cb <- use codebase
      impls <- liftIO $ findStaticMethodsByRef cb cName key
      dbugM' 6 . render $ "found static impls:" <+> sep (map text impls)
      cl <- case impls of
              cl:_ -> lookupClass cl
              _ -> fail . render 
                     $ "pushStaticMethodCall: could not find method"
                     <+> text cName <> "." <> ppMethodKey key
      let cName' = className cl
      method <- case cl `lookupMethod` key of
                  Just m -> return m
                  Nothing -> fail . render 
                    $ "pushStaticMethodCall: could not find method" 
                    <+> text cName' <> "." <> ppMethodKey key
      if methodIsNative method then
        error $ "Unsupported native static method " ++ show key ++ " in " ++ cName'
      else do
        when (cName' == "com/galois/symbolic/Symbolic") $
          expectOverride "Symbolic"
        when (cName' == "com/galois/symbolic/Symbolic$Debug") $
          expectOverride "Symbolic$Debug"
        case mRetBlock of
          Just retBB -> modifyCSM_ $ \cs ->
            case pushCallFrame cName' method retBB locals cs of
              Just cs' -> return cs'
              Nothing  -> fail "cannot invoke method: all paths failed"
          Nothing -> void $ execMethod cName' key locals
  where
    expectOverride cn =
      error $ "expected static override for " ++ cn ++ "."
            ++ methodKeyName key ++ unparseMethodDescriptor key

lookupInstanceOverride :: String 
                     -> MethodKey
                     -> Simulator sbe m (Maybe (InstanceOverride sbe m))
lookupInstanceOverride cName key = do
  overrides <- use instanceOverrides
  lookupOverride overrides cName key
  
-- | Invoke an instance method in the current simulator state. If
-- there is no basic block to return to, this is either a top-level
-- invocation, or a special case like an overridden method. The given
-- class is assumed to be the concrete type, with dynamic dispatch
-- handled by the caller.
pushInstanceMethodCall :: MonadSim sbe m
                       => String
                       -> MethodKey
                       -> JSRef (Simulator sbe m)
                       -> [Value (SBETerm sbe)]
                       -> Maybe BlockId
                       -> Simulator sbe m ()
pushInstanceMethodCall cName key objectRef args mRetBlock = do
  let locals = setupLocals (RValue objectRef : args)
  override <- lookupInstanceOverride cName key
  case override of
    Just f -> do f objectRef args
                 case mRetBlock of
                   Just retBlock -> setCurrentBlock retBlock
                   Nothing -> return ()
    Nothing -> do
      cl <- lookupClass cName
      method <- case cl `lookupMethod` key of
                  Just m -> return m
                  Nothing -> fail . render 
                    $ "pushInstanceMethodCall: could not find method" 
                    <+> text cName <> "." <> ppMethodKey key
      if methodIsNative method then
        error $ "Unsupported native instance method " ++ show key ++ " in " ++ cName
      else do
--        dbugM $ "pushCallFrame: (instance) method call to " ++ cName ++ "." ++ methodName method
        case mRetBlock of
          Just retBB -> do 
            modifyCSM_ $ \cs ->
              case pushCallFrame cName method retBB locals cs of
                Just cs' -> return cs'
                Nothing  -> fail "cannot invoke method: all paths failed"
          Nothing -> void $ execMethod cName key locals

setupLocals :: [Value term]
            -> M.Map LocalVariableIndex (Value term)
setupLocals vals = M.fromList (snd $ foldl setupLocal (0, []) vals)
  where
    setupLocal (n, acc) v@LValue{} = (n + 2, (n, v) : acc)
    setupLocal (n, acc) v@DValue{} = (n + 2, (n, v) : acc)
    setupLocal (n, acc) v          = (n + 1, (n, v) : acc)

-- | Run the simulator with default event handlers and simulation flags
runDefSimulator :: 
     MonadSim sbe IO
  => Codebase              -- ^ JVM Codebase
  -> Backend sbe           -- ^ A symbolic backend
  -> Simulator sbe IO a    -- ^ Simulator action to perform
  -> IO a
runDefSimulator cb sbe m = runSimulator cb sbe defaultSEH Nothing m

-- | Run the simulator with the given configuration
runSimulator :: 
     MonadSim sbe IO
  => Codebase              -- ^ JVM Codebase
  -> Backend sbe           -- ^ A symbolic backend
  -> SEH sbe IO            -- ^ Simulation event handlers (use defaultSEH if no
                           -- event handling is needed)
  -> Maybe SimulationFlags -- ^ Simulation flags
  -> Simulator sbe IO a    -- ^ Simulator action to perform
  -> IO a
runSimulator cb sbe seh mflags m = do
  newSt <- initialState cb sbe (fromMaybe defaultSimFlags mflags) seh
  ea <- flip evalStateT newSt $ runErrorT $ runSM $ do
    stdOverrides
    m
  case ea of
    Left ErrorPathExc{}   -> error "internal: uncaught error path exception"
    Left (UnknownExc mfr) -> error $ "internal: uncaught unknown exception: "
                                     ++ maybe "(no details)" (show . ppFailRsn) mfr
    Right x               -> return x

-- | Get the return value of a completed simulator execution. If there
-- is none (e.g., we executed @main@), or if the simulator is still
-- running, return 'Nothing'.
getProgramReturnValue :: MonadSim sbe m
                      => Simulator sbe m (Maybe (Value (SBETerm sbe)))
getProgramReturnValue = do
  cs <- use ctrlStk
  if isFinished cs 
    then case currentPath cs of
           Just p -> return (p^.pathRetVal)
           _      -> return Nothing            
    else return Nothing

-- | Get the return value of a completed simulator execution. If the simulator is still
-- running, or all paths ended in errors, return 'Nothing'.
getProgramFinalMem :: MonadSim sbe m
                   => Simulator sbe m (Maybe (Memory (SBETerm sbe)))
getProgramFinalMem = do
  cs <- use ctrlStk
  if isFinished cs
    then do mp <- getPathMaybe
            case mp of
              Just p -> return . Just $ p^.pathMemory
              _      -> return Nothing
    else return Nothing

-- | Get a list of all the 'ErrorPath's that have arisen so far during
-- a simulator execution.
getProgramErrorPaths :: MonadSim sbe m
                     => Simulator sbe m [ErrorPath sbe]
getProgramErrorPaths = use errorPaths

--------------------------------------------------------------------------------
-- Memory operations

-- | Returns a new reference value with the given type, but does not
-- initialize and fields or array elements.
-- Note: Assumes class for reference has already been initialized.
genRef :: Type -> Simulator sbe m Ref
genRef tp = do
  r <- nextRef <+= 1
  return $ Ref r tp

-- Array Operations {{{1
-- | Create a new symbolic array with the given type, length and initial value.
-- TODO: Identify appropriate error to throw if type is not an array of IValues or
-- LValues or cnt is negative.
newSymbolicArray :: MonadSim sbe m
                 => Type -> Int32 -> SBETerm sbe -> Simulator sbe m Ref
newSymbolicArray tp@(ArrayType eltType) cnt arr = do
  assert ((isIValue eltType || eltType == LongType) && cnt >= 0)
  r <- genRef tp
  m <- getMem "newSymbolicArray"
  setMem (m & memScalarArrays %~ M.insert r (cnt, arr))
  return r
newSymbolicArray _ _ _ = fail "internal: newSymbolicArray called with invalid type"

-- | Returns length and symbolic value associated with array reference,
-- and nothing if this is not an array reference.
getSymbolicArray :: MonadSim sbe m
                 => Ref -> Simulator sbe m (Maybe (SBETerm sbe, SBETerm sbe))
getSymbolicArray r = do
  m <- getMem "getSymbolicArray"
  case M.lookup r (m^.memScalarArrays) of
    Just (len, a) -> do
      sbe <- use backend
      len' <- liftIO $ termInt sbe len
      return . Just $ (len', a)
    Nothing -> return Nothing

-- | Sets integer or long array to use using given update function.
-- TODO: Revisit what error should be thrown if ref is not an array reference.
setSymbolicArray :: MonadSim sbe m => Ref -> SBETerm sbe -> Simulator sbe m ()
setSymbolicArray r arr = updateSymbolicArray r (\_ _ _ -> return arr)

-- | Updates integer or long array using given update function.
updateSymbolicArray :: MonadSim sbe m
                    => Ref
                    -> (Backend sbe -> SBETerm sbe -> SBETerm sbe -> IO (SBETerm sbe))
                    -> Simulator sbe m ()
updateSymbolicArray r modFn = do
  m <- getMem "updateSymbolicArray"
  (len,arr) <- 
    maybe (fail "updateSymbolicArray: reference is not a symbolic array") return
    $ M.lookup r (m^.memScalarArrays)
  sbe <- use backend
  len' <- liftIO $ termInt sbe len
  newArr <- liftIO $ modFn sbe len' arr
  setMem (m & memScalarArrays %~ M.insert r (len, newArr))

-- | @newIntArray arTy terms@ produces a reference to a new array of type
-- @arTy@ and populated with given @terms@.
newIntArray :: MonadSim sbe m => Type -> [SBETerm sbe] -> Simulator sbe m Ref
newIntArray tp@(ArrayType eltType) values
  | isIValue eltType = do
    sbe <- use backend
    arr <- liftIO $ do
      let v = V.fromList values
      l <- termInt sbe (fromIntegral (V.length v))
      Just a0 <- termIntArray sbe l
      let fn a i = do
            ti <- termInt sbe (fromIntegral i)
            termSetIntArray sbe l a ti (v V.! i)
      V.foldM fn a0 (V.enumFromN 0 (V.length v))
    newSymbolicArray tp (safeCast (length values)) arr
newIntArray _ _ = error "internal: newIntArray called with invalid type"

-- | @newLongArray arTy terms@ produces a reference to a new long array
-- populated with given @terms@.
newLongArray :: MonadSim sbe m => [SBETerm sbe] -> Simulator sbe m Ref
newLongArray values = do
  sbe <- use backend
  arr <- liftIO $ do
    let v = V.fromList values
    l <- termInt sbe (fromIntegral (V.length v))
    Just a0 <- termLongArray sbe l
    let fn a i = do
          ti <- termInt sbe (fromIntegral i)
          termSetLongArray sbe l a ti (v V.! i)
    V.foldM fn a0 (V.enumFromN 0 (V.length v))
  newSymbolicArray (ArrayType LongType) (safeCast (length values)) arr

-- | Returns array length at given index in the current path.
getArrayLength :: MonadSim sbe m 
               => Ref -> Simulator sbe m (SBETerm sbe)
getArrayLength ref = do
  ArrayType tp <- getType ref
  m <- getMem "getArrayLength"
  sbe <- use backend
  if isRValue tp then do
    let Just arr = M.lookup ref (m^.memRefArrays)
    liftIO $ termInt sbe (1 + snd (bounds arr))
  else do
    let Just (len,_) = M.lookup ref (m^.memScalarArrays)
    liftIO $ termInt sbe len

-- | Returns value in array at given index.
getArrayValue :: (AigOps sbe, Functor m, MonadIO m)
              => Ref
              -> SBETerm sbe
              -> Simulator sbe m (Value (SBETerm sbe))
getArrayValue r idx = do
  ArrayType tp <- getType r
  sbe <- use backend
  m <- getMem "getArrayValue"
  if isRValue tp then do
    let Just arr = M.lookup r (m^.memRefArrays)
    case asInt sbe idx of
      Just i -> return $ RValue (arr ! fromIntegral i)
      _ -> fail "Not supported: symbolic indexing into arrays of references."
  else if tp == LongType then
    liftIO $ do
      let Just (l,rslt) = M.lookup r (m^.memScalarArrays)
      l' <- termInt sbe l
      LValue <$> termGetLongArray sbe l' rslt idx
  else do
    assert (isIValue tp)
    liftIO $ do
      let Just (l,rslt) = M.lookup r (m^.memScalarArrays)
      l' <- termInt sbe l
      IValue <$> termGetIntArray sbe l' rslt idx

-- | Returns values in an array at a given reference, passing them through the
-- given projection
getArray :: MonadSim sbe m
         => (Value (SBETerm sbe) -> a)
         -> Ref
         -> Simulator sbe m [a]
getArray f ref = do
  sbe <- use backend
  len <- getArrayLength ref
  case asInt sbe len of
    Nothing -> fail "Not supported: getArray called on array with symbolic length"
    Just l ->
      forM [0..l-1] $ \i -> do
        liftM f . getArrayValue ref =<< liftIO (termInt sbe i)

-- | Returns values in byte array at given reference.
getByteArray :: MonadSim sbe m
             => Ref -> Simulator sbe m [SBETerm sbe]
getByteArray ref = do
  a <- getIntArray ref
  withSBE $ \sbe -> mapM (termByteFromInt sbe) a

-- | Returns values in integer array at given reference.
getIntArray :: MonadSim sbe m
            => Ref -> Simulator sbe m [SBETerm sbe]
getIntArray = getArray unIValue

-- | Returns values in the long array at given reference.
getLongArray :: MonadSim sbe m
             => Ref -> Simulator sbe m [SBETerm sbe]
getLongArray = getArray unLValue

-- | Returns elements in reference array at given reference.
getRefArray :: MonadSim sbe m
            => Ref -> Simulator sbe m [Ref]
getRefArray = getArray unRValue

--setStaticFieldValue :: MonadSim sbe m
--                    => FieldId -> Value (SBETerm sbe) -> Simulator sbe m ()

-- | Returns type of reference and throws null pointer exception if reference is null.
getType :: (AigOps sbe, Functor m, MonadIO m) => Ref -> Simulator sbe m Type
getType NullRef    = throwNullPtrExc
getType (Ref _ tp) = return tp

getInstanceFieldValue :: MonadSim sbe m
                      => Ref
                      -> FieldId
                      -> Simulator sbe m (Value (SBETerm sbe))
getInstanceFieldValue ref fldId = do
  m <- getMem "getInstanceFieldValue"
  case M.lookup (ref, fldId) (m^.memInstanceFields) of
    Just v   -> return v
    Nothing  -> fail $ "getInstanceFieldValue: instance field " ++
                       show fldId ++ " does not exist"

getStaticFieldValue :: MonadSim sbe m
                    => FieldId
                    -> Simulator sbe m (Value (SBETerm sbe))
getStaticFieldValue fldId = do
  let cName = fieldIdClass fldId
  -- this might be the first time we reference this class, so initialize
  initializeClass cName
  cl <- lookupClass cName
  m <- getMem "getStaticFieldValue"
  case M.lookup fldId (m^.memStaticFields) of
    Just v  -> return v
    Nothing -> do
      assert (validStaticField cl) 
      withSBE $ \sbe -> defaultValue sbe (fieldIdType fldId)
  where
    validStaticField cl =
      maybe False (\f -> fieldIsStatic f && fieldType f == fieldIdType fldId)
      $ find (\fld -> fieldName fld == fieldIdName fldId)
      $ classFields cl


tryModifyCS :: Monad m => String -> (CS sbe -> Maybe (CS sbe)) -> Simulator sbe m ()
tryModifyCS ctx f = ctrlStk %= fn
  where fn = fromMaybe (error err) . f
          where err = "internal: tryModifyCS " ++ show ctx

-- @setMem@ sets the memory model in the current path, which must exist.
setMem :: (Functor m, Monad m) => Memory (SBETerm sbe) -> Simulator sbe m ()
setMem mem = tryModifyCS "setMem" $ modifyPath $ set pathMemory mem

withSBE :: MonadIO m
        => (Backend sbe -> IO a) -> Simulator sbe m a
withSBE f = uses backend f >>= liftIO

withSBE' :: Monad m 
         => (Backend sbe -> a) -> Simulator sbe m a
withSBE' f = uses backend f

setSEH :: Monad m => SEH sbe m -> Simulator sbe m ()
setSEH = assign evHandlers

ppCurrentMethod :: Path' term -> Doc
ppCurrentMethod p = case currentCallFrame p of
  Nothing -> "<no current method>"
  Just cf -> text (cf^.cfClass) <> "." <> ppMethod (cf^.cfMethod)

-- | Converts integral into bounded num class.
-- TODO: Revisit error handling when integer is out of range.
safeCast :: (Integral s, Bounded t, Integral t, Num t) => s -> t
safeCast = impl minBound maxBound . toInteger
  where impl :: Integral t => t -> t -> Integer -> t
        impl minb maxb s
          | toInteger minb <= s && s <= toInteger maxb = fromInteger s
          | otherwise = error "internal: safeCast argument out of range"

-- initializeClass :: MonadSim sbe m => String -> Simulator sbe m ()

-- | Run a static method in the current simulator context, temporarily
-- suspending the current call stack. Useful for implementing method
-- overrides. For top-level invocations, use 'runStaticMethod' instead.
execStaticMethod :: MonadSim sbe m
                 => String
                 -> MethodKey
                 -> [Value (SBETerm sbe)]
                 -> Simulator sbe m (Maybe (Value (SBETerm sbe)))
execStaticMethod cName key args = do
  mp <- execMethod cName key (setupLocals args)
  case mp of
    Nothing -> fail "execStaticMethod: all paths failed"
    Just (_, mrv) -> return mrv

-- | Run an instance method in the current simulator context, using
-- @ref@ as @this@, temporarily suspending the current call
-- stack. Useful for implementing method overrides.
execInstanceMethod :: MonadSim sbe m
                   => String
                   -> MethodKey
                   -> Ref
                   -> [Value (SBETerm sbe)]
                   -> Simulator sbe m (Maybe (Value (SBETerm sbe)))
execInstanceMethod cName key ref args = do
  mp <- execMethod cName key (setupLocals (RValue ref : args))
  case mp of
    Nothing -> fail "execInstanceMethod: all paths failed"
    Just (_, mrv) -> return mrv

-- | Low-level method call for when we need to make a method call not
-- prescribed by the symbolic instructions. This arises mainly in
-- class initialization, as well as synthetic code as for
-- printStreams. Does not check for overrides.
execMethod :: MonadSim sbe m
           => String
           -> MethodKey
           -> M.Map LocalVariableIndex (Value (SBETerm sbe))
           -> Simulator sbe m (Maybe (Path sbe, (Maybe (Value (SBETerm sbe)))))
execMethod cName key locals = do
  cl <- lookupClass cName
  method <- case cl `lookupMethod` key of
              Just m -> return m
              Nothing -> fail . render
                $ "runMethod: could not find method" <+> text cName
                <> "." <> ppMethodKey key

  -- suspend the current computation so we can do a nested invocation of @run@
  cs <- use ctrlStk
  cs' <- maybe (fail "execMethod: no active path") return
         $ suspendCS "execMethod" cs
  -- partial pattern OK: already handled no-path case
  let Just cs'' = pushCallFrame cName method entryBlock locals cs'
  -- set suspended control stack
  ctrlStk .= cs''
  run
  finishedCS <- use ctrlStk
  case currentPath finishedCS of
    Just _ -> modifyPathM "execMethod" $ \p -> do
                let p' = p -- p & pathRetVal .~ Nothing
                return (Just (p', p^.pathRetVal), p')
    Nothing -> return Nothing

hasCustomClassInitialization :: String -> Bool
hasCustomClassInitialization "java/lang/System" = True
hasCustomClassInitialization _                  = False

runCustomClassInitialization :: MonadSim sbe m => String -> Simulator sbe m ()
runCustomClassInitialization cName = do
  let ctx = "runCustomClassInitialization"
  case cName of
    "java/lang/System" -> do
      dbugM' 5 $ "initializing java/lang/System"
      -- we're only here if initializeClass found Nothing for
      -- initialization status, so we don't need to check again
      setMem . setInitializationStatus cName Started =<< getMem ctx
      initializeClass pstreamName
      outStream <- RValue `liftM` genRef (ClassType pstreamName)
      errStream <- RValue `liftM` genRef (ClassType pstreamName)
      setStaticFieldValue (FieldId cName "out" pstreamType) outStream
      setStaticFieldValue (FieldId cName "err" pstreamType) errStream
      setMem . setInitializationStatus cName Initialized =<< getMem ctx
    _ -> return ()
  where pstreamName = "java/io/PrintStream"
        pstreamType = ClassType pstreamName

--newObject :: MonadSim sbe m => String -> Simulator sbe m Ref

errorPath ::
  ( MonadIO m
  , Functor m
  )
  => FailRsn -> Simulator sbe m a
errorPath rsn = do
  sbe <- use backend
  -- Update control stack.
  p <- getPath $ "errorPath" <+> parens (ppFailRsn rsn)
  -- Log error path  
  whenVerbosity (>=3) $ do
    dbugM $ "Error path encountered: " ++ show (ppFailRsn rsn)
    dbugM $ show $ ppPath sbe p
  cs' <- markCurrentPathAsError =<< use ctrlStk
  s <- get
  let s' = s & ctrlStk    .~ cs'
             & errorPaths %~ (EP rsn p :) 
  -- Merge negation of assumptions in current path into conditions on merge frame.
  -- NB: Since we've set up the control stack for the next invocation of
  -- run, and explicitly captured the error path, we need to be sure to
  -- ship that modified state back to the catch site so it execution can
  -- continue correctly.
  throwError $ ErrorPathExc rsn s'

-- | Returns a default value for objects with given type, suitable for
-- initializing fields.
defaultValue :: Backend sbe -> Type -> IO (Value (SBETerm sbe))
defaultValue _   (ArrayType _tp) = return $ RValue NullRef
defaultValue sbe BooleanType     = IValue <$> termInt sbe 0 
defaultValue sbe ByteType        = IValue <$> termInt sbe 0 
defaultValue sbe CharType        = IValue <$> termInt sbe 0
defaultValue _   (ClassType _st) = return $ RValue NullRef
defaultValue _   DoubleType      = return $ DValue 0.0
defaultValue _   FloatType       = return $ FValue 0.0
defaultValue sbe IntType         = IValue <$> termInt sbe 0
defaultValue sbe LongType        = LValue <$> termLong sbe 0
defaultValue sbe ShortType       = IValue <$> termInt sbe 0

-- REVISIT: it may make sense for this to be dynamic
skipInit :: String -> Bool
skipInit cname = cname `elem` [ "java/lang/System"
                              , "java/io/Reader"
                              , "java/io/InputStreamReader"
                              ]

runInsns :: MonadSim sbe m
         => [(Maybe PC, SymInsn)] -> Simulator sbe m ()
runInsns insns = mapM_ dbugStep insns

dbugStep :: MonadSim sbe m
         => (Maybe PC, SymInsn) -> Simulator sbe m ()
dbugStep (pc, insn) = do
  mp <- getPathMaybe
  case mp of
    Nothing -> dbugM' 4 . render $ "Executing: (no current path)" 
               <> colon <+> (ppSymInsn insn)
    Just p  -> do
      let loc = case currentCallFrame p of
                  Nothing -> "<unknown method>" <> parens bid
                  Just cf -> text (cf^.cfClass) 
                             <> "." <> ppMethod (cf^.cfMethod)
                             <> parens bid
          bid = case p^.pathBlockId of
                  Nothing -> "<no current block>"
                  Just b -> ppBlockId b
      dbugM' 4 . render $ 
        "Executing" <+> parens ("#" <> integer (p^.pathName))
        <+> loc <> colon <+> (ppSymInsn insn)
--  repl
  cb2 onPreStep pc insn
  step insn
  cb2 onPostStep pc insn
  whenVerbosity (>=6) $ do
    p <- getPath "dbugStep"
    sbe <- use backend
    dbugM . render $ ppPath sbe p

-- | Execute a single LLVM-Sym AST instruction
step :: MonadSim sbe m
     => SymInsn -> Simulator sbe m ()

-- invokeinterface

step (PushInvokeFrame InvInterface (ClassType iName) key retBlock) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef   <- rPop
  throwIfRefNull objectRef
  dynBind iName key objectRef $ \iName' -> do
    pushInstanceMethodCall iName' 
                           key 
                           objectRef 
                           (reverse reverseArgs)
                           (Just retBlock)

step (PushInvokeFrame InvInterface ty _ _) = do
  fail . render $ "step: invokeinterface on type" <+> ppType ty

-- invokespecial

-- arises from super.m() calls
step (PushInvokeFrame InvSpecial (ClassType cName) key retBlock) = do
  currentClassName <- Common.getCurrentClassName
  reverseArgs      <- replicateM (length (methodKeyParameterTypes key)) popValue
  currentClass     <- lookupClass currentClassName
  objectRef        <- rPop
  cb               <- use codebase
  b                <- liftIO $ isStrictSuper cb cName currentClass
  let args          = reverse reverseArgs
      call cName'   = pushInstanceMethodCall cName' key objectRef args (Just retBlock)
  if classHasSuperAttribute currentClass && b && methodKeyName key /= "<init>"
    then do
      dynBindSuper cName key objectRef call
    else
      do throwIfRefNull objectRef
         call cName

step (PushInvokeFrame InvSpecial (ArrayType _methodType) key retBlock) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef <- rPop
  throwIfRefNull objectRef
  pushInstanceMethodCall "java/lang/Object" 
                         key
                         objectRef
                         (reverse reverseArgs)
                         (Just retBlock)

step (PushInvokeFrame InvSpecial ty _ _) = do
  fail . render $ "step: invokespecial on type" <+> ppType ty

-- invokevirtual

step (PushInvokeFrame InvVirtual (ArrayType _methodType) key retBlock) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef <- rPop
  throwIfRefNull objectRef
  pushInstanceMethodCall "java/lang/Object"
                         key
                         objectRef 
                         (reverse reverseArgs)
                         (Just retBlock)

step (PushInvokeFrame InvVirtual (ClassType cName) key retBlock) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef   <- rPop
  throwIfRefNull objectRef
  dynBind cName key objectRef $ \cName' -> do
    pushInstanceMethodCall cName' 
                           key
                           objectRef
                           (reverse reverseArgs) 
                           (Just retBlock)

step (PushInvokeFrame InvVirtual ty _ _) = do
  fail . render $ "step: invokevirtual on type" <+> ppType ty

-- invokestatic

step (PushInvokeFrame InvStatic (ClassType cName) key retBlock) = do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  pushStaticMethodCall cName key (reverse reverseArgs) (Just retBlock)

step (PushInvokeFrame InvStatic ty _ _) = do
  fail . render $ "step: invokestatic on type" <+> ppType ty

step ReturnVoid = modifyCSM_ $ returnCurrentPath Nothing

step ReturnVal = do
  rv <- popValue
  modifyCSM_ $ returnCurrentPath (Just rv)

step (PushPendingExecution bid cond ml elseInsns) = do
  sbe <- use backend
  c <- evalCond cond
  b <- do blast <- alwaysBitBlastBranchTerms <$> use (simulationFlags)
          if blast 
            then liftIO (blastTerm sbe c) 
            else return $ asBool sbe c
  dbugM' 6 $ "### PushPendingExecution (condAsBool=" ++ show (asBool sbe c) ++ ")"
  case b of
   -- Don't bother with elseStmts as condition is true. 
   Just True  -> setCurrentBlock bid
   -- Don't bother with pending path as condition is false.
   Just False -> runInsns elseInsns
   Nothing -> do
     nm <- use nextPSS
     nextPSS += 1
     modifyCSM_ $ \cs -> case addCtrlBranch c bid nm ml cs of
                           Just cs' -> return cs'
                           Nothing -> fail "addCtrlBranch"
     runInsns elseInsns

step (SetCurrentBlock bid) = setCurrentBlock bid

step (NormalInsn insn) = Stepper.step insn

step (BadInsn insn) = 
  fail . render $ "step: unexpected instruction" <+> text (ppInst insn)

setCurrentBlock :: MonadSim sbe m => BlockId -> Simulator sbe m ()
setCurrentBlock b = do modifyCSM_ $ \cs -> jumpCurrentPath b cs

evalCond :: MonadSim sbe m => SymCond -> Simulator sbe m (SBETerm sbe)
evalCond (HasConstValue i) = do
  v <- iPop
  si <- iConst (fromIntegral i)
  iEq v si
evalCond (NotConstValues is) = do
  sbe <- use backend
  true <- liftIO $ termBool sbe True
  v <- iPop
  sis <- mapM (iConst . fromIntegral) is
  sbs <- mapM (\si -> bNot =<< iEq v si) sis
  liftIO $ foldM (termAnd sbe) true sbs
evalCond Null = isNull =<< rPop
evalCond NonNull = bNot =<< isNull =<< rPop
evalCond TrueSymCond = do
  sbe <- use backend
  liftIO $ termBool sbe True
evalCond (Compare cmpTy) = do
  y <- iPop
  x <- iPop
  case cmpTy of
    EQ -> iEq x y
    NE -> bNot =<< iEq x y
    LT -> iLt x y
    GE -> bNot =<< iLt x y
    GT -> bNot =<< iLeq x y
    LE -> iLeq x y
evalCond (CompareRef cmpTy) = do
  y <- rPop
  x <- rPop
  case cmpTy of
    EQ -> rEq x y
    NE -> bNot =<< rEq x y
    _  -> fail "invalid reference comparison; bug in symbolic translation?"

--------------------------------------------------------------------------------
-- Callbacks and event handlers

cb1 :: (Functor m, Monad m)
  => (SEH sbe m -> a -> Simulator sbe m ()) -> a -> Simulator sbe m ()
cb1 f x = join (f <$> use evHandlers <*> pure x)

cb2 :: (Functor m, Monad m)
  => (SEH sbe m -> a -> b -> Simulator sbe m ()) -> a -> b -> Simulator sbe m ()
cb2 f x y = join (f <$> use evHandlers <*> pure x <*> pure y)

defaultSEH :: Monad m => SEH sbe m
defaultSEH = SEH
               (return ())
               (\_ _ -> return ())
               (\_ _ -> return ())

--------------------------------------------------------------------------------
-- Conversions

compareFloat :: (Floating a, Ord a, MonadSim sbe m) 
             => a -> a -> Simulator sbe m (SBETerm sbe)
compareFloat x y = withSBE $ \sbe -> termInt sbe (fromIntegral v)
  where v = fromEnum (compare x y) - 1

floatRem :: (RealFrac a) => a -> a -> a
floatRem x y = fromIntegral z
  where z :: Integer
        z = truncate x `rem` truncate y

--refFromString :: MonadSim sbe m => String -> Simulator sbe m Ref

newString :: MonadSim sbe m => String -> Simulator sbe m Ref
newString s = do
  -- It'd be preferable to use createInstance here, but the amount of
  -- infrastructure needed to create strings via the Java runtime is significant
  -- (thread local variables, character encodings, builtin unsafe operations,
  -- etc.), so we cheat and just forcibly set the (private) instance fields.
  -- We'll want want to REVISIT this in the future.
  sbe <- use backend
  -- TODO: Check this with unicode characters.
  chars <- liftIO $ mapM (termInt sbe . fromIntegral . fromEnum) s
  arr   <- newIntArray charArrayTy chars
  initializeClass "java/lang/String"
  ref <- genRef stringTy
  setInstanceFieldValue
    ref
    (FieldId "java/lang/String" "value" charArrayTy)
    (RValue arr)
  arrayOffset <- liftIO $ termInt sbe 0
  setInstanceFieldValue
    ref
    (FieldId "java/lang/String" "offset" IntType)
    (IValue arrayOffset)
  alen <- liftIO $ termInt sbe $ fromIntegral (length s)
  setInstanceFieldValue
    ref
    (FieldId "java/lang/String" "count" IntType)
    (IValue alen)
  return ref

type instance JSDouble (Simulator sbe m) = Double
type instance JSFloat  (Simulator sbe m) = Float
type instance JSInt    (Simulator sbe m) = SBETerm sbe
type instance JSLong   (Simulator sbe m) = SBETerm sbe
type instance JSRef    (Simulator sbe m) = Ref
type instance JSBool   (Simulator sbe m) = SBETerm sbe

instance MonadSim sbe m => JavaSemantics (Simulator sbe m) where

  getCodebase = use codebase

  getCurrentClassName = Common.getCurrentClassName
  getCurrentMethod = Common.getCurrentMethod

  initializeClass name = do
    m <- getMem "initializeClass"
    case M.lookup name (m^.memInitialization) of
      Nothing | hasCustomClassInitialization name ->
                  runCustomClassInitialization name
      Nothing | otherwise -> do
        setMem $ setInitializationStatus name Started m
        let clinit = MethodKey "<clinit>" [] Nothing
            initializeField :: MonadSim sbe m => Field -> Simulator sbe m ()
            initializeField f =
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
                    when (fieldIsStatic f) $ do
                      val <- withSBE $ \sbe -> defaultValue sbe (fieldType f)
                      setStaticFieldValue fieldId val
                  Just tp -> error $ "Unsupported field type" ++ show tp
        dbugM' 5 . render $ "initializing class" <+> text name
        cl <- lookupClass name
        mapM_ initializeField $ classFields cl
        case cl `lookupMethod` clinit of
          Just _ -> do
            unless (skipInit name) $ do
              void $ execStaticMethod name clinit []
          Nothing -> return ()
        setMem . setInitializationStatus name Initialized 
          =<< getMem "initializeClass"
      Just Erroneous -> do
        createAndThrow "java/lang/NoClassDefFoundError"
      Just Started -> return ()
      Just Initialized -> return ()

  warning msg = do
    mp <- getPathMaybe
    -- Get location information
    let prefix = do
          p <- mp
          let fn = ppCurrentMethod p
          return $ case p^.pathBlockId of
            Nothing -> "at" <+> fn
            Just cb -> "at" <+> fn <> colon <> ppBlockId cb        
    liftIO . putStrLn . render $ 
      "Warning" <> maybe empty (space <>) prefix <> colon <+> msg


  throw ref@(Ref _ (ClassType cName)) = do
    ctrlStk %= modifyCurrentPath (\p -> 
      let exc = JavaException ref (p^.pathStack)
      in p & pathException .~ Just exc)
    errorPath $ FailRsn cName

  throw _ = fail "throw: null or non-reference type thrown"


  -- Negate a Boolean value
  bNot x = withSBE $ \sbe -> termNot sbe x 

  -- (x &&& y) returns logical and
  mx &&& my = do
    sbe <- use backend
    x <- mx
    case asBool sbe x of
      Just True -> my
      Just False -> return x
      _ -> do
        y <- my
        case asBool sbe y of
          Just True -> return x
          Just False -> return y
          _ -> liftIO $ termAnd sbe x y

  -- Conversions
  longFromInt x = withSBE $ \sbe -> termLongFromInt sbe x
  intFromLong x = withSBE $ \sbe -> termIntFromLong sbe x
  floatFromDouble  = return . fromRational . toRational
  intFromDouble x  = withSBE $ \sbe -> termInt sbe (truncate x)
  longFromDouble x = withSBE $ \sbe -> termLong sbe (truncate x)
  doubleFromFloat  = return . fromRational . toRational
  intFromFloat x   = withSBE $ \sbe -> termInt sbe (truncate x)
  longFromFloat x  = withSBE $ \sbe -> termLong sbe (truncate x)
  doubleFromInt  i = do
    sbe <- use backend
    case asInt sbe i of
      Just n -> return (fromIntegral n)
      Nothing -> error "cannot convert symbolic int to double"
  floatFromInt i = do
    sbe <- use backend
    case asInt sbe i of
      Just n -> return (fromIntegral n)
      Nothing -> error "cannot convert symbolic int to float"
  doubleFromLong l = do
    sbe <- use backend
    case asLong sbe l of
      Just n -> return (fromIntegral n)
      Nothing -> error "cannot convert symbolic long to double"
  floatFromLong  l = do
    sbe <- use backend
    case asLong sbe l of
      Just n -> return $ fromIntegral n
      Nothing -> error "cannot convert symbolic long to float"

  -- Double operations
  dAdd  x y = return $ x + y
  -- TODO: Fix Nan handling.
  dCmpg x y = compareFloat x y
  dCmpl x y = compareFloat x y
  dConst    = return
  dDiv  x y = return $ x / y
  dMul  x y = return $ x * y
  dNeg      = return . negate
  dRem  x y = return $ floatRem x y
  dSub  x y = return $ x - y

  -- Float operations
  fAdd  x y = return $ x + y
  fCmpg x y = compareFloat x y
  fCmpl x y = compareFloat x y
  fConst    = return
  fDiv  x y = return $ x / y
  fMul  x y = return $ x * y
  fNeg      = return . negate
  fRem  x y = return $ floatRem x y
  fSub  x y = return $ x - y

  -- Integer functions {{{1
  iAdd  x y = withSBE $ \sbe -> termIAdd sbe x y
  iAnd  x y = withSBE $ \sbe -> termIAnd sbe x y
  iConst v  = withSBE $ \sbe -> termInt sbe v
  iDiv  x y = withSBE $ \sbe -> termIDiv sbe x y
  iEq   x y = withSBE $ \sbe -> termEq sbe x y
  iLeq  x y = withSBE $ \sbe -> termILeq sbe x y
  iMul  x y = withSBE $ \sbe -> termIMul sbe x y
  iNeg  x   = withSBE $ \sbe -> termINeg sbe x
  iOr   x y = withSBE $ \sbe -> termIOr sbe x y
  iRem  x y = withSBE $ \sbe -> termIRem   sbe x y
  iShl  x y = withSBE $ \sbe -> termIShl  sbe x y
  iShr  x y = withSBE $ \sbe -> termIShr  sbe x y
  iSub  x y = withSBE $ \sbe -> termISub   sbe x y  
  iUshr x y = withSBE $ \sbe -> termIUshr sbe x y
  iXor  x y = withSBE $ \sbe -> termIXor  sbe x y 

  ------------------------------------------------------------------------------
  -- operations on longs
  lAdd x y = withSBE $ \sbe -> termLAdd sbe x y
  lAnd x y = withSBE $ \sbe -> termLAnd sbe x y
  lCmp x y = withSBE $ \sbe -> termLCompare sbe x y
  lConst v  = withSBE $ \sbe -> termLong sbe v
  lEq x y   = withSBE $ \sbe -> termEq sbe x y
  lDiv x y  = withSBE $ \sbe -> termLDiv sbe x y
  lMul x y  = withSBE $ \sbe -> termLMul sbe x y
  lNeg x    = withSBE $ \sbe -> termLNeg sbe x
  lOr   x y = withSBE $ \sbe -> termLOr sbe x y
  lRem  x y = withSBE $ \sbe -> termLRem sbe x y
  lShl  x y = withSBE $ \sbe -> termLShl sbe x y
  lShr  x y = withSBE $ \sbe -> termLShr sbe x y
  lSub  x y = withSBE $ \sbe -> termLSub sbe x y
  lUshr x y = withSBE $ \sbe -> termLUshr sbe x y
  lXor  x y = withSBE $ \sbe -> termLXor sbe x y

  setArrayValue r idx (IValue val) = updateSymbolicArray r $ \sbe l a ->
    termSetIntArray  sbe l a idx val
  setArrayValue r idx (LValue val) = updateSymbolicArray r $ \sbe l a ->
    termSetLongArray sbe l a idx val
  setArrayValue r idx (RValue v) = do
    sbe <- use backend
    m <- getMem "setArrayValue"
    case asInt sbe idx of
      Nothing -> fail "Cannot update reference arrays at symbolic indices"
      Just i -> do
        let updateFn arr = Just (arr // [(fromIntegral i,v)])
        setMem (m & memRefArrays %~ M.update updateFn r)
  setArrayValue _ _ val = do
    whenVerbosity (>= 2) $ do
      sbe <- use backend
      dbugM . render $ "bad array element" <+> ppValue sbe val
    fail "setArrayValue: unsupported array element type"

  setStaticFieldValue fieldId v = do
    let cName = fieldIdClass fieldId
    -- this might be the first time we reference this class, so initialize
    initializeClass cName

    sbe <- use backend
    dbugM' 6 . render $ "setting field" <+> text (ppFldId fieldId) <+> "to" <+> ppValue sbe v
    m <- getMem "setStaticFieldValue"
    setMem (m & memStaticFields %~ M.insert fieldId v)
          
  byteArrayVal arrayRef value = do
    cond <- arrayRef `hasType` (ArrayType BooleanType)
    t <- boolFromInt value
    e <- byteFromInt value
    sbe <- use backend
    term <- liftIO $ termIte sbe cond t e
    return $ term

  -- (arrayLength ref) return length of array at ref.
  arrayLength = getArrayLength

  -- (newMultiArray tp len) returns a reference to a multidimentional array with
  -- type tp and len = [len1, len2, ...] where len1 equals length of first
  -- dimention len2 equals length of second dimension and so on.  Note: Assumes
  -- all integer values are nonnegative.
  newMultiArray tp []
    | isRValue tp = return NullRef
  newMultiArray tp@(ArrayType eltType) [l]
    | isIValue eltType || (eltType == LongType) = do
      sbe <- use backend
      l' <- case asInt sbe l of
              Nothing -> abort "Cannot create array with symbolic size"
              Just i -> return i          
      ref <- genRef tp
      let arrayFn | eltType == LongType = termLongArray
                  | otherwise           = termIntArray
      ma <- liftIO $ arrayFn sbe l
      case ma of
        Nothing -> abort "Cannot create array with symbolic size"
        Just a -> do
          m <- getMem "newMultiArray"
          setMem $ m & memScalarArrays %~ M.insert ref (l', a)
      return ref
  newMultiArray (ArrayType DoubleType) _ =
    abort "Floating point arrays (e.g., double) are not supported"
  newMultiArray (ArrayType FloatType) _ =
    abort "Floating point arrays (e.g., float) are not supported"
  newMultiArray tp@(ArrayType eltTp) (tcnt : rest) = do
    sbe <- use backend
    case asInt sbe tcnt of
      Nothing -> abort "Cannot create array of references with symbolic size"
      Just cnt -> do
        ref <- genRef tp
        values <- replicateM (fromIntegral cnt) (newMultiArray eltTp rest)
        let arr = listArray (0, fromIntegral cnt-1) values
        m <- getMem "newMultiArray"
        setMem $ m & memRefArrays %~ M.insert ref arr
        return ref
  newMultiArray _ _ = abort "Cannot create array with symbolic size"

  newObject name = do
    initializeClass name
    ref <- genRef (ClassType name)
    -- Set fields to default value
    fields <- classFields <$> lookupClass name
    fvals <- withSBE $ \sbe -> mapM (defaultValue sbe . fieldType) fields
    m <- getMem "newObject"
    let m' = m & memInstanceFields .~
               foldl' (\fieldMap (f,val) ->
                          let fid = FieldId name (fieldName f) (fieldType f)
                          in val `seq` M.insert (ref,fid) val fieldMap)
               (m^.memInstanceFields)
               (fields `zip` fvals)
    setMem m'
    return ref

  isValidEltOfArray elt arr = do
    if elt == NullRef then
      withSBE $ \sbe -> termBool sbe True
    else do
      cb <- use codebase
      ArrayType arrayTy <- getType arr
      elTy              <- getType elt
      withSBE $ \sbe -> termBool sbe =<< isSubtype cb elTy arrayTy

  hasType ref tp = do
    cb <- use codebase
    rtp <- getType ref
    b <- liftIO $ isSubtype cb rtp tp
    withSBE $ \sbe -> termBool sbe b

  instanceOf ref tp = do
    -- this technically violates the semantics of Java, which just returns false
    -- for (null instanceof Foo), rather than throwing an exception
    throwIfRefNull ref
    cond <- hasType ref tp
    zero <- iConst 0
    one <- iConst 1
    -- rather than splitting paths, just push a conditional symbolic value here
    sbe <- use backend
    term <- liftIO $ termIte sbe cond one zero
    pushValue $ IValue term

  typeOf NullRef    = return Nothing
  typeOf (Ref _ ty) = return (Just ty)

  coerceRef NullRef _    = return NullRef
  coerceRef (Ref x _) ty = return (Ref x ty)

  -- Retuns predicate indicating super class of ref has given type.
  superHasType ref tp = do
    ClassType refClassname <- getType ref
    cb <- use codebase
    cl <- lookupClass refClassname
    withSBE $ \sbe -> do
      b <- case superClass cl of
             Just super -> isSubtype cb (ClassType super) (ClassType tp)
             Nothing    -> return False
      termBool sbe b

  -- (rEq x y) returns boolean formula that holds if x == y.
  rEq x y = withSBE $ \sbe -> termBool sbe (x == y)

  -- rNull returns node representing null pointer.
  rNull = return NullRef

  refFromString val = do
    sMap <- use strings
    case M.lookup val sMap of
      Just ref -> return ref
      Nothing  -> do
        ref <- newString val
        strings %= M.insert val ref
        return ref

  getClassObject cName = do
    m <- getMem "getClassObject"
    case M.lookup cName (m^.memClassObjects) of
      Just ref -> return ref
      Nothing -> do
        ref <- newClass cName
        setMem $ m & memClassObjects %~ M.insert cName ref
        return ref

  -- Heap related functions {{{1

  -- Pushes value of field onto stack.
  -- NOTE: Assumes ref is not null.
  pushInstanceFieldValue r fieldId = do
    m <- getMem "pushInstanceFieldValue"
    case M.lookup (r, fieldId) (m^.memInstanceFields) of
      Just value -> pushValue value
      -- Some code seems to depend on instance fields being
      -- initialized to their default value.
      Nothing    -> do
        when (isFloatType (fieldIdType fieldId)) $
          error $ "internal: unassigned floating-point instance field "
                    ++ show fieldId ++ " in " ++ show r
        val <- withSBE $ \sbe -> defaultValue sbe (fieldIdType fieldId)
        pushValue val

  -- Pushes value of field onto stack.
  -- NOTE: Assumes ref is not null.
  pushStaticFieldValue fieldId = do
    m <- getMem "pushStaticFieldValue"
    case M.lookup fieldId (m^.memStaticFields) of
      Just value -> pushValue value
      Nothing    -> do
        when (isFloatType (fieldIdType fieldId)) $
          fail $ "internal: unassigned static floating-point field " ++
                        show fieldId
        pushValue =<< withSBE (\sbe -> defaultValue sbe (fieldIdType fieldId))

  -- (pushArrayValue ref index) pushes the value of the array at index to the stack.
  -- NOTE: Assumes that ref is a valid array and index is a valid index in array.
  pushArrayValue r idx = pushValue =<< getArrayValue r idx

  setInstanceFieldValue r fieldId v = do
    m <- getMem "setInstanceFieldValue"
    setMem (m & memInstanceFields %~ M.insert (r, fieldId) v)

  -- Pop value off top of stack.
  popValue = modifyCallFrameM "popValue" $ \cf -> 
               case cf^.cfOpds of
                 [] -> fail "empty operand stack"
                 (x:xs) -> return (x, cf & cfOpds .~ xs)

  -- Push value onto top of stack.
  pushValue val = modifyCallFrameM_ "pushValue" $ \cf -> return $ cf & cfOpds %~ (val:)


  -- Local variable functions {{{1
  getLocal i = do
    cs <- use ctrlStk
    let mv = do cf <- currentCallFrame =<< currentPath cs
                M.lookup i (cf^.cfLocals)
    case mv of
      Just v -> return v
      Nothing -> error $ "internal: undefined local variable " ++ show i

  setLocal i v = modifyCallFrameM_ "setLocal" $ \cf -> 
                   return $ cf & cfLocals %~ M.insert i v

  printStream nl _ []       = liftIO $ (if nl then putStrLn else putStr) "" >> hFlush stdout
  printStream nl binary [x] = do
    sbe <- use backend
    let putStr' s = liftIO $ (if nl then putStrLn else putStr) s >> hFlush stdout
    case x of
      IValue (asInt sbe -> Just v)
        | binary    -> putStr' [chr $ fromEnum v]
        | otherwise -> putStr' $ show v
      v@IValue{} -> putStr' . render $ ppValue sbe v

      LValue (asLong sbe -> Just v) -> putStr' $ show v
      v@LValue{} -> putStr' . render $ ppValue sbe v
      FValue f -> putStr' (show f)
      DValue d -> putStr' (show d)
      RValue r -> do
        ms <- lookupStringRef r
        case ms of
          Just str  -> putStr' str
          Nothing   -> do
            let key = makeMethodKey "toString" "()Ljava/lang/String;"
            msref <- execInstanceMethod "java/lang/Object" key r []
            case msref of
              Just sref -> putStr' =<< drefString (unRValue sref)
              _ -> fail "toString terminated abnormally"
      _ -> abort $ "Unable to display values of type other than "
                 ++ "int, long, and reference to constant string"
  printStream _ _ _ = abort $ "Unable to print more than one argument"

  createInstance clNm margs = do
      ref <- newObject clNm
      void $ execInstanceMethod clNm ctorKey ref (maybe [] (map snd) margs)
      return ref
    where
      ctorKey = MethodKey "<init>" (maybe [] (map fst) margs) Nothing
{-

  -- Note: Assume linker errors can not be thrown
  dynBind' clName key objectRef cgen = do
    mty <- typeOf objectRef
    cb <- use codebase
    cls <- case mty of
             Nothing     -> return []
             Just (ClassType instTy) -> liftIO $ findVirtualMethodsByRef cb clName key instTy
             Just _ -> error "dynBind' type parameter not ClassType-constructed"
    let cases = (isNull objectRef |-> throwNullPtrExc) : map cgen cls
    -- In theory, this error should be unreachable.
    choice cases (error $ "Uncaught linker error: " ++ clName ++ ":" ++ show key)

  invokeStaticMethod cName key args = do
    cb <- use codebase
    sups <- liftIO (supers cb =<< lookupClass cb cName)
    case mapMaybe (\cl -> (,) cl `fmap` (cl `lookupMethod` key)) sups of
      ((cl,method):_) -> do
        when (not $ methodIsStatic method) $
          fatal $ "Attempted static invocation on a non-static method ("
                ++ className cl ++ "." ++ methodName method ++ ")"
        initializeClass (className cl)
        pushStaticMethodCall (className cl) method args
      [] -> error $ "Could not find static method " ++ show key ++ " in " ++ cName

-}

  -- if the asserted condition is definitely false, throw the given
  -- exception. Otherwise, continue, but add an assertion to the
  -- current path.
  assertTrueM cond exc = do
    sbe <- use backend
    term <- cond    
    when (asBool sbe term == Just False) $ createAndThrow exc
    modifyPathM_ "assertTrueM" (addPathAssertion sbe term)
    

abort :: MonadSim sbe m => String -> Simulator sbe m a
abort msg = do
  whenVerbosity (>=5) $ dbugM $ "abort invoked w/ msg:\n--\n" ++ msg ++ "\n--\n"
  fail msg
{-
-- | Extract the string from the given reference to a java.lang.String
-- contains concrete characters.
drefString :: MonadSim sbe m => Ref -> Simulator sbe m String
drefString strRef = do
  Just ty       <- typeOf strRef
  assert (ty == stringTy)

  Just m <- getMem
  let iflds  = m^.memInstanceFields
      lkup   = (`M.lookup` iflds) . (,) strRef
      fldIds = [ FieldId "java/lang/String" "value"  (ArrayType CharType)
               , FieldId "java/lang/String" "count"  IntType
               , FieldId "java/lang/String" "offset" IntType
               ]
  case mapMaybe lkup fldIds of
    [RValue arrRef, IValue cnt, IValue off] -> do
      chars <- getIntArray arrRef
      sbe <- use backend
      when (any (not . isJust . asInt sbe) $ cnt:off:chars) $
        abort "Unable to dereference symbolic strings"
      let cvt = fromIntegral . fromJust . asInt sbe
      return $ take (cvt cnt) $ drop (cvt off) $ map (toEnum . cvt) chars
    _ -> error "Invalid field name/type for java.lang.String instance"
-}

prettyTermSBE :: MonadSim sbe m => SBETerm sbe -> Simulator sbe m Doc
prettyTermSBE t = withSBE' $ \sbe -> prettyTermD sbe t

prettyValueSBE :: MonadSim sbe m => Value (SBETerm sbe) -> Simulator sbe m Doc
prettyValueSBE v = withSBE' $ \sbe -> ppValue sbe v


-- | Override behavior of simulator when it encounters a specific instance
-- method to perform a user-definable action.
-- Note: Fails if the method has already been overridden.
overrideInstanceMethod :: String
                       -> MethodKey
                       -> (Ref -> [Value (SBETerm sbe)] -> Simulator sbe m ())
                       -> Simulator sbe m ()
overrideInstanceMethod cName key action = do
  overrides <- use instanceOverrides
  let k = (cName, key)
  when (k `M.member` overrides) $ do
    fail $ "Method " ++ cName ++ "." ++ methodKeyName key  ++ " is already overridden."
  instanceOverrides %= M.insert k action

-- | Override behavior of simulator when it encounters a specific static
-- method to perform a user-definable action.
-- Note: Fails if the method has already been overridden.
overrideStaticMethod :: MonadSim sbe m
                     => String
                     -> MethodKey
                     -> ([Value (SBETerm sbe)] -> Simulator sbe m ())
                     -> Simulator sbe m ()
overrideStaticMethod cName mKey action = do
  overrides <- use staticOverrides
  let key = (cName, mKey)
  when (key `M.member` overrides) $
    abort $ "Method " ++ cName ++ "." ++ methodKeyName mKey ++ " is already overridden."
  staticOverrides %= M.insert key action

-- | Register all predefined overrides for builtin native implementations.
stdOverrides :: MonadSim sbe m => Simulator sbe m ()
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
      , \this _ -> pushValue =<< classNameIsArray =<< getClassName this
      )
    -- java.lang.Class.isPrimitive
    , ( "java/lang/Class"
      , makeMethodKey "isPrimitive" "()Z"
      , \this _ -> pushValue =<< classNameIsPrimitive =<< getClassName this
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
            pushStaticMethodCall nativeClass arrayCopyKey opds Nothing
        )
      -- java.lang.System.exit(int status)
    , ( "java/lang/System"
      , makeMethodKey "exit" "(I)V"
      , \[IValue status] -> do
          sbe <- use backend
          let codeStr = case asInt sbe status of
                          Nothing -> "unknown exit code"
                          Just code -> "exit code " ++ show code
          errorPath . FailRsn
            $ "java.lang.System.exit(int status) called with " ++ codeStr
      )
      -- java.lang.Float.floatToRawIntBits: override for invocation by
      -- java.lang.Math's static initializer
    , ( "java/lang/Float"
      , makeMethodKey "floatToRawIntBits" "(F)I"
      , \args -> case args of
                   [FValue flt] -> do
                     when (flt /= (-0.0 :: Float)) $
                       abort "floatToRawIntBits: overridden for -0.0f only"
                     pushValue =<< withSBE (\sbe -> IValue <$> termInt sbe 0x80000000)
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
                     pushValue =<< withSBE (\sbe -> LValue <$> termLong sbe 0x8000000000000000)
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
      , \_ -> pushValue =<< withSBE (\sbe -> IValue <$> termInt sbe 1)
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
                     pushInstanceMethodCall
                     cn
                     (makeMethodKey "run" "()Ljava/lang/Object;")
                     a [] Nothing
                   _ -> abort "doPrivileged called with incorrect arguments"
      )
    ]
  where
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
            sbe <- use backend
            case st of
              IValue (asInt sbe -> Just{}) -> return ()
              LValue (asLong sbe -> Just{}) -> return ()
              _ -> warn
            sr        <- refFromString . render . ppValue sbe $ st
            void $ execInstanceMethod cn redir this [RValue sr]
        )

lookupStringRef :: Ref -> Simulator sbe m (Maybe String)
lookupStringRef r =
  lookup r. map (\(a,b) -> (b,a)) . M.assocs <$> use strings

-- | Extract the string from the given reference to a java.lang.String
-- contains concrete characters.
drefString :: MonadSim sbe m => Ref -> Simulator sbe m String
drefString strRef = do
  Just ty <- typeOf strRef
  assert (ty == stringTy)

  m <- getMem "drefString"
  let iflds  = m^.memInstanceFields
      lkup   = (`M.lookup` iflds) . (,) strRef
      fldIds = [ FieldId "java/lang/String" "value"  (ArrayType CharType)
               , FieldId "java/lang/String" "count"  IntType
               , FieldId "java/lang/String" "offset" IntType
               ]
  case mapMaybe lkup fldIds of
    [RValue arrRef, IValue cnt, IValue off] -> do
      chars <- getIntArray arrRef
      sbe <- use backend
      when (any (not . isJust . asInt sbe) $ cnt:off:chars) $
        abort "Unable to dereference symbolic strings"
      let cvt = fromIntegral . fromJust . asInt sbe
      return $ take (cvt cnt) $ drop (cvt off) $ map (toEnum . cvt) chars
    _ -> error "Invalid field name/type for java.lang.String instance"





-- setLocal :: LocalVariableIndex -> Value (SBETerm sbe) -> Simulator sbe m ()

-- | Create an instance of the @Class@ class; should only be called once per class
newClass :: MonadSim sbe m => String -> Simulator sbe m Ref
newClass cName = do
  -- As with String above, creating a proper Class object is tricky,
  -- and intimately tied with things like reflection. We probably want
  -- to REVISIT this, as well.
  initializeClass "java/lang/Class"
  ref <- genRef (ClassType "java/lang/Class")
  str <- newString cName
  setInstanceFieldValue ref (FieldId "java/lang/Class" "name" stringTy) (RValue str)
  return ref

-- | Obtain the string value of the name field for the given instance of class
-- @Class@
getClassName :: MonadSim sbe m => Ref -> Simulator sbe m String
getClassName classRef@(Ref _ (ClassType "java/lang/Class")) = do
  drefString =<< unRValue <$> getInstanceFieldValue classRef
                                (FieldId "java/lang/Class" "name" stringTy)
getClassName _ = error "getClassName: wrong argument type"

-- | Returns (the Value) 'true' if the given class name represents an array class
-- (using java.lang.Class naming conventions)
classNameIsArray :: MonadSim sbe m
                 => String 
                 -> Simulator sbe m (Value (SBETerm sbe))
classNameIsArray s = withSBE $ \sbe -> IValue <$> termInt sbe v
  where v = if classNameIsArray' s then 1 else 0

classNameIsArray' :: String -> Bool
classNameIsArray' ('[':_) = True
classNameIsArray' _       = False

-- | Returns (the Value) 'true' if the given class name represents a primtive
-- type (using java.lang.Class naming conventions)
classNameIsPrimitive :: MonadSim sbe m 
                     => String
                     -> Simulator sbe m (Value (SBETerm sbe))
classNameIsPrimitive s = withSBE $ \sbe -> IValue <$> termInt sbe v 
  where v = if classNameIsPrimitive' s then 1 else 0
    
classNameIsPrimitive' :: String -> Bool
classNameIsPrimitive' (ch:[]) = ch `elem` ['B','S','I','J','F','D','Z','C']
classNameIsPrimitive' _       = False

unlessQuiet :: MonadIO m => Simulator sbe m () -> Simulator sbe m ()
unlessQuiet act = getVerbosity >>= \v -> unless (v == 0) act

-- For user feedback that gets silenced when verbosity = 0.
tellUser :: (MonadIO m) => String -> Simulator sbe m ()
tellUser msg = unlessQuiet $ dbugM msg

-- | (dynBind cln meth r act) provides to 'act' the class name that defines r's
-- implementation of 'meth'
dynBind :: MonadSim sbe m
        => String           -- ^ Name of 'this''s class
        -> MethodKey        -- ^ Key of method to invoke
        -> JSRef (Simulator sbe m) -- ^ 'this'
        -> (String -> Simulator sbe m ()) -- ^ e.g., an invokeInstanceMethod invocation.
        -> Simulator sbe m ()
dynBind clName key objectRef act = do
  impls <- dynBind' clName key objectRef    
  case impls of
    cl:_ -> act cl
    _    -> fail . render $ "no implementations found for" 
              <+> text clName <+> ppMethodKey key

-- | (dynBindSuper cln meth r act) provides to 'act' the class name
-- that defines r's implementation of 'meth', with lookup starting
-- from the superclass of r's concrete type. This is used primarily to
-- implement invokespecial
dynBindSuper :: MonadSim sbe m
             => String           -- ^ Name of 'this''s class
             -> MethodKey        -- ^ Key of method to invoke
             -> JSRef (Simulator sbe m) -- ^ 'this'
             -> (String -> Simulator sbe m ()) 
             -- ^ e.g., an invokeInstanceMethod invocation.
             -> Simulator sbe m ()
dynBindSuper clName key objectRef act = do
  impls <- dynBind' clName key objectRef    
  case impls of
    _:cl:_ -> act cl
    _    -> fail . render $ "no super implementations found for" 
              <+> text clName <+> ppMethodKey key

-- | Assumes reference non-null
dynBind' :: MonadSim sbe m
         => String
         -> MethodKey
         -> JSRef (Simulator sbe m)
         -> Simulator sbe m [String]
dynBind' clName key objectRef = do
  dbugM' 6 . render $ "dynBind" <+> text clName <+> ppMethodKey key
  mty <- typeOf objectRef
  case mty of
    Nothing -> fail . render $ "dynBind': could not determine type of reference"
    Just (ClassType instTy) -> do
      cb <- use codebase
      impls <- liftIO $ findVirtualMethodsByRef cb clName key instTy
      dbugM' 6 . render $ "found impls:" <+> sep (map text impls)
      return impls
    Just _ -> fail "dynBind' type parameter not ClassType-constructed"

--------------------------------------------------------------------------------
-- Debugging

-- | Add a breakpoint for the given method
addBreakpoint :: String
              -- ^ class name
              -> MethodKey
              -> Breakpoint
              -> Simulator sbe m ()
addBreakpoint = toggleBreakpoint S.insert

-- | Remove a breakpoint for the given method, doing nothing if one is not present
removeBreakpoint :: String
                 -- ^ class name
                 -> MethodKey
                 -> Breakpoint
                 -> Simulator sbe m ()
removeBreakpoint = toggleBreakpoint S.delete

toggleBreakpoint :: (PC -> Set PC -> Set PC)
                 -> String
                 -- ^ class name
                 -> MethodKey
                 -> Breakpoint
                 -> Simulator sbe m ()
toggleBreakpoint fn clName key bp = do
  cl <- lookupClass clName
  let methodDoc = text clName <> "." <> ppMethodKey key
  method <- case lookupMethod cl key of
              Nothing -> fail . render $ "unknown method:" <+> methodDoc
              Just m -> return m
  bp' <- case breakpointToPC method bp of
           Just pc -> return pc
           Nothing -> fail . render $
             "line number not availble for method:" <+> methodDoc
  mbps <- M.lookup (clName, method) <$> use breakpoints
  let bps = fn bp' $ fromMaybe S.empty mbps
  breakpoints %= M.insert (clName, method) bps

-- | Pretty-prints a symbolic value with an accompanying description
dbugValue :: MonadSim sbe m => String -> Value (SBETerm sbe) -> Simulator sbe m ()
dbugValue desc v = dbugM =<< ((++) (desc ++ ": ")) . render <$> prettyValueSBE v

-- | Dump the symbolic translation of all methods in the given class
dumpSymASTs :: Codebase -> String -> IO ()
dumpSymASTs cb cname = do
  mc <- tryLookupClass cb cname
  case mc of
    Just c -> mapM_ (dumpMethod c) $ classMethods c
    Nothing -> putStrLn $ "Main class " ++ cname ++ " not found."
  where ppInst' (pc, i) = show pc ++ ": " ++ ppInst i
        ppSymInst' (mpc, i) =
          maybe "" (\pc -> show pc ++ ": ") mpc ++ render (ppSymInsn i)
        dumpMethod c m =
          case methodBody m of
            Code _ _ cfg _ _ _ _ -> do
              putStrLn ""
              putStrLn . className $ c
              putStrLn . show . methodKey $ m
              putStrLn ""
              mapM_ (putStrLn . ppInst') . concatMap bbInsts $ allBBs cfg
              putStrLn ""
              mapM_ dumpBlock . fst $ liftCFG cfg
            _ -> return ()
        dumpBlock b = do
          putStrLn . render . ppBlockId . sbId $ b
          mapM_ (\i -> putStrLn $ "  " ++ ppSymInst' i) $ sbInsns b
