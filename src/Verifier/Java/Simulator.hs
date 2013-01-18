{- |
Module           : $Header$
Description      : The symbolic simulation engine for Java
Stability        : stable
Point-of-contact : jstanley
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Verifier.Java.Simulator
  ( module Verifier.Java.Codebase
  , Simulator (SM)
  , State(..)
  , SEH(..)
  , calldefine
  , calldefine_
  , defaultSEH
  , lookupSymbolDef
  , getProgramReturnValue
  , getProgramFinalMem
  , EvalContext
  , getEvalContext
  , getTypedTerm'
  , prettyTermSBE
  , runSimulator
  , withSBE
--  , withSBE'
  , getSizeT
  -- * Memory operations
  , alloca
  , load
  , setStaticFieldValue
  -- for testing
  , dbugM
--  , dbugTerm
--  , dbugTypedTerm
--  , dumpMem
  , getMem
  , setSEH
  , warning
  ) where

import Control.Applicative hiding (empty)
import Control.Lens hiding (Path)
import Control.Monad
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.State (get)

import Data.Array
import Data.Int
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Text.PrettyPrint

import Data.JVM.Symbolic.AST
import Execution.JavaSemantics hiding (createAndThrow, throwNullPtrExc, newObject, initializeClass, setStaticFieldValue)
import Language.JVM.Common
import Language.JVM.Parser
import Verifier.Java.Codebase
import Verifier.Java.Backend
import Verifier.Java.Common
import Verifier.Java.Utils

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
  sbe <- use backend
  tcnt <- liftIO $ termInt sbe cnt
  Just m <- getMem
  setMem (m & memScalarArrays %~ M.insert r (tcnt, arr))
  return r
newSymbolicArray _ _ _ = fail "internal: newSymbolicArray called with invalid type"

-- | Returns length and symbolic value associated with array reference,
-- and nothing if this is not an array reference.
getSymbolicArray :: MonadSim sbe m
                 => Ref -> Simulator sbe m (Maybe (SBETerm sbe, SBETerm sbe))
getSymbolicArray r = do
  Just m <- getMem
  return $ M.lookup r (m^.memScalarArrays)

-- | Sets integer or long array to use using given update function.
-- TODO: Revisit what error should be thrown if ref is not an array reference.
setSymbolicArray :: MonadSim sbe m => Ref -> SBETerm sbe -> Simulator sbe m ()
setSymbolicArray r arr = updateSymbolicArray r (\_ _ _ -> return arr)

-- | Updates integer or long array using given update function.
-- TODO: Revisit what error should be thrown if ref is not an array refence.
updateSymbolicArray :: MonadSim sbe m
                    => Ref
                    -> (Backend sbe -> SBETerm sbe -> SBETerm sbe -> IO (SBETerm sbe))
                    -> Simulator sbe m ()
updateSymbolicArray r modFn = do
  Just m <- getMem
  let (len,arr) = maybe (error "internal: reference is not a symbolic array") id
                      $ M.lookup r (m^.memScalarArrays)
  sbe <- use backend
  newArr <- liftIO $ modFn sbe len arr
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
  Just m <- getMem
  if isRValue tp then do
    sbe <- use backend
    let Just arr = M.lookup ref (m^.memRefArrays)
    liftIO $ termInt sbe (1 + snd (bounds arr))
  else do
    let Just (len,_) = M.lookup ref (m^.memScalarArrays)
    return len

-- | Returns value in array at given index.
getArrayValue :: (AigOps sbe, Functor m, MonadIO m)
              => Ref
              -> SBETerm sbe
              -> Simulator sbe m (Value (SBETerm sbe))
getArrayValue r idx = do
  ArrayType tp <- getType r
  sbe <- use backend
  Just m <- getMem
  if isRValue tp then do
    let Just arr = M.lookup r (m^.memRefArrays)
    case asInt sbe idx of
      Just i -> return $ RValue (arr ! fromIntegral i)
      _ -> fail "Not supported: symbolic indexing into arrays of references."
  else if tp == LongType then
    liftIO $ do
      let Just (l,rslt) = M.lookup r (m^.memScalarArrays)
      LValue <$> termGetLongArray sbe l rslt idx
  else do
    assert (isIValue tp)
    liftIO $ do
      let Just (l,rslt) = M.lookup r (m^.memScalarArrays)
      IValue <$> termGetIntArray sbe l rslt idx

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

setArrayValue r idx (IValue val) = updateSymbolicArray r $ \sbe l a ->
  termSetIntArray  sbe l a idx val
setArrayValue r idx (LValue val) = updateSymbolicArray r $ \sbe l a ->
  termSetLongArray sbe l a idx val
setArrayValue r idx (RValue v) = do
  sbe <- use backend
  (Just m) <- getMem
  case asInt sbe idx of
    Nothing -> fail "Cannot update reference arrays at symbolic indices"
    Just i -> do
      let updateFn arr = Just (arr // [(fromIntegral i,v)])
      setMem (m & memRefArrays %~ M.update updateFn r)

setInstanceFieldValue r fieldId v = do
  Just m <- getMem
  setMem (m & memInstanceFields %~ M.insert (r, fieldId) v)

setStaticFieldValue fieldId v = do
  Just m <- getMem
  setMem (m & memStaticFields %~ M.insert fieldId v)

-- | Returns type of reference and throws null pointer exception if reference is null.
getType :: (AigOps sbe, Functor m, MonadIO m) => Ref -> Simulator sbe m Type
getType NullRef    = throwNullPtrExc
getType (Ref _ tp) = return tp


tryModifyCS :: Monad m => String -> (CS sbe -> Maybe (CS sbe)) -> Simulator sbe m ()
tryModifyCS ctx f = ctrlStk %= fn
  where fn = fromMaybe (error err) . f
          where err = "internal: tryModifyCS " ++ show ctx

-- | Obtain the first pending path in the topmost merge frame; @Nothing@ means
-- that the control stack is empty or the top entry of the control stack has no
-- pending paths recorded.
getPath :: (Functor m, Monad m) => Simulator sbe m (Maybe (Path sbe))
getPath = uses ctrlStk currentPath

-- @getMem@ yields the memory model of the current path, which must exist.
getMem :: (Functor m, Monad m) => Simulator sbe m (Maybe (Memory (SBETerm sbe)))
getMem = fmap (view pathMemory <$>) getPath

-- @setMem@ sets the memory model in the current path, which must exist.
setMem :: (Functor m, Monad m) => Memory (SBETerm sbe) -> Simulator sbe m ()
setMem mem = tryModifyCS "setMem" $ modifyPath $ set pathMemory mem

withSBE :: MonadIO m
        => (Backend sbe -> IO a) -> Simulator sbe m a
withSBE f = liftIO =<< uses backend f

setSEH :: Monad m => SEH sbe m -> Simulator sbe m ()
setSEH = assign evHandlers

ppCurrentMethod :: Path' term -> Doc
ppCurrentMethod p = case currentCallFrame p of
  Nothing -> "<no current method>"
  Just cf -> text (cf^.cfClass) <> "." <> ppMethod (cf^.cfMethod)

warning :: (Functor m, MonadIO m) => Doc -> Simulator sbe m ()
warning msg = do
  mp <- getPath
  -- Get location information
  let prefix = do
        p <- mp
        let fn = ppCurrentMethod p
        return $ case p^.pathBlockId of
          Nothing -> "at" <+> fn
          Just cb -> "at" <+> fn <> colon <> ppBlockId cb        
  liftIO . putStrLn . render $ 
    "Warning" <> maybe empty (space <>) prefix <> colon <+> msg

-- | Converts integral into bounded num class.
-- TODO: Revisit error handling when integer is out of range.
safeCast :: (Integral s, Bounded t, Integral t, Num t) => s -> t
safeCast = impl minBound maxBound . toInteger
  where impl :: Integral t => t -> t -> Integer -> t
        impl minb maxb s
          | toInteger minb <= s && s <= toInteger maxb = fromInteger s
          | otherwise = error "internal: safeCast argument out of range"

initializeClass = undefined 
{-
initializeClass name = do
  ps <- getPathState
  case M.lookup name (initialization ps) of
    Nothing -> do
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
                  when (fieldIsStatic f) $ do
                    val <- withSBE $ \sbe -> defaultValue sbe (fieldType f)
                    setStaticFieldValue fieldId val
                Just tp -> error $ "Unsupported field type" ++ show tp
      cb <- getCodebase
      cl <- liftIO $ lookupClass cb name
      mapM_ initializeField $ classFields cl
      case cl `lookupMethod` (MethodKey "<clinit>" [] Nothing) of
        Just method -> do
          setInitializationStatus name Started
          unless (skipInit name) $ do

            pushCallFrame name method []
            runFrame
        Nothing -> return ()
      runCustomClassInitialization cl
      setInitializationStatus name Initialized
    Just Erroneous -> do
      createAndThrow "java/lang/NoClassDefFoundError"
    Just Started -> return ()
    Just Initialized -> return ()
-}

newObject :: (Functor m, MonadIO m) => String -> Simulator sbe m Ref
newObject name = do
  initializeClass name
  ref <- genRef (ClassType name)
  -- Set fields to default value
  cb <- use codebase
  fields <- liftIO $ classFields <$> lookupClass cb name
  fvals <- withSBE $ \sbe -> mapM (defaultValue sbe . fieldType) fields
  Just m <- getMem
  let m' = m & memInstanceFields .~
             foldl' (\fieldMap (f,val) ->
                        let fid = FieldId name (fieldName f) (fieldType f)
                        in val `seq` M.insert (ref,fid) val fieldMap)
             (m^.memInstanceFields)
             (fields `zip` fvals)
  setMem m'
  return ref

errorPath ::
  ( MonadIO m
  , Functor m
  )
  => FailRsn -> Simulator sbe m a
errorPath rsn = do
  sbe <- use backend
  -- Update control stack.
  Just p <- getPath
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


-- | Creates an exception of the given class (which is assumed to have a no
-- argument constructor) and throws it.
createAndThrow :: (Functor m, MonadIO m) => String -> Simulator sbe m a
createAndThrow clName = do
  ref <- newObject clName
  ctrlStk %= modifyCurrentPath (\p -> 
    let exc = JavaException ref (p^.pathStack)
    in p & pathException .~ Just exc)
  errorPath $ FailRsn clName

throwNullPtrExc :: (Functor m, MonadIO m) => Simulator sbe m a
throwNullPtrExc = createAndThrow "java/lang/NullPointerException"


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