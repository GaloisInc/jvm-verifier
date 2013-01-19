{- |
Module           : $Header$
Description      : The symbolic simulation engine for Java
Stability        : stable
Point-of-contact : jstanley
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.Char
import Data.Int
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector as V

import Text.PrettyPrint

import Data.JVM.Symbolic.AST
import Data.JVM.Symbolic.Translation
import Execution.JavaSemantics hiding (createAndThrow, throwNullPtrExc, newObject, initializeClass, setStaticFieldValue, getCodebase)
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

setStaticFieldValue :: MonadSim sbe m
                    => FieldId -> Value (SBETerm sbe) -> Simulator sbe m ()
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

initializeClass :: MonadSim sbe m => String -> Simulator sbe m ()
initializeClass name = do
  Just m <- getMem
  case M.lookup name (m^.memInitialization) of
    Nothing -> do
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
      cb <- use codebase
      cl <- liftIO $ lookupClass cb name
      mapM_ initializeField $ classFields cl
      case cl `lookupMethod` clinit of
        Just method -> do          
          Just m <- getMem
          setMem $ setInitializationStatus name Started m
          unless (skipInit name) $ do
            Just symBlocks <- liftIO $ lookupSymbolicMethod cb name clinit
            cs <- use ctrlStk
            let Just cs'      = pushCallFrame name method entryBlock M.empty cs
                Just symInsns = M.lookup entryBlock (symBlockMap symBlocks)
            runInsns symInsns
        Nothing -> return ()
      runCustomClassInitialization cl
      Just m <- getMem
      setMem $ setInitializationStatus name Initialized m
    Just Erroneous -> do
      createAndThrow "java/lang/NoClassDefFoundError"
    Just Started -> return ()
    Just Initialized -> return ()

-- | Low-level method call for when we need to make a method call not
-- prescribed by the symbolic instructions. This arises mainly in
-- class initialization, as well as synthetic code as for
-- printStreams.
runMethod :: MonadSim sbe m 
          => String
          -> MethodKey
          -> M.Map LocalVariableIndex (Value (SBETerm sbe)) 
          -> Simulator sbe m (Maybe (Value (SBETerm sbe)))
runMethod clName key locals = do
  cb <- use codebase
  cl <- liftIO $ lookupClass cb clName 
  Just p <- getPath
  Just symBlocks <- liftIO $ lookupSymbolicMethod cb clName key
  let Just method = cl `lookupMethod` key
      Just curId  = p^.pathBlockId  
  cs <- use ctrlStk
  let Just cs'      = pushCallFrame clName method curId locals cs
      Just symInsns = M.lookup entryBlock (symBlockMap symBlocks)
  runInsns symInsns
  Just p' <- getPath
  return (p'^.pathRetVal)


runCustomClassInitialization :: MonadSim sbe m => Class -> Simulator sbe m ()
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

newObject :: MonadSim sbe m => String -> Simulator sbe m Ref
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
createAndThrow :: MonadSim sbe m => String -> Simulator sbe m a
createAndThrow clName = do
  ref <- newObject clName
  ctrlStk %= modifyCurrentPath (\p -> 
    let exc = JavaException ref (p^.pathStack)
    in p & pathException .~ Just exc)
  errorPath $ FailRsn clName

throwNullPtrExc :: MonadSim sbe m => Simulator sbe m a
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

-- REVISIT: it may make sense for this to be dynamic
skipInit :: String -> Bool
skipInit cname = cname `elem` [ "java/lang/System"
                              , "java/io/Reader"
                              , "java/io/InputStreamReader"
                              ]

runInsns :: MonadSim sbe m
         => [SymInsn] -> Simulator sbe m ()
runInsns = mapM_ dbugStep

dbugStep :: MonadSim sbe m 
         => SymInsn -> Simulator sbe m ()
dbugStep insn = do
  mp <- getPath
  case mp of
    Nothing -> dbugM' 2 $ "Executing: (no current path): " ++ show (ppSymInsn insn)
    Just p  -> do
      dbugM' 2 $ "Executing ("
                 ++ "#" ++ show (p^.pathName) ++ "): "
                 ++ maybe "" (show . parens . ppBlockId) (p^.pathBlockId)
                 ++ ": " ++
                 case insn of
                   PushPendingExecution{} -> "\n"
                   _ -> ""
                 ++ show (ppSymInsn insn)
--  repl
  cb1 onPreStep insn
  step insn
  cb1 onPostStep insn
  whenVerbosity (>=5) dumpCtrlStk

step = undefined

--------------------------------------------------------------------------------
-- Callbacks and event handlers

cb1 :: (Functor m, Monad m)
  => (SEH sbe m -> a -> Simulator sbe m ()) -> a -> Simulator sbe m ()
cb1 f x = join (f <$> use evHandlers <*> pure x)

{-
cb2 :: (Functor m, Monad m)
  => (SEH sbe m -> a -> b -> Simulator sbe m ()) -> a -> b -> Simulator sbe m ()
cb2 f x y = join $ gets (f . evHandlers) <*> pure x <*> pure y
-}
defaultSEH :: Monad m => SEH sbe m
defaultSEH = SEH
               (return ())
               (\_   -> return ())
               (\_   -> return ())

--------------------------------------------------------------------------------
-- Conversions
longFromInt x = withSBE $ \sbe -> termLongFromInt sbe x
intFromLong x = withSBE $ \sbe -> termIntFromLong sbe x

compareFloat :: (Floating a, Ord a, MonadSim sbe m) 
             => a -> a -> Simulator sbe m (SBETerm sbe)
compareFloat x y = withSBE $ \sbe -> termInt sbe (fromIntegral v)
  where v = fromEnum (compare x y) - 1

floatRem :: (RealFrac a) => a -> a -> a
floatRem x y = fromIntegral z
  where z :: Integer
        z = truncate x `rem` truncate y

refFromString :: MonadSim sbe m => String -> Simulator sbe m Ref
refFromString val = do
  sMap <- use strings
  case M.lookup val sMap of
    Just ref -> return ref
    Nothing  -> do
      ref <- newString val
      strings %= M.insert val ref
      return ref

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
{-
  -- Negate a Boolean value
  bNot x = withSBE $ \sbe -> termNot sbe x 

  -- (x &&& y) returns logical and
  mx &&& my = do
    sbe <- gets backend
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
  floatFromDouble  = return . fromRational . toRational
  intFromDouble x  = withSBE $ \sbe -> termInt sbe (truncate x)
  longFromDouble x = withSBE $ \sbe -> termLong sbe (truncate x)
  doubleFromFloat  = return . fromRational . toRational
-}
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
  lXor  x y = withSBE $ \sbe -> termIXor sbe x y
{-
  intFromFloat x   = withSBE $ \sbe -> termInt sbe (truncate x)
  longFromFloat x  = withSBE $ \sbe -> termLong sbe (truncate x)
  doubleFromInt  i = do
    sbe <- gets backend
    case asInt sbe i of
      Just n -> return (fromIntegral n)
      Nothing -> error "cannot convert symbolic int to double"
  floatFromInt i = do
    sbe <- gets backend
    case asInt sbe i of
      Just n -> return (fromIntegral n)
      Nothing -> error "cannot convert symbolic int to float"
  doubleFromLong l = do
    sbe <- gets backend
    case asLong sbe l of
      Just n -> return (fromIntegral n)
      Nothing -> error "cannot convert symbolic long to double"
  floatFromLong  l = do
    sbe <- gets backend
    case asLong sbe l of
      Just n -> return $ fromIntegral n
      Nothing -> error "cannot convert symbolic long to float"


  -- (arrayLength ref) return length of array at ref.
  arrayLength ref = do
    pd  <- getPSS
    getArrayLength pd ref


  -- (newMultiArray tp len) returns a reference to a multidimentional array with
  -- type tp and len = [len1, len2, ...] where len1 equals length of first
  -- dimention len2 equals length of second dimension and so on.  Note: Assumes
  -- all integer values are nonnegative.
  newMultiArray tp []
    | isRValue tp = return NullRef
  newMultiArray tp@(ArrayType eltType) [l]
    | isIValue eltType || (eltType == LongType) = do
      sbe <- gets backend
      ref <- genRef tp
      let arrayFn | eltType == LongType = termLongArray
                  | otherwise           = termIntArray
      ma <- liftIO $ arrayFn sbe l
      case ma of
        Nothing -> abort "Cannot create array with symbolic size"
        Just a ->
          modifyPathState $ \ps ->
            ps { arrays = M.insert ref (l, a) (arrays ps) }
      return ref
  newMultiArray (ArrayType DoubleType) _ =
    abort "Floating point arrays (e.g., double) are not supported"
  newMultiArray (ArrayType FloatType) _ =
    abort "Floating point arrays (e.g., float) are not supported"
  newMultiArray tp@(ArrayType eltTp) (tcnt : rest) = do
    sbe <- gets backend
    case asInt sbe tcnt of
      Nothing -> abort "Cannot create array of references with symbolic size"
      Just cnt -> do
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
   cb <- use codebase
   fields <- liftIO $ classFields <$> lookupClass cb name
   fvals <- withSBE $ \sbe -> mapM (defaultValue sbe . fieldType) fields
   modifyPathState $ \ps ->
     ps { instanceFields =
            foldl' (\fieldMap (f,val) ->
                      let fid = FieldId name (fieldName f) (fieldType f)
                       in val `seq` M.insert (ref,fid) val fieldMap)
                   (instanceFields ps)
                   (fields `zip` fvals)
        }
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


  typeOf NullRef    = return Nothing
  typeOf (Ref _ ty) = return (Just ty)

  coerceRef NullRef _    = return NullRef
  coerceRef (Ref x _) ty = return (Ref x ty)

  -- Retuns predicate indicating super class of ref has given type.
  superHasType ref tp = do
    ClassType refClassname <- getType ref
    cb <- use codebase
    withSBE $ \sbe -> do
      cl <- lookupClass cb refClassname
      b <- case superClass cl of
             Just super -> isSubtype cb (ClassType super) (ClassType tp)
             Nothing    -> return False
      termBool sbe b

  -- (rEq x y) returns boolean formula that holds if x == y.
  rEq x y = withSBE $ \sbe -> termBool sbe (x == y)

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
      Nothing    -> do
        when (isFloatType (fieldIdType fieldId)) $
          error $ "internal: unassigned floating-point instance field "
                    ++ show fieldId ++ " in " ++ show r
        val <- withSBE $ \sbe -> defaultValue sbe (fieldIdType fieldId)
        pushValue val

  -- Pushes value of field onto stack.
  -- NOTE: Assumes ref is not null.
  pushStaticFieldValue fieldId = do
    ps <- getPathState
    case M.lookup fieldId (staticFields ps) of
      Just value -> pushValue value
      Nothing    -> do
        when (isFloatType (fieldIdType fieldId)) $
          error $ "internal: unassigned static floating-point field " ++
                        show fieldId
        pushValue =<< withSBE (\sbe -> defaultValue sbe (fieldIdType fieldId))

  -- (pushArrayValue ref index) pushes the value of the array at index to the stack.
  -- NOTE: Assumes that ref is a valid array and index is a valid index in array.
  pushArrayValue r idx = do
    pd  <- getPSS
    val <- getArrayValue pd r idx
    pushValue val

  setArrayValue r idx (IValue val) = updateSymbolicArray r $ \sbe l a ->
    termSetIntArray  sbe l a idx val
  setArrayValue r idx (LValue val) = updateSymbolicArray r $ \sbe l a ->
    termSetLongArray sbe l a idx val
  setArrayValue r idx (RValue v) = do
    sbe <- gets backend
    ps <- getPathState
    case asInt sbe idx of
      Nothing -> abort "Cannot update reference arrays at symbolic indices"
      Just i ->
        let updateFn arr = Just (arr // [(fromIntegral i,v)])
            ps'          = ps{ refArrays = M.update updateFn r (refArrays ps) }
         in refArrays ps' M.! r `seq` putPathState ps'
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
            error $ "internal: Undefined local variable " ++ show i
      _ -> error "Frames are empty"

  setLocal i v = do
    ps <- getPathState
    let Call st m pc lvars stack : rest = frames ps
    putPathState ps { frames = Call st m pc (M.insert i v lvars) stack : rest
                    }

  printStream nl _ []       = liftIO $ (if nl then putStrLn else putStr) "" >> hFlush stdout
  printStream nl binary [x] = do
    sbe <- gets backend
    let putStr' s = liftIO $ (if nl then putStrLn else putStr) s >> hFlush stdout
    case x of
      IValue (asInt sbe -> Just v)
        | binary    -> putStr' [chr $ fromEnum v]
        | otherwise -> putStr' $ show v
      v@IValue{} -> putStr' $ ppValue v

      LValue (asLong sbe -> Just v) -> putStr' $ show v
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

  createInstance clNm margs = do
    cb <- use codebase
    cl <- liftIO $ lookupClass cb clNm
    case cl `lookupMethod` ctorKey of
      Just method -> do
        ref <- newObject clNm
        runInstanceMethodCall clNm method ref (maybe [] (map snd) margs)
        return ref
      Nothing ->
        error $ "Unable to find method " ++ clNm ++ " (signature mismatch?)"
    where
      ctorKey = MethodKey "<init>" (maybe [] (map fst) margs) Nothing

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

  invokeInstanceMethod cName key objectRef args = do
    cb <- use codebase
    cl <- liftIO $ lookupClass cb cName
    case cl `lookupMethod` key of
       Just method -> pushInstanceMethodCall cName method objectRef args
       Nothing -> error $
         "Could not find instance method " ++ show key ++ " in " ++ cName
           ++ "\n  objectRef = " ++ show objectRef ++ ", args = " ++ show args

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