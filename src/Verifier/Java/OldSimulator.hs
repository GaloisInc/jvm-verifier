import qualified Control.Arrow as CA
import Control.Arrow (second)
import Control.Applicative
import Control.Lens hiding (Path)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State hiding (State)
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
import qualified Data.Vector as V
import Data.Word
import Prelude hiding (catch)
import System.IO (hFlush, stdout)

import Execution
import Execution.JavaSemantics
import Verifier.Java.Backend
import Verifier.Java.Codebase
import Verifier.Java.Common

import Verinf.Utils.LogMonad

-- | Converts integral into bounded num class.
-- TODO: Revisit error handling when integer is out of range.
safeCast :: (Integral s, Bounded t, Integral t, Num t) => s -> t
safeCast = impl minBound maxBound . toInteger
  where impl :: Integral t => t -> t -> Integer -> t
        impl minb maxb s
          | toInteger minb <= s && s <= toInteger maxb = fromInteger s
          | otherwise = error "internal: safeCast argument out of range"


dbugM :: MonadIO m => String -> m ()
dbugM = liftIO . putStrLn

-- | Override behavior of simulator when it encounters a specific instance
-- method to perform a user-definable action.
-- Note: Fails if the method has already been overridden.
overrideInstanceMethod :: String
                       -> MethodKey
                       -> (Ref -> [Value (SBETerm sym)] -> Simulator sbe ())
                       -> Simulator sbe ()
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
  AigOps sym
  => String
  -> MethodKey
  -> ([Value (SBETerm sym)] -> Simulator sbe ())
  -> Simulator sbe ()
overrideStaticMethod cName mKey action = do
  s <- get
  let key = (cName, mKey)
  when (key `M.member` staticOverrides s) $
    abort $ "Method " ++ cName ++ "." ++ methodKeyName mKey ++ " is already overridden."
  staticOverrides %= M.insert key action

-- | Register all predefined overrides for builtin native implementations.
stdOverrides :: AigOps sbe => Simulator sbe ()
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
            cb <- getCodebase
            cl <- liftIO $ lookupClass cb nativeClass
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
                     invokeInstanceMethod
                     cn
                     (makeMethodKey "run" "()Ljava/lang/Object;")
                     a []
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
            sbe <- gets backend        
            case st of
              IValue (asInt sbe -> Just{}) -> return ()
              LValue (asLong sbe -> Just{}) -> return ()
              _ -> warn
            sr        <- refFromString (ppValue st)
            cb <- getCodebase
            Just meth <- liftIO ((`lookupMethod` redir) <$> lookupClass cb cn)
            runInstanceMethodCall cn meth this [RValue sr]
        )

{-
-- (pushCallFrame st m vals) pushes frame to the stack for PC 0.
pushCallFrame :: String
              -> Method
              -> [Value (SBETerm sym)]
              -> Simulator sbe ()
pushCallFrame name method vals = pushFrame call
  where
    call = Call name method 0 localMap []
    localMap = M.fromList (snd $ foldl setupLocal (0, []) vals)
    setupLocal (n, acc) v@LValue{} = (n + 2, (n, v) : acc)
    setupLocal (n, acc) v@DValue{} = (n + 2, (n, v) : acc)
    setupLocal (n, acc) v          = (n + 1, (n, v) : acc)
-}

-- | Returns a new reference value with the given type, but does not
-- initialize and fields or array elements.
-- Note: Assumes class for reference has already been initialized.
genRef :: Type -> Simulator sbe Ref
genRef tp = do
  s <- get
  let r = nextRef s
  put s { nextRef = r + 1 }
  return $ Ref r tp

-- | Returns initialization status of class
getInitializationStatus :: String -> Simulator sbe (Maybe InitializationStatus)
getInitializationStatus cName = do
  ps <- getPathState
  return $ M.lookup cName (initialization ps)

-- | Sets initialization status of class with given name.
setInitializationStatus :: String -> InitializationStatus -> Simulator sbe ()
setInitializationStatus cName status =
  modifyPathState $ \ps ->
    ps { initialization = M.insert cName status (initialization ps) }

setFinalResult :: FinalResult term -> Path term -> Path term
setFinalResult = set finalResult

popFrameImpl :: Bool -> Simulator sbe ()
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
frameCount :: Simulator sbe Int
frameCount = length . frames <$> getPathState

runFrame :: AigOps sbe => Simulator sbe ()
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
runCustomClassInitialization :: AigOps sbe => Class -> Simulator sbe ()
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

compareFloat :: ( Floating a, Ord a) => a -> a -> Simulator sbe (SBETerm sym)
compareFloat x y = withSBE $ \sbe -> termInt sbe (fromIntegral v)
  where v = fromEnum (compare x y) - 1

--------------------------------------------------------------------------------
-- Symbolic simulation semantics

type instance JSDouble (Simulator sym) = Double
type instance JSFloat  (Simulator sym) = Float
type instance JSInt    (Simulator sym) = SBETerm sym
type instance JSLong   (Simulator sym) = SBETerm sym
type instance JSRef    (Simulator sym) = Ref
type instance JSBool   (Simulator sym) = SBETerm sym
type instance JSRslt   (Simulator sym) = [(PathDescriptor, FinalResult (SBETerm sym))]

withSBE :: (Backend sbe -> IO a) -> Simulator sbe a
withSBE f = liftIO . f =<< gets backend
    
--------------------------------------------------------------------------------
-- Path state merging

data MergePrecond
  = ArraySizesMatch
  | RefArraysEqual
  | StackFramesAlign
    deriving Show

merge :: forall sbe . AigOps sym
      => SymPath sym
      -> SymPath sym
      -> Simulator sbe (Maybe (SymPath sym))
merge from@PathState{ finalResult = fromFR } to@PathState{ finalResult = toFR } = do
  -- A path that has thrown an exception should not have been identified as a
  -- merge candidate!
  assert (not (isExc fromFR) && not (isExc toFR))

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
        dbugM $ "merge: "
                ++ show $ pathStSel from
                ++ " => "
                ++ show $ pathStSel to
        whenVerbosity (>=5) $ do
          dbugM $ "From state:" ++ show from
          dbugM $ "To state:" ++ show to

      Just <$> to & finalResult   %%~ const mergeRVs
                >>= psAssumptions %%~ const pathConds
                >>= arrays        %%~ const mergeArrays
                >>= refArrays     %%~ const mergeRefArrays
                >>= instanceFields %%~ const mergeInstanceFields
                >>= staticFields %%~ const mergeStaticFields
                >>= frame %%~ const mergeFrm
    else do
      return Nothing
  where
    t <-> f = do
      -- NB: The term equality check below is about correctness as well as
      -- performance.  If we don't do this, we can obtain terms that are
      -- "superfluously" symbolic, which can make code that should trivially
      -- terminate (e.g., because its termination conditions are concrete) no
      -- longer do so.
      sbe <- gets backend
      liftIO $ termIte sbe (psAssumptions from) t f
    --
    same f              = f from == f to
    pathConds           = return (psAssumptions from) ||| return (psAssumptions to)
    mergeRefArrays      = return $ refArrays from `M.union` refArrays to
    mergeArrays         = arrays `mergeBy` \(l1,a1) (l2,a2) -> do
                            liftM2 (,) (l1 <-> l2) (a1 <-> a2)
    mergeInstanceFields = instanceFields `mergeBy` mergeV
    mergeStaticFields   = staticFields `mergeBy` mergeV
    mergeFrm            = do
      -- Merge the top frame of both states.
      case (from^.frame, to^.frame) of
        (frm1, frm2) -> do
          locals <- view (frame.frmLocals) `mergeBy` mergeV
          opds   <- zipWithM mergeV (frmOpds frm1) (frmOpds frm2)
          return $ frm2 & frmLocals .~ locals & frmOpds .~ opds
        _ -> fail "frame mismatch (uncaught merge precondition violation?)"
    --
    mergeRVs = do
      assert (not (isBkpt fromFR || isBkpt toFR))
      fromMaybe (finalResult to)
      <$> case (fromFR, toFR) of
            (ReturnVal v1, ReturnVal v2) ->
              Just . ReturnVal <$> (v1 `mergeV` v2)
            _ -> return Nothing
    --
    --
    intersectBy selector combineFn =
      M.intersectionWith combineFn (selector from) (selector to)
    --
    checkPreconds =
      return
      $ (\xs -> case xs of [] -> Nothing ; _ -> Just xs)
      $ map fst . filter (not . snd)
      $
      [
        -- 1) The refArray maps in the intersection of the path states are the
        -- same
        ( RefArraysEqual
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

      , -- 3) Both PathStates have the same stack frame (same
        -- method, PC, operand stack, etc.)
        ( StackFramesAlign
        , let opdTypesMatch DValue{} DValue{} = True
              opdTypesMatch FValue{} FValue{} = True
              opdTypesMatch IValue{} IValue{} = True
              opdTypesMatch LValue{} LValue{} = True
              opdTypesMatch RValue{} RValue{} = True
              opdTypesMatch AValue{} AValue{} = True
              opdTypesMatch _ _ = False
          in case (from^.frame, to^.frame) of
               (Call c1 m1 pc1 _ opds1, Call c2 m2 pc2 _ opds2) ->
                      and [ c1 == c2
                          , methodKey m1 == methodKey m2
                          , pc1 == pc2
                            -- (above) PCs differ -> both frames at a return instruction
                          , length opds1 == length opds2
                          , all (uncurry opdTypesMatch) $
                              opds1 `zip` opds2
                          ]
        )
      ]

--------------------------------------------------------------------------------
-- Misc utilities

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

-- | Create a new merge frame
-- mergeFrame :: [(PathDescriptor, ResumeAction sym)] -> MergeFrame sym
-- mergeFrame paths = MergeFrame [] paths CallExit


-- | Extract the string from the given reference to a java.lang.String
-- contains concrete characters.
drefString :: AigOps sbe => Ref -> Simulator sbe String
drefString strRef = do
  Just ty       <- typeOf strRef
  assert (ty == stringTy)

  p <- peekPending "drefString"
  let iflds  = p^.instanceFields
      lkup   = (`M.lookup` iflds) . (,) strRef
      fldIds = [ FieldId "java/lang/String" "value"  (ArrayType CharType)
               , FieldId "java/lang/String" "count"  IntType
               , FieldId "java/lang/String" "offset" IntType
               ]
  case mapMaybe lkup fldIds of
    [RValue arrRef, IValue cnt, IValue off] -> do
      chars <- getIntArray p arrRef
      sbe <- gets backend
      when (any (not . isJust . asInt sbe) $ cnt:off:chars) $
        abort "Unable to dereference symbolic strings"
      let cvt = fromIntegral . fromJust . asInt sbe
      return $ take (cvt cnt) $ drop (cvt off) $ map (toEnum . cvt) chars
    _ -> error "Invalid field name/type for java.lang.String instance"

-- | Create an instance of the @Class@ class, if it doesn't already exist.
newClass :: AigOps sbe => String -> Simulator sbe Ref
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
getClassName :: AigOps sbe => Ref -> Simulator sbe String
getClassName classRef@(Ref _ (ClassType "java/lang/Class")) = do
  p <- peekPending "getClassName"
  drefString =<< unRValue <$> getInstanceFieldValue p classRef
                                (FieldId "java/lang/Class" "name" stringTy)
getClassName _ = error "getClassName: wrong argument type"

-- | Returns (the Value) 'true' if the given class name represents an array class
-- (using java.lang.Class naming conventions)
classNameIsArray :: String -> Simulator sbe (Value (SBETerm sym))
classNameIsArray s = withSBE $ \sbe -> IValue <$> termInt sbe v
  where v = if classNameIsArray' s then 1 else 0

classNameIsArray' :: String -> Bool
classNameIsArray' ('[':_) = True
classNameIsArray' _       = False

-- | Returns (the Value) 'true' if the given class name represents a primtive
-- type (using java.lang.Class naming conventions)
classNameIsPrimitive :: String -> Simulator sbe (Value (SBETerm sym))
classNameIsPrimitive s = withSBE $ \sbe -> IValue <$> termInt sbe v 
  where v = if classNameIsPrimitive' s then 1 else 0
    
classNameIsPrimitive' :: String -> Bool
classNameIsPrimitive' (ch:[]) = ch `elem` ['B','S','I','J','F','D','Z','C']
classNameIsPrimitive' _       = False

lookupStringRef :: Ref -> Simulator sbe (Maybe String)
lookupStringRef r =
  lookup r. map (\(a,b) -> (b,a)) . M.assocs <$> gets strings

-- Array Operations {{{1
-- | Create a new symbolic array with the given type, length and initial value.
-- TODO: Identify appropriate error to throw if type is not an array of IValues or
-- LValues or cnt is negative.
newSymbolicArray :: Type -> Int32 -> SBETerm sbe -> Simulator sbe Ref
newSymbolicArray tp@(ArrayType eltType) cnt arr = do
  assert ((isIValue eltType || eltType == LongType) && cnt >= 0)
  r <- genRef tp
  sbe <- gets backend
  tcnt <- liftIO $ termInt sbe cnt
  modifyPending $ arrays %~ M.insert r (tcnt, arr)
  return r
newSymbolicArray _ _ _ = fail "internal: newSymbolicArray called with invalid type"

-- | Returns length and symbolic value associated with array reference,
-- and nothing if this is not an array reference.
getSymbolicArray :: Ref -> Simulator sbe (Maybe (SBETerm sym, SBETerm sym))
getSymbolicArray r = M.lookup r . view arrays <$> peekPending

-- | Sets integer or long array to use using given update function.
-- TODO: Revisit what error should be thrown if ref is not an array reference.
setSymbolicArray :: Ref -> SBETerm sbe -> Simulator sbe ()
setSymbolicArray r arr = updateSymbolicArray r (\_ _ _ -> return arr)

-- | Updates integer or long array using given update function.
-- TODO: Revisit what error should be thrown if ref is not an array refence.
updateSymbolicArray :: Ref
                    -> (Backend sbe -> SBETerm sbe -> SBETerm sbe -> IO (SBETerm sym))
                    -> Simulator sbe ()
updateSymbolicArray r modFn = do
  withPoppedPending_ "updateSymbolicArray" $ \ps -> do
    let (len,arr) = maybe (error "internal: reference is not a symbolic array") id
                        $ M.lookup r (arrays ps)
    sbe <- gets backend
    newArr <- liftIO $ modFn sbe len arr
    return $ ps & arrays %~ M.insert r (len, newArr)

-- | @newIntArray arTy terms@ produces a reference to a new array of type
-- @arTy@ and populated with given @terms@.
newIntArray :: AigOps sbe => Type -> [SBETerm sym] -> Simulator sbe Ref
newIntArray tp@(ArrayType eltType) values
  | isIValue eltType = do
    sbe <- gets backend
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
newLongArray :: AigOps sbe => [SBETerm sym] -> Simulator sbe Ref
newLongArray values = do
  sbe <- gets backend
  arr <- liftIO $ do
    let v = V.fromList values
    l <- termInt sbe (fromIntegral (V.length v))
    Just a0 <- termLongArray sbe l
    let fn a i = do
          ti <- termInt sbe (fromIntegral i)
          termSetLongArray sbe l a ti (v V.! i)
    V.foldM fn a0 (V.enumFromN 0 (V.length v))
  newSymbolicArray (ArrayType LongType) (safeCast (length values)) arr

-- | Returns array length at given index in path.
getArrayLength :: AigOps sbe => SymPath sbe -> Ref -> Simulator sbe (SBETerm sym)
getArrayLength p ref = do
  ArrayType tp <- getType ref
  if isRValue tp then do
    sbe <- gets backend
    let Just arr = M.lookup ref (refArrays p)
    liftIO $ termInt sbe (1 + snd (bounds arr))
  else do
    let Just (len,_) = M.lookup ref (arrays p)
    return len

-- | Returns value in array at given index.
getArrayValue :: AigOps sym
              => SymPath sym
              -> Ref
              -> SBETerm sym
              -> Simulator sbe (Value' sym)
getArrayValue p r idx = do
  ArrayType tp <- getType r
  sbe <- gets backend
  if isRValue tp then do
    let Just arr = M.lookup r (refArrays p)
    case asInt sbe idx of
      Just i -> return $ RValue (arr ! fromIntegral i)
      _ -> abort "Not supported: symbolic indexing into arrays of references."
  else if tp == LongType then
    liftIO $ do
      let Just (l,rslt) = M.lookup r (arrays p)
      LValue <$> termGetLongArray sbe l rslt idx
  else do
    assert (isIValue tp)
    liftIO $ do
      let Just (l,rslt) = M.lookup r (arrays p)
      IValue <$> termGetIntArray sbe l rslt idx

-- | Returns values in an array at a given reference, passing them through the
-- given projection
getArray :: AigOps sym
         => (Value' sbe -> a)
         -> PathDescriptor
         -> Ref
         -> Simulator sbe [a]
getArray f pd ref = do
  sbe <- gets backend
  len <- getArrayLength pd ref
  case asInt sbe len of
    Nothing -> abort "Not supported: getArray called on array with symbolic length"
    Just l ->
      forM [0..l-1] $ \i -> do
        liftM f . getArrayValue pd ref =<< liftIO (termInt sbe i)

-- | Returns values in byte array at given reference.
getByteArray :: AigOps sbe =>
                PathDescriptor -> Ref -> Simulator sbe [SBETerm sym]
getByteArray pd ref = do
  a <- getIntArray pd ref
  withSBE $ \sbe -> mapM (termByteFromInt sbe) a

-- | Returns values in integer array at given reference.
getIntArray ::AigOps sym
            => PathDescriptor -> Ref -> Simulator sbe [SBETerm sym]
getIntArray = getArray unIValue

-- | Returns values in the long array at given reference.
getLongArray :: AigOps sym
             => PathDescriptor -> Ref -> Simulator sbe [SBETerm sym]
getLongArray = getArray unLValue

-- | Returns elements in reference array at given reference.
getRefArray :: AigOps sym
            => PathDescriptor -> Ref -> Simulator sbe [Ref]
getRefArray = getArray unRValue

-- Misc operations {{{1

-- | Returns type of reference and throws null pointer exception if reference is null.
getType :: AigOps sbe => Ref -> Simulator sbe Type
getType NullRef    = throwNullPtrExc
getType (Ref _ tp) = return tp

getInstanceFieldValue :: SymPath
                      -> Ref
                      -> FieldId
                      -> Simulator sbe (Value (SBETerm sym))
getInstanceFieldValue p ref fldId = do
  case M.lookup (ref, fldId) (instanceFields p) of
    Just v   -> return v
    Nothing  -> fail $ "getInstanceFieldValue: instance field " ++
                       show fldId ++ " does not exist"

getStaticFieldValue :: AigOps sym
                    => SymPath sym
                    -> FieldId
                    -> Simulator sbe (Value (SBETerm sym))
getStaticFieldValue p fldId = do
  cb <- getCodebase
  withSBE $ \sbe -> do
    cl <- lookupClass cb (fieldIdClass fldId)
    case M.lookup fldId (staticFields p) of
      Just v  -> return v
      Nothing -> do
        assert (validStaticField cl) 
        defaultValue sbe (fieldIdType fldId)
  where
    validStaticField cl =
      maybe False (\f -> fieldIsStatic f && fieldType f == fieldIdType fldId)
      $ find (\fld -> fieldName fld == fieldIdName fldId)
      $ classFields cl


  -- Control functions {{{1

  getCodebase    = gets codebase

  isFinished = do mfs <- use ctrlStk.mergeFrames
                  case mfs of
                    [ExitMergeFrame ef] -> return (ef^.efPending.to null)

  fork v trueM falseM = do
    sbe <- gets backend
    case asBool sbe v of
      Just True -> trueM
      Just False -> falseM
      Nothing -> do
        let splitPaths = do
              v' <- bNot v
              onNewPath  $ CustomRA "falseM" $ assume v' >> falseM >> return NextInst
              onCurrPath $ CustomRA "trueM"  $ assume v  >> trueM  >> return NextInst
        -- NB: We permit explicit bitblasting when the 'alwaysBitBlastBranchTerms'
        -- flag is true.  This can help addresses some symbolic termination
        -- problems, e.g. the 'recursive multiplier' problem (see the mul2 function
        -- in test/src/support/PathStateMerges.java).
        blast <- gets $ alwaysBitBlastBranchTerms . simulationFlags
        if blast then do
          mb <- liftIO $ blastTerm sbe v
          case mb of 
            Just True -> trueM
            Just False -> falseM
            Nothing -> do
              v' <- bNot v
              vSat <- liftIO $ satTerm sbe v
              v'Sat <- liftIO $ satTerm sbe v'
              case (vSat, v'Sat) of
                (False, False) -> terminateCurrentPath False >> return ()
                (False, True) -> falseM
                (True, False) -> trueM
                (True, True) -> splitPaths
        else
          splitPaths
  -- | Resolve the monadic condition to exactly one of its branches
  singleForkM condM trueM falseM = do
    sbe <- gets backend
    cond <- condM
    case asBool sbe cond of
      Just True  -> trueM
      Just False -> falseM
      _ -> error "singleForkM: Failed to resolve to single branch"

  -- Returns one result per control flow path ; if all paths yielded Java
  -- exceptions, we die and inform the user.
  getResult = do
    mf <- getMergeFrame
    rslts <- M.map (finalResult CA.&&& frames) <$> gets 
    let rs = map (second fst) (M.assocs rslts)
    when (all isRealExc $ map snd rs) $ throwExternal (msgs rs) rslts
    return rs
    where
      isRealExc Exc{} = True
      isRealExc _       = False
      -- TODO: Append the result of e.toString() or somesuch when printing an
      -- exception e
      msgs rs     =
        "All execution paths yielded exceptions.\n"
        ++ concatMap
             (\(pd, Exc r) -> ppExcMsg (length rs > 1) pd (ppSimulatorExc r))
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
          -- dbugM $ "pushCallFrame: (instance) method call to " ++ cName ++ "." ++ methodName method
          pushCallFrame cName method (RValue objectref : operands)

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
          -- dbugM $ "pushCallFrame: (static) method call to " ++ cName ++ "." ++ methodName method
          pushCallFrame cName method operands
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

  doStep = do
    done <- isPathFinished <$> getPathState
    let term = terminateCurrentPath True >> return ()
    if done
      then handleBreakpoints >> term
      else stepCommon doStep term

  die = abort
