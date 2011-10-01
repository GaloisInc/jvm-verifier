{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : atomb, jhendrix
-}

{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module MethodSpec
  -- * MethodSpec
  ( SpecJavaExpr(..)
  , SpecExpr(..)
  , SpecType(..)
  , RefChange(..)
  , MethodSpec(..)
  -- * Method overrides
  , overrideFromSpec
  -- * Verification
  , JVMDiff(..)
  , VerifyException(..)
  , printVerifyException
  , blastMethodSpec
  , redMethodSpec
  -- * Misc
  , partitions
  ) where

-- Imports {{{1

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Int
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as LV
import Text.Show

import JavaParser
import Simulation hiding (typeOf)
import Utils
import Utils.Common

import Verinf.Symbolic
import Verinf.Utils.LogMonad

-- MethodSpec {{{1

-- | An expression representing a particular JVM value.
data SpecJavaExpr = This
                  | Arg Int -- ^ Argument to method indexed from 0.
                  | InstanceField SpecJavaExpr FieldId -- ^ Value of field for given symbol.
                  | LocalVar LocalVariableIndex -- ^ Local variable indexed from 0.
  deriving (Eq, Ord, Show)

-- | Expression representing a (MonadTerm sym).
data SpecExpr sym
  = ArrayValue SpecJavaExpr
  | ScalarValue SpecJavaExpr
  | SymApp
      (Vector (MonadTerm sym) -> sym (MonadTerm sym))
      (Vector (SpecExpr sym))

instance Show (SpecExpr sym) where
  showsPrec p (ArrayValue jExpr) =
    showParen (p >= 10) $ showString "ArrayValue " . showsPrec 10 jExpr
  showsPrec p (ScalarValue jExpr) =
    showParen (p >= 10) $ showString "ScalarValue " . showsPrec 10 jExpr
  showsPrec p (SymApp _fn expr) =
    showParen (p >= 10) $ showString "SymApp XXXX " . showsPrec 10 expr

-- | A typing constraint on a specJavaExpr.
data SpecType
  = SpecRefClass !String -- ^ Specific class for a reference.
  -- | Array of constant integer values.
  | SpecIntArrayConstant !(Vector Int32)
  -- | Array of constant long values.
  | SpecLongArrayConstant !(Vector Int64)
  -- | Array of refence values with given length and element type.
  -- NB. Type should be a class type, array type.
  | SpecArray !Int32 !Type
  deriving (Eq, Ord, Show)

-- | Change to the JVM state made by he method.
data RefChange sym
  -- | Update value in array to given expression.
  = ArrayUpdate (SpecExpr sym)
  -- | Trash value in array.
  | ArrayTrash
  deriving Show

-- | Specification of JVM Method
data MethodSpec sym = MethodSpec {
  -- | Class this method is for.
    specClass :: String
  -- | Key for method this spec is for.
  , specMethodKey :: MethodKey
  -- | Classes that are assumed to be initialized before method executes.
  , specInitializedClasses :: [String]
  -- | Types of references declared in specification.
  , specRefs :: [([SpecJavaExpr], SpecType)]
  -- | Condition on arguments before method is called.
  , specPrecondition :: SpecExpr sym
  -- | Conditions that must hold at intermediate PC locations.
  , specAssertions :: [(PC, SpecExpr sym)]
  -- | Changes on references declared in specification.
  , specChanges :: [(SpecJavaExpr, RefChange sym)]
  } deriving (Show)

-- Common spec operations {{{1

-- | Returns term associated with simulator value for given reference resolution
-- function.

resolveSpecExpr :: (Monad m, Monad sym)
                => (SpecJavaExpr -> m (MonadTerm sym))        -- ^ Array resolution function
                -> (SpecJavaExpr -> m (MonadTerm sym))        -- ^ Scalar resolution function
                -> (sym (MonadTerm sym) -> m (MonadTerm sym)) -- ^ Symbolic lift function
                -> SpecExpr sym                               -- ^ Expression to resolve.
                -> m (MonadTerm sym)
resolveSpecExpr arrayTerm scalarTerm liftSym val = impl val
  where impl (ArrayValue rd)    = arrayTerm rd
        impl (ScalarValue rd)   = scalarTerm rd
        impl (SymApp fn values) = do
          terms <- V.mapM impl values
          liftSym $ fn terms

-- execSpecOverride {{{1

-- | Exceptions thrown by specification overrides.
data OverrideException (sym :: * -> *)
  = Aliasing           (MethodSpec sym) SpecJavaExpr SpecJavaExpr
  | UninitializedClass (MethodSpec sym) String
  | BadNullRef         (MethodSpec sym) SpecJavaExpr
  | BadType            (MethodSpec sym) SpecJavaExpr Type
  | BadArrayLength     (MethodSpec sym) SpecJavaExpr Int32
  | BadValue           (MethodSpec sym) SpecJavaExpr (MonadTerm sym)

deriving instance Show (MonadTerm sym) => Show (OverrideException sym)

instance Typeable1 sym => Typeable (OverrideException sym) where
  typeOf f = mkTyConApp
               (mkTyCon "MethodSpec.OverrideException")
               [ typeOf1 (q f) ]
    where
      q :: OverrideException sym -> sym a
      q = undefined

instance (Show (MonadTerm sym), Typeable1 sym) => Exception (OverrideException sym)

-- | Check that ref is non-null and has specific type.
checkType :: AigOps sym => MethodSpec sym -> SpecJavaExpr -> Ref -> Type -> Simulator sym ()
checkType spec d NullRef _type = liftIO $ throwIO $ BadNullRef spec d
checkType spec d r expectedType = do
  tp <- getType r
  liftIO $ when (tp /= expectedType) $
    throwIO $ BadType spec d tp

-- | Check that value has given ground type.
checkSpecType :: AigOps sym => MethodSpec sym -> SpecJavaExpr -> SpecType -> Ref -> Simulator sym ()
checkSpecType spec d (SpecRefClass st) r = do
  checkType spec d r (ClassType st)
checkSpecType spec d (SpecArray expectedLen expectedEltType) r = do
  checkType spec d r (ArrayType expectedEltType)
  len <- getPSS >>= (`getArrayLength` r)
  liftIO $ when (len /= expectedLen) $
    throwIO $ BadArrayLength spec d len
checkSpecType spec d (SpecIntArrayConstant v) r = do
  maybeArray <- getSymbolicArray r
  case maybeArray of
    Just (_,t) -> do
      let arrayVals = V.map (mkCInt 32 . toInteger) v
      isEq <- liftSymbolic $ do
        let eltType = SymInt (constantWidth 32)
        applyEq t =<< symbolicArrayFromV eltType arrayVals
      assume isEq
    Nothing -> do
      tp <- getType r
      liftIO $ throwIO $ BadType spec d tp
checkSpecType spec d (SpecLongArrayConstant v) r = do
  maybeArray <- getSymbolicArray r
  case maybeArray of
    Just (_,t) -> do
      let arrayVals = V.map (mkCInt 64 . toInteger) v
      isEq <- liftSymbolic $ do
        let eltType = SymInt (constantWidth 64)
        applyEq t =<< symbolicArrayFromV eltType arrayVals
      assume isEq
    Nothing -> do
      tp <- getType r
      liftIO $ throwIO $ BadType spec d tp

-- Because of the complexity of being sure that a path sticks around
-- when you need it inside the simulator, it might make more sense for
-- this to simply be a pure function that takes a PathState as a
-- parameter.
getSpecExprValue :: (MonadIO sym)
                 => Maybe Ref
                 -> Vector (Value (MonadTerm sym))
                 -> SpecJavaExpr
                 -> Simulator sym (Value (MonadTerm sym))
getSpecExprValue maybeThis _ This =
  case maybeThis of
    Nothing -> error "internal: No this object in static methods"
    Just this -> return (RValue this)
getSpecExprValue _ args (Arg i) =
  assert (0 <= i && i < V.length args) $ return (args V.! i)
getSpecExprValue maybeThis args (InstanceField d fid) = do
  r  <- getSpecExprRef maybeThis args d
  getPSS >>= \pd -> getInstanceFieldValue pd r fid
getSpecExprValue _ _ (LocalVar i) = do
  ps <- getPathState
  case frames ps of
    [] -> error "internal: LocalVar outside of stack frame"
    (f : _) -> case Map.lookup i (frmLocals f) of
                 Nothing -> error "internal: invalid local variable index"
                 Just v -> return v

getSpecExprRef :: (MonadIO sym)
               => Maybe Ref
               -> Vector (Value (MonadTerm sym))
               -> SpecJavaExpr
               -> Simulator sym Ref
getSpecExprRef maybeThis args expr = do
  t <- getSpecExprValue maybeThis args expr
  case t of
    RValue r -> return r
    _ -> error "internal: spec expression does not yield a reference"

getArrayTerm :: AigOps sym
             => MethodSpec sym
             -> Maybe Ref
             -> Vector (Value (MonadTerm sym))
             -> SpecJavaExpr
             -> Simulator sym (MonadTerm sym)
getArrayTerm spec maybeThis args expr = do
  r <- getSpecExprRef maybeThis args expr
  maybeTerm <- getSymbolicArray r
  case maybeTerm of
    Nothing -> do
      tp <- getType r
      liftIO $ throwIO $ BadType spec expr tp
    Just (_,t) -> return t

getScalarTerm :: MonadIO sym
              => Maybe Ref
              -> Vector (Value (MonadTerm sym))
              -> SpecJavaExpr
              -> Simulator sym (MonadTerm sym)
getScalarTerm maybeThis args expr = do
  v <- getSpecExprValue maybeThis args expr
  case v of
    IValue t -> return t
    LValue t -> return t
    _ -> error "internal: spec expression does not yield a term"

-- | Perform actions in a method override.
-- Throws OverrideException if preconditions fail to hold.
execSpecOverride ::
  forall sym. (AigOps sym)
  => MethodSpec sym
  -> Maybe Ref
  -> Vector (Value (MonadTerm sym))
  -> Simulator sym ()
execSpecOverride spec maybeThis args = do
  let evalSpecValue =
        resolveSpecExpr
          (getArrayTerm spec maybeThis args)
          (getScalarTerm maybeThis args)
          liftSymbolic
  -- Check initialization status
  forM_ (specInitializedClasses spec) $ \c -> do
    status <- getInitializationStatus c
    when (status /= Just Initialized) $
      liftIO $ throwIO $ UninitializedClass spec c
  -- Add precondition to assumptions.
  assume =<< evalSpecValue (specPrecondition spec)
  -- Check equivalence classes
  seenRefsRef <- liftIO $ newIORef Map.empty
  forM_ (specRefs spec) $ \(defs, groundVal) -> do
    seenRefs <- liftIO $ readIORef seenRefsRef
    newRefs <- forM defs $ \def -> do
      r <- getSpecExprRef maybeThis args def
      case Map.lookup r seenRefs of
        Nothing -> return ()
        Just prevDef -> liftIO $ throwIO $ Aliasing spec prevDef def
      checkSpecType spec def groundVal r
      return (r,def)
    liftIO $ writeIORef seenRefsRef
           $ foldr (uncurry Map.insert) seenRefs newRefs
  -- Update state
  refsAndTerms <- forM (specChanges spec) $ \(def, change) -> do
    case change of
      ArrayUpdate value -> do
        r <- getSpecExprRef maybeThis args def
        t <- evalSpecValue value
        return $ Just (r, t)
      ArrayTrash -> do
        -- TODO: Remove symbolic array value.
        return Nothing
  forM_ (catMaybes refsAndTerms) (uncurry setSymbolicArray)

-- | Override method using spec.
overrideFromSpec :: AigOps sym => MethodSpec sym -> Simulator sym ()
overrideFromSpec ms = do
  let cName = specClass ms
      key   = specMethodKey ms
  cl <- lookupClass cName
  let method = maybe (error $ "Could not find instance method " ++ show key ++ " in " ++ cName) id
             $ cl `lookupMethod` key
  if methodIsStatic method
    then overrideStaticMethod cName key $
           execSpecOverride ms Nothing . V.fromList
    else overrideInstanceMethod cName key $ \this ->
           execSpecOverride ms (Just this) . V.fromList

-- VerificationException {{{1

-- | Difference between spec and JVM states.
data JVMDiff
  -- | A value whose spec value difers from simulator value.
  = DV SpecJavaExpr -- ^ Symbol with divergent value.
       CValue -- ^ Value in spec
       CValue -- ^ Value from JVM bytecode.
  | StrongerAssumption
  deriving (Show)

-- | Exception thrown when attempting to blast method
data VerifyException
  = CounterExample (Vector [SpecJavaExpr]) -- ^ Stores expressions in each equivalence class.
                   (Map SpecJavaExpr CValue) -- ^ Maps symbolic inputs to input values.
                   [JVMDiff]
  deriving (Show, Typeable)

instance Exception VerifyException

ppJavaExpr :: SpecJavaExpr -> ShowS
ppJavaExpr This = showString "this"
ppJavaExpr (Arg i) = showString "arg[" . shows i . showChar ']'
ppJavaExpr (InstanceField expr (fieldIdName -> fname)) =
  ppJavaExpr expr . showChar '.' . showString fname
ppJavaExpr (LocalVar i) = showString "local[" . shows i . showChar ']'

ppJVMDiff :: JVMDiff -> ShowS
ppJVMDiff StrongerAssumption = showString "Stronger assumption"
ppJVMDiff (DV expr specVal jvmVal) =
  showString "  Value changes: " . ppJavaExpr expr
    . showString "\n    Spec value: " . ppCValue Mixfix specVal
    . showString "\n    JVM value:  " . ppCValue Mixfix jvmVal

-- | Print verify exception to IO.
printVerifyException :: VerifyException -> IO ()
printVerifyException (CounterExample classes inputs diffs) = do
  putStrLn "Verification failed"
  putStrLn ""
  putStrLn "Equivalence classes:"
  V.forM_ (V.enumFromN 0 (V.length classes)) $ \i -> do
    let cl = classes V.! i
    putStrLn $ showString "  " $ shows i $ showString ": " $ showListWith ppJavaExpr cl ""
  putStrLn "Input values:"
  forM_ (Map.toList inputs) $ \(expr,val) -> do
    putStrLn $ showString "  " $ ppJavaExpr expr
                               $ showString " = "
                               $ ppCValue Mixfix val ""
  putStrLn "Differences:"
  forM_ diffs $ \diff -> do
    putStrLn $ ppJVMDiff diff ""

-- Common verification code {{{1

equivClasses :: Ord k => [k] -> Int -> Int -> Map Int [k] -> [(Int, Map Int [k])]
equivClasses (h:l) clMin cnt m = do
  i <- [clMin..cnt]
  if i == cnt
    then equivClasses l clMin (cnt+1) (Map.insert cnt [h] m)
    else equivClasses l clMin cnt (Map.adjust (h:) i m)
equivClasses [] _ c m = [(c,m)]

-- | Given a list @classes@ whose elements are pairs @(elts,tp)@ where elts is a
-- list of elements that may alias, @partitions classes@ returns the list of all
-- possible equivalence classes of aliases.  Each equivalence class is
-- represented by a triple (cnt,eltMap,tpMap).
-- eltMap maps class indices to the associated set of indices.
partitions :: Ord k => [([k],tp)] -> [(Int, Map Int [k], Map Int tp)]
partitions (([],_):l) = partitions l
partitions ((h,d):l) = do
  (c,mk,md) <- partitions l
  (c',mk') <- equivClasses h c c mk
  let md' = foldr (\i -> Map.insert i d) md [c..c'-1]
  return (c', mk', md')
partitions [] = [(0,Map.empty,Map.empty)]

type CounterFn sym = (MonadTerm sym -> CValue) -> [JVMDiff]
type MCounterFn sym = (MonadTerm sym -> CValue) -> (Maybe JVMDiff)

buildPathStateEquations ::
  forall sym.
  ( TermDagMonad sym
  , WordMonad sym
  , MonadTerm sym ~ Node
  )
  => Vector [SpecJavaExpr]
  -> Vector (Maybe (MonadTerm sym))
  -> Map Int (RefChange sym)
  -> (Ref -> Maybe Int)
  -> (SpecExpr sym -> sym (MonadTerm sym))
  -> PathState (MonadTerm sym)
  -> PathState (MonadTerm sym)
  -> sym ([MonadTerm sym], [MCounterFn sym])
buildPathStateEquations classDefVec
                        classTermVec
                        classChangeMap
                        refClassFn
                        specExprValue
                        oldPathState
                        newPathState = do
    -- Check initialization
    let newInits =
          let specInits = Set.fromList $ Map.keys $ initialization oldPathState
              jvmInits  = Set.fromList $ Map.keys $ initialization newPathState
           in jvmInits `Set.difference` specInits
    when (not (Set.null newInits)) $
      error $ "Unexpected initializations " ++ show (Set.toList newInits)
    -- Check class objects
    do let newClassObjects = classObjects newPathState
       when (newClassObjects /= classObjects oldPathState) $
         error "Class objects have changed"
    -- Check static fields
    do let newStaticFields = staticFields newPathState
       when (newStaticFields /= staticFields oldPathState) $
         error "Static fields have changed"
    -- Check instance fields
    do let newInstanceFields = instanceFields newPathState
       when (newInstanceFields /= instanceFields oldPathState) $
         error "Instance fields have changed"
    -- Check ref arrays
    do when (refArrays newPathState /= refArrays oldPathState) $
         error "Reference arrays have changed"
    -- Get array equations and counterexample parse functions
    (arrayEqns, arrayCounterFns) <- fmap unzip $
      forM (Map.toList (arrays newPathState)) $ \(ref,(_,jvmTerm)) -> do
        case refClassFn ref of
          Nothing -> error $ "Unexpected symbolic array element " ++ show ref
          Just i -> do
            let counterFn :: MonadTerm sym -> MCounterFn sym
                counterFn specTerm evalFn = do
                  let d:_ = classDefVec V.! i
                      jvmVal = evalFn jvmTerm
                      specVal = evalFn specTerm
                   in if jvmVal == specVal
                        then Nothing
                        else Just $ DV d specVal jvmVal
            case Map.lookup i classChangeMap of
              Nothing -> do
                let Just specTerm = classTermVec V.! i
                res <- applyEq jvmTerm specTerm
                return (res, counterFn specTerm)
              Just (ArrayUpdate value) -> do
                specTerm <- specExprValue value
                res <- applyEq jvmTerm specTerm
                return (res, counterFn specTerm)
              Just ArrayTrash ->
                return (mkCBool True, return Nothing)
    return (arrayEqns, arrayCounterFns)

buildVC ::
  ( TermDagMonad sym
  , WordMonad sym
  , MonadTerm sym ~ Node
  )
  => MethodSpec sym
  -> (SpecExpr sym -> Simulator sym (MonadTerm sym))
  -> PathState (MonadTerm sym)
  -> PathState (MonadTerm sym)
  -> FinalResult (MonadTerm sym)
  -> [MonadTerm sym]
  -> [MCounterFn sym]
  -> Simulator sym (MonadTerm sym, CounterFn sym)
buildVC spec specExprValue oldPathState newPathState fres stateEqns stateCounterFns = do
  -- eqAassumption that newAssumptions are implied by old assumptions.
  -- Note: The converse should hold automatically.
  let newAssumption = psAssumptions newPathState
  eqAssumption <- liftSymbolic $ do
    negNew <- applyBNot newAssumption
    applyBOr negNew (psAssumptions oldPathState)
  let assumptionCounterFn evalFn =
        let CBool assumptionValue = evalFn newAssumption
         in if assumptionValue
              then Nothing
              else Just StrongerAssumption
  let counterFns = assumptionCounterFn : stateCounterFns
  -- Perform SAT check on assumptions.
  bFinalEq <- liftSymbolic $
    case fres of
     Terminated ->
       foldM applyBAnd (mkCBool True) (eqAssumption : stateEqns)
     Breakpoint _pc -> return $ mkCBool True -- TODO
     _ -> error $ "unsupported simulator result: " ++ show fres
  cond <-
    case startingPC newPathState of
      0  -> specExprValue (specPrecondition spec)
      pc -> case lookup pc (specAssertions spec) of
              Nothing -> error "invalid starting PC"
              Just a  -> specExprValue a
  negCond <- liftSymbolic $ applyBNot cond
  finalTerm <-  liftSymbolic $ applyBOr negCond bFinalEq
  return (finalTerm, \evalFn -> catMaybes (map ($ evalFn) counterFns))

-- | Run simulator monad on method spec using fixed set of reference equivalence classes.
runEquivChoice ::
  forall sym.
  ( AigOps sym
  , TermDagMonad sym
  , MonadTerm sym ~ Node
  )
  => MethodSpec sym -- ^ Spec to execute
  -- | vector containing equivalence classes of Java expressions
  -> Vector [SpecJavaExpr]
  -- | Type of each Java expression in equivalence classes.
  -> Vector SpecType
  -- | symbolic term if any for array equivalence classes.
  -> Vector (Maybe (MonadTerm sym))
  -- | Returns term that is valid if spec is valid, and function for computing
  -- differences between spec and implementation if specification is different.
  -> Simulator sym (MonadTerm sym, CounterFn sym)
runEquivChoice spec classDefVec classTypeVec classTermVec = do
  -- TODO: it may make sense for much of the following to be in a
  -- separate buildInitialState function that sets up the PathState of
  -- the initial path. Then, most of the local definitions below
  -- wouldn't need to be visible to any of the other code: it would
  -- just look values up in the state, using the SimulatorMonad
  -- interface.
  let classCount = V.length classDefVec
  let defClassMap :: Map SpecJavaExpr Int
      defClassMap = Map.fromList $
        [ (def, i) | (i,defs) <- [0..] `zip` V.toList classDefVec, def <- defs ]
  let classChangeMap :: Map Int (RefChange sym)
      classChangeMap = Map.fromList
                     $ [ (i, c)
                       | (d, c) <- specChanges spec
                       , let Just i = Map.lookup d defClassMap ]
  -- Update initialization status.
  forM_ (specInitializedClasses spec) $ \c -> do
    setInitializationStatus c Initialized
  -- Create references
  classRefVec <- V.forM (V.enumFromN 0 classCount) $ \cl -> do
    case classTypeVec V.! cl of
      SpecRefClass refClass -> do
        genRef (ClassType refClass)
      SpecIntArrayConstant v -> do
        let Just t = classTermVec V.! cl
        newSymbolicArray (ArrayType IntType) (fromIntegral $ V.length v) t
      SpecLongArrayConstant v -> do
        let Just t = classTermVec V.! cl
        newSymbolicArray (ArrayType LongType) (fromIntegral $ V.length v) t
      SpecArray l eltType
        | isIValue eltType || eltType == LongType -> do
        let Just t = classTermVec V.! cl
        newSymbolicArray (ArrayType eltType) l t
      SpecArray _ _ -> error "Unsupported lit type"
      -- Maps classes to term value.
  let classArrayTermVec :: Vector (Maybe (MonadTerm sym))
      classArrayTermVec = classTermVec
  -- Maps references in initial state to class that constructed them.
  let refClassMap = Map.fromList $ V.toList $ classRefVec `V.zip` V.enumFromN 0 classCount
      refClassFn r = Map.lookup r refClassMap
  -- Maps definition to value.
  let defClassFn :: SpecJavaExpr -> Int
      defClassFn d =
        case Map.lookup d defClassMap of
          Just i -> i
          Nothing -> error $ "Java expression " ++ show d
                             ++ " was expected to be a reference."
  let defRefFn d = classRefVec V.! defClassFn d
      resolveClassArray :: SpecJavaExpr -> MonadTerm sym
      resolveClassArray e =
        let i = defClassFn e
         in case classArrayTermVec V.! i of
              Nothing -> error $
                "Java expression " ++ show e ++ " was expected to be an array, but is not"
                ++ " an array."
              Just t -> t
  let thisRef = defRefFn This

  -- Update instance field values.
  let addInstanceDef (InstanceField d fid) i = do
        let r = defRefFn d
        setInstanceFieldValue r fid (RValue $ classRefVec V.! i)
      addInstanceDef _ _ = return ()
  mapM_ (uncurry addInstanceDef) (Map.toList defClassMap)
  -- End of initial state setup
  -- Get old path state
  oldPathState <- getPathState
  -- Run method and get final path state
  let cName = specClass spec
  let key = specMethodKey spec
  cl <- lookupClass cName
  let method = maybe (error $ "Could not find method " ++ show key ++ " in " ++ cName) id
             $ cl `lookupMethod` key
  let paramFn i = RValue (defRefFn (Arg i))
  let params = map paramFn [0 .. length (methodKeyParameterTypes key) - 1]
  let -- Maps sym value to term associated with value.
      specExprValue = resolveSpecExpr
                        (return . resolveClassArray)
                        (error "specExprValue called on non-reference")
                        id
      evalSpec = resolveSpecExpr
                        (return . resolveClassArray)
                        (error "evalSpec called on non-reference")
                        liftSymbolic
  registerBreakpoints . map ((cName, key,) . fst) . specAssertions $ spec
  if methodIsStatic method
    then invokeStaticMethod cName key params
    else invokeInstanceMethod cName key thisRef params
  ps <- getPathState
  let pc = startingPC ps
      pd = pathStSel ps
  results <- processBreakpoints (Set.singleton pc) [(pd, pc)]
  --liftIO . putStrLn $ unlines ("" : "Results:" : map show results)
  terms <- forM results $ \(pd', fres) ->
             withPathState pd' $ \newPathState -> do
    (stateEqns, stateCounterFns) <- liftSymbolic $
                                    buildPathStateEquations classDefVec
                                                            classTermVec
                                                            classChangeMap
                                                            refClassFn
                                                            specExprValue
                                                            oldPathState
                                                            newPathState
    buildVC spec evalSpec oldPathState newPathState
            fres stateEqns stateCounterFns
  return (head terms) -- TODO: combine all results

processBreakpoints ::
  (AigOps sym)
  => Set.Set PC
  -> [(PathDescriptor, PC)]
  -> Simulator sym [(PathDescriptor, FinalResult (MonadTerm sym))]
processBreakpoints _ [] = return []
processBreakpoints finishedPCs ((pd, pc) : rest) = do
  ps <- getPathStateByName pd
  ps' <- havoc ps
  unless (pc == 0) $ resumeBreakpoint ps'
  results <- run
  let assertionResumePS (pd', Breakpoint pc') = do
        if pc' `Set.member` finishedPCs
          then return Nothing
          else return $ Just (pd', pc')
      assertionResumePS _ = return Nothing
  newPaths <- catMaybes `liftM` mapM assertionResumePS results
  let finishedPCs' = finishedPCs `Set.union`
                     Set.fromList (map snd newPaths)
      nextPaths = rest ++ newPaths
  if null nextPaths
    then return results
    else processBreakpoints finishedPCs' nextPaths

havoc :: AigOps sym =>
         PathState (MonadTerm sym) -> Simulator sym (PathState (MonadTerm sym))
havoc ps = do
  frames' <- havocFrames (frames ps)
  return $ ps { frames = frames' }

havocFrames :: AigOps sym =>
               [Frame (MonadTerm sym)] -> Simulator sym [Frame (MonadTerm sym)]
havocFrames [] = return []
havocFrames (f : fs) = do
  freshLocals <- mapM (\(k, v) -> (k,) <$> freshenValue v) $
                 Map.toList $ frmLocals f
  return $ f { frmLocals = Map.fromList freshLocals } : fs

-- | Create a fresh variable of the same type as the given value.
freshenValue :: AigOps sym =>
                Value (MonadTerm sym) -> Simulator sym (Value (MonadTerm sym))
-- TODO: deal with booleans, bytes
freshenValue (IValue _) = IValue <$> liftSymbolic freshInt
freshenValue (LValue _) = LValue <$> liftSymbolic freshLong
freshenValue (RValue r) = do
  t <- getType r
  RValue <$> genRef t
-- Can't freshen floating-point or address values.
freshenValue v = return v

-- blastMethodSpec {{{1

-- | Blasts a method specification and throws VerifyException if attempt fails.
blastMethodSpec :: OpCache -> Codebase -> MethodSpec SymbolicMonad -> IO ()
blastMethodSpec oc cb spec = do
  forM_ (partitions (reverse $ specRefs spec)) $ \(c,clDefMap,clTypeMap) -> do
    let classDefVec  = V.map snd $ V.fromListN c $ Map.toAscList clDefMap
        classTypeVec = V.map snd $ V.fromListN c $ Map.toAscList clTypeMap
    defInputsRef <- liftIO $ newIORef []
    litParseFnsRef <- liftIO $ newIORef []
    runSymbolic oc $ do
      be <- getBitEngine
      -- CreateTerms
      classTermVec <- V.forM (V.enumFromN 0 c) $ \cl -> do
        case classTypeVec V.! cl of
          SpecRefClass _ -> return Nothing
          SpecIntArrayConstant v -> do
            let eltType = SymInt (constantWidth 32)
            fmap Just $ symbolicArrayFromV eltType
                      $ V.map (mkCInt 32 . toInteger) v
          SpecLongArrayConstant v -> do
            let eltType = SymInt (constantWidth 64)
            fmap Just $ symbolicArrayFromV eltType
                      $ V.map (mkCInt 64 . toInteger) v
          SpecArray len eltType -> do
            let eltSize = do
                  case eltType of
                    IntType -> 32
                    LongType -> 64
                    _ -> error "internal: Unsupported lit type"
            -- Create lits
            varLits <- liftIO $ do
              offset <- beInputLitCount be
              -- Update litParseFnRef
              litParseFns <- readIORef litParseFnsRef
              let litParseFn = \lits ->
                   let arrayVal j = mkCIntFromLsbfV $ LV.slice (offset + eltSize * j) eltSize lits
                    in CArray $ V.map arrayVal
                              $ V.enumFromN 0 (fromIntegral len)
              writeIORef litParseFnsRef (litParseFn : litParseFns)
              V.replicateM (fromIntegral len) $
                LV <$> LV.replicateM eltSize (beMakeInputLit be)
            -- Create input.
            input <- getInputCount
            -- Update defInputs
            let clDef = classDefVec V.! cl
            liftIO $ modifyIORef defInputsRef $ (++ (clDef `zip` repeat input))
            let symEltType = SymInt (constantWidth (Wx eltSize))
                arrayType = SymArray (constantWidth (fromIntegral len)) symEltType
            fmap Just $ freshVar arrayType (LVN varLits)
      defInputVec <- liftIO $ fmap (V.reverse . V.fromList) $ readIORef defInputsRef
      litParseFns <- liftIO $ fmap (V.reverse . V.fromList) $ readIORef litParseFnsRef
      -- Run simulator on given classes and terms.
      (bFinalEq,counterFn) <-
        runSimulator cb $
          runEquivChoice spec classDefVec classTypeVec classTermVec
      whenVerbosity (>=2) $
         dbugM $ "Starting checkSat"
      -- Check final result.
      LV v <- getVarLit bFinalEq
      when (LV.length v /= 1) $
        error "internal: Unexpected number of in verification condition"
      case beCheckSat be of
        Nothing -> error "Symbolic backend does not support SAT checking"
        Just checkSat -> do
          b <- liftIO $ checkSat (beNeg be (v `LV.unsafeIndex` 0))
          case b of
            UnSat -> return ()
            Unknown -> error "Checking assumptions failed"
            Sat lits -> do
              let inputValues = V.map ($lits) litParseFns
              --evalAndBlast inputValues lits
              --liftIO $ putStrLn "EvalAndBlast succeeded"
              de <- getDagEngine
              evalFn <- liftIO $ deMkEvalFn de inputValues
              let counters = counterFn evalFn
              let inputMap = Map.fromList
                           $ V.toList
                           $ V.map (\(d,i) -> (d,inputValues V.! i))
                           $ defInputVec
              assert (not (null counters)) $
                liftIO $ throwIO
                       $ CounterExample classDefVec inputMap counters

-- redMethodSpec {{{1

-- | Uses rewriting to simplify method spec and throws error if attempt fails.
redMethodSpec ::
     OpCache
  -> Codebase
  -> MethodSpec SymbolicMonad
  -> Simulator SymbolicMonad () -- | Action to run before running simulator.
  -> (MonadTerm SymbolicMonad -> SymbolicMonad ())
  -> IO ()
redMethodSpec oc cb spec initializeM reduceFn = do
  forM_ (partitions (reverse $ specRefs spec)) $ \(c,clDefMap,clTypeMap) -> do
    let classDefVec  = V.map snd $ V.fromListN c $ Map.toAscList clDefMap
        classTypeVec = V.map snd $ V.fromListN c $ Map.toAscList clTypeMap
    runSymbolic oc $ do
      -- CreateTerms
      classTermVec <- V.forM (V.enumFromN 0 c) $ \cl -> do
        case classTypeVec V.! cl of
          SpecRefClass _ -> return Nothing
          SpecIntArrayConstant v -> do
            let eltType = SymInt (constantWidth 32)
            fmap Just $ symbolicArrayFromV eltType
                      $ V.map (mkCInt 32 . toInteger) v
          SpecLongArrayConstant v -> do
            let eltType = SymInt (constantWidth 64)
            fmap Just $ symbolicArrayFromV eltType
                      $ V.map (mkCInt 64 . toInteger) v
          SpecArray len eltType -> do
            let eltSize =
                  case eltType of
                    IntType -> 32
                    LongType -> 64
                    _ -> error "internal: Unsupported lit type"
            let etp = SymInt (constantWidth eltSize)
            let arrayType = SymArray (constantWidth (fromIntegral len)) etp
            fmap Just $ freshUninterpretedVar arrayType
      -- Run simulator on given classes and terms.
      (bFinalEq,_) <-
        runSimulator cb $ do
          initializeM
          runEquivChoice spec classDefVec classTypeVec classTermVec
      reduceFn bFinalEq
