{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module SBVParser (
    isTuple
  , inferFunctionType
  , SBVException
  , ppSBVException
  , UninterpFnMap
  , WordEngine
  , mkEngineFromMonad
  , WordEvalFn(..)
  , parseSBV
  , parseSBVOp
  , inferSBVFunctionType
  ) where

-- Imports {{{1
import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable
import qualified Data.Vector as V

import SBVModel.SBV

import Verinf.Symbolic
import Verinf.Utils.CatchMIO

-- General purpose utility functions {{{1

partitionVector :: [Int] -> V.Vector a -> [V.Vector a]
partitionVector cnts = impl [] cnts 
  where impl result (n : rest) cur =
          let (hd,tl) = V.splitAt n cur
           in impl (hd : result) rest tl
        impl result [] cur 
          | V.null cur = reverse result
          | otherwise = error $ "internal: bad number of elements in partitionVector"

-- | Returns size of DagType in bits.
typeSize :: DagType -> Integer
typeSize SymBool = 1
typeSize (SymInt (widthConstant -> Just (Wx w))) = toInteger w
typeSize (SymArray (widthConstant -> Just (Wx len)) eltTp) = toInteger len * typeSize eltTp
typeSize (SymRec recDef recSubst) =
  V.sum $ V.map typeSize $ recFieldTypes recDef recSubst
typeSize _ = error "internal: typeSize called on non-ground type"

-- Exceptions thrown by SBV parser {{{1

-- | Class of exceptions thrown by SBV parser.
data SBVException
  -- | Thrown if SBV file was not a version supported by parser.
  = SBVBadFileVersion Int Int
  -- | Thrown if SBV file was valid, but contained features not supported by
  -- parser and/or symbolic backend
  | SBVUnsupportedFeature String
  -- | Thrown if SBV file did not satisfy expected format.
  | SBVBadFormat String
  -- | Thrown if SBV file contains a record that could not be identified.
  -- N.B. The argument contains a pretty printed version of the fields and types.
  | SBVUnidentifiedRecord String
  -- | Thrown if SBV file contains a function that could not be identified.
  | SBVUnidentifiedFunction String
  -- | Thrown if the record definition map or uninterpreted function map
  -- returned
  | SBVCallbackError String
 deriving (Show,Typeable)

instance Exception SBVException

ppSBVException :: SBVException -> String
ppSBVException (SBVBadFileVersion vMajor vMinor) =
  "SBV file had an unexpected version number of " ++ show vMajor ++ "." ++ show vMinor ++ "."
ppSBVException (SBVUnsupportedFeature msg) = "This SBV file could not be loaded: " ++ msg
ppSBVException (SBVBadFormat msg) = "This SBV file contained invalid contants: " ++ msg
ppSBVException (SBVUnidentifiedRecord fields) =
  "Could not identify record type " ++ fields ++ "."
ppSBVException (SBVUnidentifiedFunction name) =
   "Unexpected uninterpreted fn " ++ name
ppSBVException (SBVCallbackError msg) = msg

-- | Converts Integer to a bounded integer type and throws error message if
-- value is out of range.
downcastInt :: (Bounded a, Integral a) => Integer -> String -> a
downcastInt i name = impl minBound maxBound
  where impl :: Integral a => a -> a -> a
        impl minB maxB
          | toInteger minB <= i && i <= toInteger maxB = fromInteger i
          | otherwise
          = throw $ SBVUnsupportedFeature
                  $ "A " ++ name ++ " read in from the SBV file was out of range."

-- Types used for defining meaning to uninterpreted functions and SBV records {{{1

newtype SymbolicFn = SFN (forall m t . Monad m => WordEngine m t -> V.Vector t -> m t)

type UninterpFnMap = String -> [DagType] -> Maybe Op

-- SBVType inference {{{1

-- | Returns true if the string identifies a tuple with the given number
-- of arguments.
isTuple :: String -> Int -> Bool
isTuple op argLen
  = argLen >= 2
  && ("(" ++ replicate (argLen - 1) ',' ++ ")") == op

-- | Returns SBVType inferred from IRType if one exists.
inferType :: OpCache -> IRType -> DagType
inferType _ (TApp "." []) = SymBool
-- inferType (TApp ":" [TInt 1, TApp "." []]) = Just SBVBool
inferType _ (TApp ":" [TInt w, TApp "." []]) =
  SymInt (constantWidth (Wx (downcastInt w "integer width")))
inferType oc (TApp ":" [TInt l, irType]) = do
  SymArray (constantWidth (Wx (downcastInt l "Array size")))
           (inferType oc irType)
inferType oc (TRecord (unzip -> (names,schemes))) = SymRec def sub
  where parseScheme (Scheme [] [] [] tp) = inferType oc tp
        parseScheme _ = throw $ SBVBadFormat "Cannot parse record scheme"
        expectedTypes = map parseScheme schemes
        def = getStructuralRecord oc (Set.fromList names)
        sub = emptySubst { shapeSubst = Map.fromList $ names `zip` expectedTypes }
inferType _ irType =
  throw $ SBVBadFormat $ "Cannot parse parameter type:\n" ++ show irType

-- | symbolic terms provided by operator into list expected by SBV.
inferTypeList :: OpCache -> IRType -> V.Vector DagType
inferTypeList oc (TApp op args)
  | isTuple op (length args) = V.map (inferType oc) (V.fromList args)
inferTypeList oc arg = V.singleton $ inferType oc arg

-- Infer inputs and outputs from function type.
inferFunctionType :: OpCache -> IRType -> (V.Vector DagType, DagType)
inferFunctionType oc (TApp "->" [irTypes, irResult]) =
  (inferTypeList oc irTypes, inferType oc irResult)
inferFunctionType oc irType = (V.empty, inferType oc irType) -- Constant case

-- | Infer the SBVType of an SBVPgm as a function type.
-- Constant types are represented as nullary functions.
inferSBVFunctionType :: OpCache -> SBVPgm -> (V.Vector DagType, DagType)
inferSBVFunctionType oc (SBVPgm (_,ir,_,_,_,_)) = inferFunctionType oc ir

-- Code for splitting and joining SBVtypes {{{1

data WordEngine m t = WordEngine {
         weApplyUnary :: Op -> t -> m t
       , weApplyOp :: Op -> [t] -> m t
       , weEq :: t -> t -> m t
       , weIte :: t -> t -> t -> m t
       , weAppendInt :: t -> t -> m t
       , weGetArrayValue :: t -> t -> m t
       , weUnsignedLeq :: t -> t -> m t
       , weUnsignedLt :: t -> t -> m t
       , weBoolConstant :: Bool -> t
       , weIntConstant :: BitWidth -> Integer -> t
       , weArrayFromList :: DagType -> [t] -> m t
       , weBNot :: t -> m t
       , weINot :: t -> m t
       , weBAnd :: t -> t -> m t
       , weBOr  :: t -> t -> m t
       , weBXor :: t -> t -> m t
       , weIAnd :: t -> t -> m t
       , weIOr  :: t -> t -> m t
       , weIXor :: t -> t -> m t
       , weShl  :: t -> t -> m t
       , weUShr :: t -> t -> m t
       , weTrunc :: BitWidth -> t -> m t
       , weAdd :: t -> t -> m t
       , weSub :: t -> t -> m t
       , weMul :: t -> t -> m t
       }

mkEngineFromMonad :: WordMonad m => m (WordEngine m (MonadTerm m))
mkEngineFromMonad = return $ WordEngine {
    weApplyUnary = applyUnaryOp
  , weApplyOp = applyOp
  , weEq = applyEq
  , weIte = applyIte
  , weAppendInt = applyAppendInt
  , weGetArrayValue = applyGetArrayValue
  , weUnsignedLeq = applyUnsignedLeq
  , weUnsignedLt = applyUnsignedLt
  , weArrayFromList = symbolicArrayFromList
  , weBoolConstant = mkCBool
  , weIntConstant = mkCInt
  , weBNot = applyBNot
  , weINot = applyINot
  , weBAnd = applyBAnd
  , weBOr  = applyBOr
  , weBXor = applyBXor
  , weIAnd = applyIAnd
  , weIOr  = applyIOr
  , weIXor = applyIXor
  , weShl = applyShl
  , weUShr = applyUshr
  , weTrunc = applyTrunc
  , weAdd = applyAdd
  , weSub = applySub
  , weMul = applyMul
  }

toBool :: DagType -> (forall m t . Monad m => WordEngine m t -> t -> m t)
toBool SymBool = const return
toBool (SymInt (widthConstant -> Just 1)) = \we -> weEq we (weIntConstant we 1 1)
toBool _ = throw $ SBVBadFormat "Illegal type for Boolean input"

toInt :: DagType -> (forall m t . Monad m => WordEngine m t -> t -> m t)
toInt SymBool = \we x -> weIte we x (weIntConstant we 1 1) (weIntConstant we 1 0)
toInt SymInt{} = const return
toInt _ = throw $ SBVBadFormat "Illegal type for integer input"


newtype SplitFn = SF (forall m t . Monad m => WordEngine m t -> t -> m [t])

-- | Split a specific argument into list of arguments expected by SBV.
splitInput :: DagType -> (V.Vector DagType, SplitFn)
splitInput SymBool = (V.singleton SymBool, SF $ \_ x -> return [x])
splitInput tp@SymInt{} = (V.singleton tp, SF $ \_ x -> return [x])
splitInput (SymArray (widthConstant -> Just (Wx len)) eltTp) = do
  let (eltTypes, SF eltParser) = splitInput eltTp
   in ( V.concatMap id (V.replicate (fromIntegral len) eltTypes)
      , SF $ \we arr -> do
          res <- forM [0..toInteger len-1] $ \i -> do
            eltParser we =<< weGetArrayValue we arr (weIntConstant we 32 i)
          return (concat res))
splitInput (SymRec recDef recParams) =
  let fieldTypes = recFieldTypes recDef recParams
      fieldOpDefs = recDefFieldOps recDef
      fieldOps = V.map (\op -> mkOp op recParams) fieldOpDefs
      (fieldResTypes, fieldResFns) = V.unzip (V.map splitInput fieldTypes)
   in ( V.concatMap id fieldResTypes
      , SF $ \we t -> do
          res <- V.forM (fieldOps `V.zip` fieldResFns) $ \(op, SF splitFn) -> do
            splitFn we =<< weApplyUnary we op t
          return (concat (V.toList res)))
splitInput _ = error "internal: splitInput called on non-ground type."


-- | Strip off types with the given size.
splitTypesBySize :: Integer -> [DagType] -> ([DagType], [DagType])
splitTypesBySize initSize initTps = impl initSize initTps []
  where impl 0 tps rest = (reverse rest, tps)
        impl _ [] _ = error "internal: typesForSize given too few inputs"
        impl n (tp:tps) rest
          | n >= sz = impl (n-sz) tps (tp:rest)
          | otherwise = error "internal: typesForSize given an invalid size"
         where sz = typeSize tp

-- | Group input types together to fill result type.
groupInputTypesBySize :: [Integer] -- ^ Sizes of result
                      -> [DagType] -- ^ Types of inputs
                      -> [[DagType]]
groupInputTypesBySize = impl []
  where impl res [] [] = reverse res
        impl _ [] _ = error "internal: groupInputTypesBySize given an invalid size"
        impl res (sz:szL) inputTypes =
          let (h,inputTypes') = splitTypesBySize sz inputTypes
           in impl (h:res) szL inputTypes'

joinTypesFn :: Monad m
            => [DagType]
            -> [DagType]
            -> WordEngine m t -> V.Vector t -> m [t]
joinTypesFn resTypes sbvTypes =
  let typeSizes = map typeSize resTypes
      groupedTypes = groupInputTypesBySize typeSizes sbvTypes
      sizes = map length groupedTypes
      fieldJoinFns = zipWith joinSBVTerm resTypes groupedTypes
   in \we args -> sequence
                $ zipWith (\fn v -> fn we v)
                          fieldJoinFns
                          (partitionVector sizes args)

-- | Join split terms from SBV into single argument
-- for symbolic simulator.
joinSBVTerm :: Monad m
            => DagType -- ^ Type of result
            -> [DagType] -- ^ Type of inputs
            -> WordEngine m t -> V.Vector t -> m t
joinSBVTerm SymBool [resType] =
  let fn = toBool resType
   in \we args -> assert (V.length args == 1) $ fn we (args V.! 0)
joinSBVTerm SymInt{} (V.fromList -> exprTypes) =
  let joinFns = V.map toInt exprTypes
   in \we args -> do -- Parse arguments as integers
                     (V.toList -> h:l) <- V.mapM (\(fn,v) -> fn we v) (joinFns `V.zip` args)
                      -- Append arguments together.
                     foldM (weAppendInt we) h l
joinSBVTerm (SymArray (widthConstant -> Just (Wx len)) resEltTp) sbvTypes = do
  let fn = joinTypesFn (replicate len resEltTp) sbvTypes
   in \we -> weArrayFromList we resEltTp <=< fn we
joinSBVTerm (SymRec recDef recSubst) sbvTypes = do
  let fieldTypes = V.toList $ recFieldTypes recDef recSubst
      op = mkOp (recDefCtor recDef) recSubst
      fn = joinTypesFn fieldTypes sbvTypes
   in \we args -> weApplyOp we op =<< fn we args
joinSBVTerm _ _ = error "internal: joinSBVTerm called on invalid types."

-- SBV execution and Type checking computations plus operations {{{1
-- Core definitions {{{2

-- | State monad used for executing SBV Function.
newtype SBVExecutor = SBVE (forall m t . Monad m => WordEngine m t -> StateT (Map NodeId t) m t)

type ParseResult = (DagType, SBVExecutor)

data SBVTypeCheckerState = SBVTS {
    opCache :: OpCache
  , uninterpFnMap :: UninterpFnMap
    -- | Maps applications to parse result.
  , nodeTypeMap :: Map NodeId ParseResult
  , remInputs :: [DagType]
  , revInputNodes :: [NodeId]
  , revOutputs :: [ParseResult]
  }

type SBVTypeChecker a = State SBVTypeCheckerState a

-- | Bind node identifier to symbolic term.
bindCheck :: NodeId
          -> DagType
          -> SBVExecutor
          -> SBVTypeChecker ()
bindCheck node tp eval = do
  modify $ \s -> s { nodeTypeMap = Map.insert node (tp,eval) (nodeTypeMap s) }

-- | Run parser with given inputs.
runChecker :: OpCache
           -> UninterpFnMap
           -> [DagType]
           -> SBVTypeChecker a
           -> a
runChecker oc uFn inputs m = evalState m initialState
  where initialState = SBVTS {
            opCache = oc
          , uninterpFnMap = uFn
          , nodeTypeMap = Map.empty
          , remInputs = inputs
          , revInputNodes = []
          , revOutputs = []
          }

-- Apply code {{{2

-- | Asserts that two types are equal.
assertTypesEqual :: String -> DagType -> DagType -> a -> a
assertTypesEqual loc xtp ytp 
  | xtp == ytp = id
  | otherwise =
     throw $ SBVBadFormat
           $ "Illegal argument types " ++ ppType xtp ++ " " ++ ppType ytp
                ++ " to operator (" ++ loc ++ ")."

-- | Apply appropriate Boolean operator over Bool or integer bitvectors.
-- Note: On integers, the operation is distributed over the bits.
applyBoolOp :: (forall m t . Monad m => WordEngine m t -> t -> t -> m t)
            -> (forall m t . Monad m => WordEngine m t -> t -> t -> m t)
            -> DagType
            -> DagType
            -> (DagType, SymbolicFn)
applyBoolOp bFn _iFn SymBool SymBool =
  ( SymBool
  , SFN $ \we v -> assert (V.length v == 2)
                 $ bFn we (v V.! 0) (v V.! 1))
applyBoolOp _bFn iFn xtp@SymInt{} ytp@SymInt{} = do
  assertTypesEqual "applyBoolOp" xtp ytp $
    ( xtp
    , SFN $ \we v -> assert (V.length v == 2) $
                       iFn we (v V.! 0) (v V.! 1))
applyBoolOp _ _ _ _ = throw $ SBVBadFormat "Illegal types for Boolean operator"

-- | Apply operator over integers.
applyIntOp :: (forall m t . Monad m => WordEngine m t -> t -> t -> m t)
           -> DagType
           -> DagType
           -> (DagType, SymbolicFn)
applyIntOp termFn xTp yTp = do
  assertTypesEqual "applyIntOp" xTp yTp $
    ( xTp
    , SFN $ let xFn = toInt xTp
                yFn = toInt yTp
             in \we v -> assert (V.length v == 2) $ join $
                           return (termFn we)
                             `ap` xFn we (v V.! 0)
                             `ap` yFn we (v V.! 1))

-- | Apply operator over integers.
applyShiftOp :: (forall m t . Monad m => WordEngine m t -> t -> t -> m t)
             -> DagType
             -> DagType
             -> (DagType, SymbolicFn)
applyShiftOp termFn xTp yTp =
  ( xTp
  , SFN $ let xFn = toInt xTp
              yFn = toInt yTp
           in \we v -> assert (V.length v == 2) $ join $
                return (termFn we)
                  `ap` xFn we (v V.! 0)
                  `ap` yFn we (v V.! 1))

-- | Apply operator over integers.
applyIntRel :: (forall m t . Monad m => WordEngine m t -> t -> t -> m t)
            -> DagType
            -> DagType
            -> (DagType, SymbolicFn)
applyIntRel termFn xTp yTp = do
  assertTypesEqual "applyIntRel" xTp yTp $
    ( SymBool
    , SFN $ let xFn = toInt xTp
                yFn = toInt yTp
             in \we v -> assert (V.length v == 2) $ join $
                  return (termFn we) `ap` xFn we (v V.! 0) `ap` yFn we (v V.! 1))

-- | @ceilLgl2 i@ returns @ceil(lgl2(i))@
ceilLgl2 :: Int -> Int
ceilLgl2 val | val > 0 = impl 0 (val-1)
             | otherwise = error "Illegal value given to ceilLgl2"
  where impl :: Int -> Int -> Int
        impl j i | i == 0 = j
                 | otherwise = impl (j + 1) (i `shiftR` 1)

-- | Parse an SBV application
apply :: (OpCache, UninterpFnMap)
      -> Operator
      -> [DagType]
      -> (DagType, SymbolicFn)
apply _ BVAdd [x, y] = applyIntOp weAdd x y
apply _ BVSub [x, y] = applyIntOp weSub x y
apply _ BVMul [x, y] = applyIntOp weMul x y

-- TODO: Support below
apply _ (BVDiv _) _args = error "BVDiv unsupported"
apply _ (BVMod _) _args = error "BVMod unsupported"
apply _ BVPow _args = error "BVPow unsupported"

apply _ BVIte [cType, tType, fType] = do
  assertTypesEqual "BVIte" tType fType $
    ( tType
    , SFN $ let boolConv = toBool cType
             in \we v -> assert (V.length v == 3) $ do
                  b <- boolConv we (v V.! 0)
                  weIte we b (v V.! 1) (v V.! 2))

apply _ BVShl [x, y] = applyShiftOp weShl x y
apply _ BVShr [x, y] = applyShiftOp weUShr x y

-- TODO: Support below
apply _ BVRol _args = error "BVRol unsupported"
apply _ BVRor _args = error "BVRor unsupported"
apply _ (BVExt hi lo) [SymInt (widthConstant -> Just (Wx w))] 
  | newWidth < 0 = throw $ SBVBadFormat "Negative size given to BVExt"
  | otherwise =
      ( SymInt (constantWidth newWidth)
      , SFN $ \we args -> assert (V.length args == 1) $ do
                 -- Shift x to the right by lo bits.
                 xred <- weUShr we (args V.! 0) (weIntConstant we shiftWidth lo)
                 -- Trunc hi - lo + 1 bits off top.
                 weTrunc we newWidth xred)
 where shiftWidth = Wx (ceilLgl2 w)
       newWidth = Wx $ downcastInt (hi - lo + 1) "BVExt size"
apply _ (BVExt hi lo) [SymArray (widthConstant -> Just (Wx arrayLength)) eltType]
  | extSize /= eltSize =
    throw $ SBVBadFormat
          $ "BVExt only supported on arrays when " ++
            "extracting single array elements. Extracting " ++
            show extSize ++ " bits from array of " ++
            show arrayLength ++ " elements of type " ++ ppType eltType
  | idx < 0 || fromIntegral arrayLength <= idx =
    throw $ SBVBadFormat "BVExt array index out of range"
  | r /= 0 = throw $ SBVBadFormat "BVExt applied to unaligned array value"
  | otherwise =
      ( eltType
      , SFN $ \we v -> assert (V.length v == 1) $
                weGetArrayValue we (v V.! 0) (weIntConstant we (Wx 32) idx))
  where extSize = hi - lo + 1
        eltSize = typeSize eltType
        (idx,r) = lo `quotRem` eltSize
apply ctxt (BVExt hi lo) [(SymRec recDef recSubst)] = do
  let fieldTypes = recFieldTypes recDef recSubst
      fieldOps = recDefFieldOps recDef
  let fieldSizes = V.map typeSize fieldTypes
  let fieldEnds = V.postscanl (+) 0 fieldSizes
  case V.findIndex (\end -> end > lo) $ fieldEnds of
    Nothing ->
      throw $ SBVBadFormat
            $ "BVExt applied to illegal field index: "
                 ++ show lo ++ " " ++ show hi ++ " " ++ show fieldTypes
    Just i ->
      let fieldType = fieldTypes V.! i
          fieldSize = fieldSizes V.! i
          off = (fieldEnds V.! i) - fieldSize
          op = mkOp (fieldOps V.! i) recSubst
          extSize = hi - lo + 1
       in case () of
            _ | not (off <= hi && hi <= off + fieldSize) ->
                 throw $ SBVBadFormat $
                   "BVExt requested " ++ show extSize ++ " bits out of field with type "
                     ++ ppType fieldType ++ show off ++ show lo ++ " " ++ show hi
              | extSize == fieldSize ->
                 ( fieldType
                 , SFN $ \we v -> assert (V.length v == 1) $
                                    weApplyUnary we op (v V.! 0))
              | otherwise ->
                 let (tp, SFN extractFn) = 
                       apply ctxt (BVExt (hi - off) (lo - off)) [fieldType]
                  in (tp, SFN $ \we v -> assert (V.length v == 1) $ do
                                   fieldVal <- weApplyUnary we op (v V.! 0)
                                   extractFn we (V.singleton fieldVal))
apply _ BVAnd [x, y] = applyBoolOp weBAnd weIAnd x y
apply _ BVOr  [x, y] = applyBoolOp weBOr  weIOr  x y
apply _ BVXor [x, y] = applyBoolOp weBXor weIXor x y
apply _ BVNot [SymBool] =
  ( SymBool
  , SFN $ \we v -> assert (V.length v == 1) $ weBNot we (v V.! 0))
apply _ BVNot [tp@SymInt{}] =
  ( tp, SFN $ \we v -> assert (V.length v == 1) $ weINot we (v V.! 0))
apply _ BVEq  [xTp, yTp] =
  ( SymBool
  , case (xTp, yTp) of
      (SymBool, _) ->
        SFN (\we v -> assert (V.length v == 2) $ 
                        toBool yTp we (v V.! 1) >>= \y -> weEq we (v V.! 0) y)
      (_, SymBool) ->
        SFN (\we v -> assert (V.length v == 2) $
                        toBool xTp we (v V.! 0) >>= \x -> weEq we x (v V.! 1))
      (_, _) | xTp == yTp ->
        SFN (\we v -> assert (V.length v == 2) $ weEq we (v V.! 0) (v V.! 1))
      _ -> throw $ SBVBadFormat $
             "BVEq applied to incompatible types: " ++ show xTp ++ ", " ++ show yTp)
apply _ BVGeq [x, y] = applyIntRel (flip . weUnsignedLeq) y x
apply _ BVGt  [x, y] = applyIntRel (flip . weUnsignedLt) y x
apply _ BVLeq [x, y] = applyIntRel weUnsignedLeq x y
apply _ BVLt  [x, y] = applyIntRel weUnsignedLt x y
apply _ BVApp [xTp, yTp] =
  ( SymInt (constantWidth $ fromInteger $ typeSize xTp + typeSize yTp)
  , SFN $ let xFn = toInt xTp
              yFn = toInt yTp
           in \we v -> assert (V.length v == 2) $ do
                         x' <- xFn we (v V.! 0)
                         y' <- yFn we (v V.! 1)
                         weAppendInt we y' x') -- Order of arguments is reversed.

-- TODO: Support below
apply _ (BVLkUp _ _) _args = error "BVLkUp unsupported"

apply (oc,uFn) (BVUnint (Loc _path _line _col) [] (name,ir)) inputArgTypes = do
  -- Compute types expected for uninterpreted function
  case uFn name (V.toList fnArgTypes) of
    Nothing -> throw $ SBVUnidentifiedFunction name
    Just uOp
      | fnArgTypes /= opArgTypes uOp ->
          throw $ SBVCallbackError $
                 "Operator returned by uninterpreted function for " ++ show name
                 ++ " has different argument types than expected.\n"
                 ++ "Expected argument types:\n"
                 ++ concat (V.toList (V.map printType fnArgTypes))
                 ++ "Returned argument types:\n"
                 ++ concat (V.toList (V.map printType (opArgTypes uOp)))
      | resType /= opResultType uOp ->
          throw $ SBVCallbackError $
                 "Operator returned by uninterpreted function for " ++ show name
                 ++ " has different return type than expected.\n"
                 ++ "Expected return type:\n"
                 ++ printType resType
                 ++ "Returned return type:\n"
                 ++ printType (opResultType uOp)
      | otherwise -> 
         ( resType
         , SFN $ let joinFn = joinTypesFn (V.toList fnArgTypes) inputArgTypes
                  in \we args -> weApplyOp we uOp =<< joinFn we args)
 where (fnArgTypes,resType) = inferFunctionType oc ir
       printType tp = "  " ++ ppType tp ++ "\n"
apply _ op args =
  throw $ SBVBadFormat $
    "apply unexpected op (" ++ show op ++ ") ["
      ++ intercalate ", " (map ppType args) ++ "]"

-- checkSBV {{{2

-- | Check SBV and return type and execution engine.
checkSBV :: SBV -> SBVTypeChecker ParseResult
-- Bool constant case
checkSBV (SBV 1 (Left val)) =
  return ( SymBool
         , SBVE $ let applyFn we = return (weBoolConstant we (val /= 0))
                   in applyFn)
-- Int constant case
checkSBV (SBV w (Left val)) = do
  let w' = downcastInt w "integer width"
  return ( SymInt (constantWidth (Wx w'))
         , SBVE $ \we -> return (weIntConstant we (Wx w') val))
-- Application case
checkSBV (SBV _ (Right node)) = (Map.! node) <$> gets nodeTypeMap

-- parseSBVCommand {{{2

-- | Run SBV command though typechecker and update outputs and bindings.
parseSBVCommand :: SBVCommand -> SBVTypeChecker ()
parseSBVCommand (Decl _p (SBV _ (Right n)) Nothing) = do
  s <- get
  let tp : rest = remInputs s
  put s { remInputs = rest
        , revInputNodes = n : revInputNodes s }
  bindCheck n tp (SBVE $ \_ -> gets (Map.! n))
parseSBVCommand (Decl _p (SBV _ (Right n)) (Just (SBVApp sOp sArgs))) = do
  checkedArgs <- V.mapM checkSBV (V.fromList sArgs)
  oc <- gets opCache
  uFn <- gets uninterpFnMap
  let (tp, SFN applyFn) = apply (oc, uFn) sOp (V.toList (V.map fst checkedArgs))
  let argEvalFns = V.map snd checkedArgs
  bindCheck n tp $ SBVE $ \we -> do
    m <- get
    case Map.lookup n m of
      Just r -> return r
      Nothing -> do
        r <- (lift . applyFn we) =<< V.mapM (\(SBVE fn) -> fn we) argEvalFns
        m' <- get
        put (Map.insert n r m')
        return r
parseSBVCommand (Output sbv) = do
  res <- checkSBV sbv
  modify $ \s -> s { revOutputs = res : revOutputs s }
parseSBVCommand d = do
  throw $ SBVUnsupportedFeature $ "Unsupported sbvCommand: " ++ show d

-- parseSBV top-level declaration {{{1

-- | Returns type of SBV program.
parseSBVType :: OpCache -> SBVPgm -> (V.Vector DagType, DagType)
parseSBVType oc (SBVPgm (_,ir,_c, _v, _w, _ops)) = inferFunctionType oc ir

newtype WordEvalFn = WEF (forall m t . Monad m => WordEngine m t -> V.Vector t -> m t)

-- | Parse a SBV file into an action running in an arbitrary word monad.
parseSBV :: OpCache -- ^ Stores current operators.
         -> UninterpFnMap -- ^ Maps uninterpreted function names to corresponding op.
         -> SBVPgm
         -> WordEvalFn
parseSBV oc
         uninterpFn
         pgrm@(SBVPgm ((Version vMajor vMinor),
                       _ir,
                       cmds,
                       _vc,
                       _warnings,
                       _opDecls)) 
  | not (vMajor == 4 && vMinor == 0) = throw $ SBVBadFileVersion vMajor vMinor
  | otherwise =
      runChecker oc uninterpFn (V.toList (V.concatMap id inputTypes)) $ do
        mapM_ parseSBVCommand (reverse cmds)
        inputNodes <- reverse <$> gets revInputNodes
        (outputTypes, V.fromList -> outputEvals) <- unzip <$> reverse <$> gets revOutputs
        return $ WEF $ \we args -> do
          let res = joinSBVTerm resType outputTypes
          inputs <- V.mapM id $ V.zipWith (\(SF fn) a -> fn we a) inputFns args
          outputs <- evalStateT (V.mapM (\(SBVE fn) -> fn we) outputEvals)
                                (Map.fromList (inputNodes `zip` (concat (V.toList inputs))))
          res we outputs
 where (argTypes, resType) = parseSBVType oc pgrm
       (inputTypes, inputFns) = V.unzip $ V.map splitInput argTypes

-- | Parse a SBV file into an operator and action.
parseSBVOp :: OpCache 
              -- | Maps uninterpreted function names to corresponding op
           -> UninterpFnMap
           -> String -- ^ Name for new operator
           -> SBVPgm
           -> IO (OpDef, WordEvalFn)
parseSBVOp oc
           uninterpFn
           opDefName
           pgrm@(SBVPgm ((Version vMajor vMinor), _ir, _cmds, vc, warnings, _opDecls)) = do
  unless (vMajor == 4 && vMinor == 0) $
    throwMIO $ SBVBadFileVersion vMajor vMinor
  unless (null vc) $
    throwMIO $ SBVUnsupportedFeature
             $ "SBV Parser does not support loading SBV files with verification condition."
  unless (null warnings) $
    throwMIO $ SBVUnsupportedFeature
             $ "SBV Parser does not support loading SBV files with warnings."
  let wef@(WEF evalFn) = parseSBV oc uninterpFn pgrm
  let (argTypes,resType) = parseSBVType oc pgrm
  op <- definedOp oc opDefName (V.toList argTypes) resType $ 
          \_ args -> mkEngineFromMonad >>= flip evalFn args
  return (op, wef)
