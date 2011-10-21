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
  , WordEvalFn(..)
  , parseSBV
  , parseSBVOp
  , inferSBVFunctionType
  ) where

-- Imports {{{1
import Control.Applicative ((<$>))
import Control.Exception
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

-- TermSemantics functions {{{1

tsIte :: Monad m => DagType -> TermSemantics m t -> t -> t -> t -> m t
tsIte tp = impl
  where op = iteOp tp
        impl ts c t f = do
          tc <- tsBoolConstant ts True
          fc <- tsBoolConstant ts False
          case () of
            _ | tsEqTerm ts c tc -> return t
              | tsEqTerm ts c fc -> return f
              | tsEqTerm ts t f -> return t
              | otherwise -> tsApplyTernary ts op c t f

-- General purpose utility functions {{{1

partitionVector :: [Int] -> V.Vector a -> V.Vector (V.Vector a)
partitionVector cnts = impl [] cnts 
  where impl result (n : rest) cur =
          let (hd,tl) = V.splitAt n cur
           in impl (hd : result) rest tl
        impl result [] cur 
          | V.null cur = V.reverse (V.fromList result)
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

newtype SymbolicFn = SFN (forall m t . Monad m 
                            => TermSemantics m t -> V.Vector t -> m t)

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

toBool :: DagType -> (forall m t . Monad m => TermSemantics m t -> t -> m t)
toBool SymBool = \_ -> return
toBool tp@(SymInt (widthConstant -> Just 1)) = \ts x -> 
  tsApplyBinary ts (eqOp tp) x =<< tsIntConstant ts 1 1
toBool _ = throw $ SBVBadFormat "Illegal type for Boolean input"

toIntType :: DagType -> DagType
toIntType SymBool = SymInt (constantWidth 1)
toIntType tp@SymInt{} = tp
toIntType _ = throw $ SBVBadFormat "Illegal type for integer input"

toInt :: DagType -> (forall m t . Monad m => TermSemantics m t -> t -> m t)
toInt SymBool = \ts x ->  do
    t1 <- tsIntConstant ts 1 1
    t0 <- tsIntConstant ts 1 0
    tsApplyTernary ts op x t1 t0
  where op = iteOp (SymInt (constantWidth 1))
toInt SymInt{} = \_ -> return
toInt _ = throw $ SBVBadFormat "Illegal type for integer input"

newtype SplitFn = SF (forall m t . Monad m => TermSemantics m t -> t -> m (V.Vector t))

-- | Split a specific argument into list of arguments expected by SBV.
splitInput :: DagType -> (V.Vector DagType, SplitFn)
splitInput SymBool = (V.singleton SymBool, SF $ \_ -> return . V.singleton)
splitInput tp@SymInt{} = (V.singleton tp, SF $ \_ -> return . V.singleton)
splitInput (SymArray lenType@(widthConstant -> Just (Wx len)) eltType) = do
  let (eltTypes, SF eltParser) = splitInput eltType
      arrayOp = getArrayValueOp lenType (constantWidth 32) eltType
   in ( V.concatMap id (V.replicate (fromIntegral len) eltTypes)
      , SF $ \ts arr -> do
               res <- V.generateM len $ \i -> do
                 c <- tsIntConstant ts 32 (toInteger i)
                 eltParser ts =<< tsApplyBinary ts arrayOp arr c
               return (V.concatMap id res))
splitInput (SymRec recDef recParams) =
  let fieldTypes = recFieldTypes recDef recParams
      fieldOps = V.map (\op -> mkOp op recParams) (recDefFieldOps recDef)
      (fieldResTypes, fieldResFns) = V.unzip (V.map splitInput fieldTypes)
   in ( V.concatMap id fieldResTypes
      , SF $ \ts t ->  do
               res <- V.zipWithM 
                        (\op (SF splitFn) -> splitFn ts =<< tsApplyUnary ts op t)
                        fieldOps
                        fieldResFns
               return (V.concatMap id res))
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
            => OpCache -> V.Vector DagType -> [DagType]
            -> TermSemantics m t -> V.Vector t -> m (V.Vector t)
joinTypesFn oc resTypes sbvTypes =
  let typeSizes = V.map typeSize resTypes
      groupedTypes = V.fromList $ groupInputTypesBySize (V.toList typeSizes) sbvTypes
      sizes = V.toList $ V.map length groupedTypes
      fieldJoinFns = V.zipWith (joinSBVTerm oc) resTypes groupedTypes
   in \ts args ->
        V.zipWithM (\fn v -> fn ts v) 
                   fieldJoinFns
                   (partitionVector sizes args)

-- | Join split terms from SBV into single argument
-- for symbolic simulator.
joinSBVTerm :: Monad m
            => OpCache
            -> DagType -- ^ Type of result
            -> [DagType] -- ^ Type of inputs
            -> (TermSemantics m t -> V.Vector t -> m t)
joinSBVTerm _ SymBool [resType] = 
  let fn = toBool resType
   in \ts args -> assert (V.length args == 1) $ fn ts (args V.! 0)
joinSBVTerm _ SymInt{} (V.fromList -> exprTypes) = 
    assert (n > 0) $ 
      let joinFns = V.map toInt exprTypes
       in \ts args -> 
         assert (V.length args == n) $
           let impl i r 
                 | i == n = return r
                 | otherwise =
                     tsApplyBinary ts (appendOps V.! (i-1)) r
                       =<< (joinFns V.! i) ts (args V.! i)
            in impl 1 =<< (joinFns V.! 0) ts (args V.! 0)
  where n = V.length exprTypes
        intSize SymBool = constantWidth 1 
        intSize (SymInt w) = w
        intSize _ = error "ilegal: joinSBVTerm given non-integer"
        -- Size of input at each point.
        exprSizes = V.map intSize exprTypes
        -- Accumulated size of result at each point.
        inputSizes = V.tail $ V.prescanl addWidth (constantWidth 0) exprSizes
        appendOps = V.zipWith appendIntOp inputSizes (V.tail exprSizes)
joinSBVTerm oc (SymArray (widthConstant -> Just (Wx len)) resEltTp) sbvTypes =
    \ts args -> tsApplyOp ts arrayOp =<< fn ts args
  where fn = joinTypesFn oc (V.replicate len resEltTp) sbvTypes
        arrayOp = mkArrayOp oc len resEltTp
joinSBVTerm oc (SymRec recDef recSubst) sbvTypes =
  let fieldTypes = recFieldTypes recDef recSubst
      op = mkOp (recDefCtor recDef) recSubst
      fn = joinTypesFn oc fieldTypes sbvTypes
   in \ts args -> tsApplyOp ts op =<< fn ts args
joinSBVTerm _ _ _ = error "internal: joinSBVTerm called on invalid types."

-- SBV execution and Type checking computations plus operations {{{1
-- Core definitions {{{2

-- | State monad used for executing SBV Function.
newtype SBVExecutor = SBVE (forall m t . Monad m
                              => TermSemantics m t
                              -> StateT (Map NodeId t) m t)

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
applyBoolOp :: Op -> (WidthExpr -> Op) -> DagType -> DagType
            -> (DagType, SymbolicFn)
applyBoolOp bOp _iOp SymBool SymBool =
  ( SymBool
  , SFN $ \ts v -> assert (V.length v == 2)
                 $ tsApplyBinary ts bOp (v V.! 0) (v V.! 1))
applyBoolOp _bOp iOp xtp@(SymInt wx)  ytp@SymInt{} = do
  assertTypesEqual "applyBoolOp" xtp ytp $
    ( xtp
    , SFN $ let op = iOp wx
             in \ts v -> assert (V.length v == 2) $
                           tsApplyBinary ts op (v V.! 0) (v V.! 1))
applyBoolOp _ _ _ _ = throw $ SBVBadFormat "Illegal types for Boolean operator"

-- | Apply operator over integers.
applyIntOp :: (WidthExpr -> Op) -> DagType -> DagType -> (DagType, SymbolicFn)
applyIntOp opFn xTp@(SymInt xw) yTp = do
  assertTypesEqual "applyIntOp" xTp yTp $
    ( xTp
    , SFN $ let op = opFn xw
                xFn = toInt xTp
                yFn = toInt yTp
             in \ts v -> assert (V.length v == 2) $ do
                           x <- xFn ts (v V.! 0)
                           y <- yFn ts (v V.! 1)
                           tsApplyBinary ts op x y)
applyIntOp  _ _ _ = error "internal: illegal type to applyIntOp"

-- | Apply operator over integers.
applyShiftOp :: (WidthExpr -> WidthExpr -> Op)
             -> DagType
             -> DagType
             -> (DagType, SymbolicFn)
applyShiftOp opFn xTp@(toIntType -> SymInt vw) yTp@(toIntType -> SymInt sw) =
  ( xTp
  , SFN $ let op = opFn vw sw
              xFn = toInt xTp
              yFn = toInt yTp
           in \ts v -> assert (V.length v == 2) $ do
                x <- xFn ts (v V.! 0)
                y <- yFn ts (v V.! 1)
                tsApplyBinary ts op x y)
applyShiftOp _ _ _ = error "internal: illegal types given to applyShiftOp"

-- | Apply operator over integers.
applyIntRel :: Bool -> (WidthExpr -> Op) -> DagType -> DagType
            -> (DagType, SymbolicFn)
applyIntRel shouldFlip opFn xTp@(toIntType -> SymInt xw) yTp = do
  assertTypesEqual "applyIntRel" (toIntType xTp) (toIntType yTp) $
    ( SymBool
    , SFN $ let op = opFn xw
                xFn = toInt xTp
                yFn = toInt yTp
             in case shouldFlip of
                  False -> \ts v -> 
                    assert (V.length v == 2) $ do
                      x <- xFn ts (v V.! 0) 
                      y <- yFn ts (v V.! 1)
                      tsApplyBinary ts op x y
                  True -> \ts v -> 
                    assert (V.length v == 2) $ do
                      x <- xFn ts (v V.! 0) 
                      y <- yFn ts (v V.! 1)
                      tsApplyBinary ts op y x)
applyIntRel _ _ _ _ = error "illegal types to applyIntRel"

-- | @ceilLgl2 i@ returns @ceil(lgl2(i))@
ceilLgl2 :: Int -> Int
ceilLgl2 val | val > 0 = impl 0 (val-1)
             | otherwise = error "Illegal value given to ceilLgl2"
  where impl :: Int -> Int -> Int
        impl j i | i == 0 = j
                 | otherwise = impl (j + 1) (i `shiftR` 1)

-- | Parse an SBV application
apply :: (OpCache, UninterpFnMap)
      -> Operator -> [DagType] -> (DagType, SymbolicFn)
apply _ BVAdd [x, y] = applyIntOp addOp x y
apply _ BVSub [x, y] = applyIntOp subOp x y
apply _ BVMul [x, y] = applyIntOp mulOp x y

-- TODO: Support below
apply _ (BVDiv _) _args = error "BVDiv unsupported"
apply _ (BVMod _) _args = error "BVMod unsupported"
apply _ BVPow _args = error "BVPow unsupported"

apply _ BVIte [cType, tType, fType] =
  assertTypesEqual "BVIte" tType fType $
    ( tType
    , SFN $ let boolConv = toBool cType
                ite = tsIte tType
             in \ts v -> assert (V.length v == 3) $ do
                  b <- boolConv ts (v V.! 0)
                  ite ts b (v V.! 1) (v V.! 2))

apply _ BVShl [x, y] = applyShiftOp shlOp x y
apply _ BVShr [x, y] = applyShiftOp ushrOp x y

-- TODO: Support below
apply _ BVRol _args = error "BVRol unsupported"
apply _ BVRor _args = error "BVRor unsupported"

apply (oc,_) (BVExt hi lo) [SymInt wx@(widthConstant -> Just (Wx w))] 
  | newWidth < 0 = throw $ SBVBadFormat "Negative size given to BVExt"
  | otherwise =
      ( SymInt (constantWidth newWidth)
      , SFN $ \ts args -> assert (V.length args == 1) $ do
                 -- Shift x to the right by lo bits.
                 loc <- tsIntConstant ts ws lo
                 xred <- tsApplyBinary ts uOp (args V.! 0) loc
                 tsApplyUnary ts trOp xred) -- Trunc hi - lo + 1 bits off top.
 where ws = Wx (ceilLgl2 w)
       newWidth = Wx $ downcastInt (hi - lo + 1) "BVExt size"
       uOp = ushrOp wx (constantWidth ws)
       trOp = truncOp oc wx newWidth

apply _ (BVExt hi lo) [SymArray lenType@(widthConstant -> Just (Wx arrayLength)) eltType]
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
      , SFN $ \ts v -> assert (V.length v == 1) $ do
                tIdx <- tsIntConstant ts (Wx 32) idx
                tsApplyBinary ts arrayOp (v V.! 0) tIdx)
  where extSize = hi - lo + 1
        eltSize = typeSize eltType
        (idx,r) = lo `quotRem` eltSize
        arrayOp = getArrayValueOp lenType (constantWidth 32) eltType
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
                 , SFN $ \ts v -> assert (V.length v == 1) $
                                    tsApplyUnary ts op (v V.! 0))
              | otherwise ->
                 let (tp, SFN extractFn) = 
                       apply ctxt (BVExt (hi - off) (lo - off)) [fieldType]
                  in (tp, SFN $ \ts v -> assert (V.length v == 1) $ do
                                   fieldVal <- tsApplyUnary ts op (v V.! 0)
                                   extractFn ts (V.singleton fieldVal))
apply _ BVAnd [x, y] = applyBoolOp bAndOp iAndOp x y
apply _ BVOr  [x, y] = applyBoolOp bOrOp  iOrOp x y
apply _ BVXor [x, y] = applyBoolOp bXorOp iXorOp x y
apply _ BVNot [SymBool] =
  ( SymBool
  , SFN $ \ts v -> assert (V.length v == 1) $ tsApplyUnary ts bNotOp (v V.! 0))
apply _ BVNot [tp@(SymInt xw)] =
  (tp, SFN $ \ts v -> assert (V.length v == 1) $
                        tsApplyUnary ts (iNotOp xw) (v V.! 0))
apply _ BVEq  [xTp, yTp] =
  ( SymBool
  , case (xTp, yTp) of
     (SymBool, _) ->
       SFN $ let yFn = toBool yTp
              in \ts v -> assert (V.length v == 2) $ do
                            y <- yFn ts (v V.! 1)
                            tsApplyBinary ts op (v V.! 0) y
      where op = eqOp SymBool
     (_, SymBool) ->
       SFN $ let xFn = toBool xTp
              in \ts v -> assert (V.length v == 2) $ do
                            x <- xFn ts (v V.! 0)
                            tsApplyBinary ts op x (v V.! 1)
      where op = eqOp SymBool
     (_, _) | xTp == yTp ->
       SFN (\ts v -> assert (V.length v == 2) $
                       tsApplyBinary ts op (v V.! 0) (v V.! 1))
      where op = eqOp xTp
     _ -> throw $ SBVBadFormat $
            "BVEq applied to incompatible types: " ++ show xTp ++ ", " ++ show yTp)
apply _ BVGeq [x, y] = applyIntRel True  unsignedLeqOp x y
apply _ BVGt  [x, y] = applyIntRel True  unsignedLtOp  x y
apply _ BVLeq [x, y] = applyIntRel False unsignedLeqOp x y
apply _ BVLt  [x, y] = applyIntRel False unsignedLtOp  x y
apply _ BVApp [xTp@(toIntType -> SymInt xw), yTp@(toIntType -> SymInt yw)] =
  ( SymInt (xw `addWidth` yw)
  , SFN $ let xFn = toInt xTp
              yFn = toInt yTp
              op = appendIntOp yw xw
           in \ts v -> assert (V.length v == 2) $ do
                         x <- xFn ts (v V.! 0)
                         y <- yFn ts (v V.! 1)
                         -- Reverse arguments in call to appendIntOp
                         tsApplyBinary ts op y x)
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
         , SFN $ let joinFn = joinTypesFn oc fnArgTypes inputArgTypes
                  in \ts args -> tsApplyOp ts uOp =<< joinFn ts args)
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
         , SBVE $ \ts -> lift $ tsBoolConstant ts (val /= 0))
-- Int constant case
checkSBV (SBV w (Left val)) = do
  let w' = downcastInt w "integer width"
  return ( SymInt (constantWidth (Wx w'))
         , SBVE $ \ts -> lift $ tsIntConstant ts (Wx w') val)
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
  bindCheck n tp $ SBVE $ \ts -> do
    m <- get
    case Map.lookup n m of
      Just r -> return r
      Nothing -> do
        args <- V.mapM (\(SBVE fn) -> fn ts) argEvalFns
        r <- lift $ applyFn ts args
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

newtype WordEvalFn = WEF (forall m t . Monad m => TermSemantics m t -> V.Vector t -> m t)

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
        (outputTypes, V.fromList -> outputEvals)
            <- unzip <$> reverse <$> gets revOutputs
        return $ WEF $ 
          let res = joinSBVTerm oc resType outputTypes
           in \ts args -> do
                 inputs <- V.zipWithM (\(SF fn) a -> fn ts a) inputFns args
                 let inputMap = Map.fromList $
                       inputNodes `zip` (V.toList (V.concatMap id inputs))
                 outputs <- evalStateT (V.mapM (\(SBVE fn) -> fn ts) outputEvals)
                                       inputMap
                 res ts outputs
 where (argTypes, resType) = parseSBVType oc pgrm
       (inputTypes, inputFns) = V.unzip $ V.map splitInput argTypes

-- | Parse a SBV file into an operator and action.
parseSBVOp :: OpCache 
           -> UninterpFnMap  -- ^ Maps uninterpreted function names to corresponding op
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
  op <- definedOp oc opDefName (V.toList argTypes) resType (OpSem (\_ -> evalFn))
  return (op, wef)
