{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module SBVParser (
    isTuple
  , inferFunctionType
  , SBVException
  , ppSBVException
  , RecordDefMap
  , UninterpFnMap
  , WordEvalFn(..)
  , parseSBV
  , parseSBVOp
  -- * SAW declarations
  , OpInfo(..)
  , inferRecordType
  , noRecords
  , fieldNameTysM
  , extendRecords
  , noUfuns
  , extendUfuns
  -- * "Internal" exports that may change
  , inferSBVFunctionType
  -- , makeSBVInput
  , splitInput
  , joinSBVTerm
  ) where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bits
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Typeable
import qualified Data.Vector as V

import SBVModel.SBV

import Verinf.Symbolic
import Verinf.Utils.Assert
import Verinf.Utils.CatchMIO

-- General purpose utility functions {{{1

assertErr :: Bool -> String -> a -> a
assertErr True  _   a = a
assertErr False msg _ = error msg

partitionLists :: [Int] -> [a] -> [[a]]
partitionLists cnts = impl [] cnts
  where impl result (n : rest) cur =
          let (hd,tl) = splitAt n cur
           in impl (hd : result) rest tl
        impl result [] [] = reverse result
        impl _res   [] _a = error $ "internal: bad number of elements in partitionLists"

-- Pretty print list of fields.
ppFields :: [(String, DagType)] -> String
ppFields fields = "{ " ++ intercalate "; " (map ppField fields) ++ " }"
  where ppField (name,tp) = name ++ " = " ++ ppType tp

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

type SymbolicFn sym = [MonadTerm sym] -> sym (MonadTerm sym)

type RecordDefMap = [(String, DagType)] -> Maybe DagType -- ^ Type map

type UninterpFnMap = String -> [DagType] -> Maybe Op

-- SBVType inference {{{1

-- | Returns true if the string identifies a tuple with the given number
-- of arguments.
isTuple :: String -> Int -> Bool
isTuple op argLen
  = argLen >= 2
  && ("(" ++ replicate (argLen - 1) ',' ++ ")") == op

-- | Returns SBVType inferred from IRType if one exists.
inferType :: RecordDefMap -> IRType -> DagType
inferType _ (TApp "." []) = SymBool
-- inferType (TApp ":" [TInt 1, TApp "." []]) = Just SBVBool
inferType _ (TApp ":" [TInt w, TApp "." []]) =
  SymInt $ constantWidth $ Wx $ downcastInt w "integer width"
inferType rdm (TApp ":" [TInt l, irType]) =
  let eltType = inferType rdm irType
   in SymArray (constantWidth (Wx (downcastInt l "Array size"))) eltType
inferType rdm (TRecord (unzip -> (names,schemes))) =
  let parseScheme (Scheme [] [] [] tp) = inferType rdm tp
      parseScheme _ = throw $ SBVBadFormat "Cannot parse record scheme"
      expectedTypes = map parseScheme schemes
      fields = names `zip` expectedTypes
      printType tp = "  " ++ ppType tp ++ "\n"
   in case rdm fields of
        Nothing -> throw (SBVUnidentifiedRecord (ppFields fields))
        Just recTp@(SymRec def sub) ->
          -- Check field types match expected types.
          let foundTypes = recFieldTypes def sub
           in if foundTypes == V.fromList expectedTypes
                then recTp
                else throw $ SBVCallbackError $
                       "Record returned by RecordDefMap has different fields than expected.\n"
                       ++ "Expected types:\n"
                       ++ concat (map printType expectedTypes)
                       ++ "Returned types:\n"
                       ++ concat (V.toList (V.map printType foundTypes))
        Just retTp ->
          throw $ SBVCallbackError $
                   "Type returned by RecordDefMap is not a record.\n"
                    ++ "Typed returned:\n"
                    ++ printType retTp
inferType _ irType =
  throw $ SBVBadFormat $ "Cannot parse parameter type:\n" ++ show irType

-- | symbolic terms provided by operator into list expected by SBV.
inferTypeList :: RecordDefMap -> IRType -> [DagType]
inferTypeList rdm (TApp op args)
  | isTuple op (length args) = map (inferType rdm) args
inferTypeList rdm arg = [inferType rdm arg]

-- Infer inputs and outputs from function type.
inferFunctionType :: RecordDefMap -> IRType -> ([DagType], DagType)
inferFunctionType rdm (TApp "->" [irTypes, irResult]) =
  (inferTypeList rdm irTypes, inferType rdm irResult)
inferFunctionType rdm irType = ([], inferType rdm irType) -- Constant case

-- | Infer the SBVType of an SBVPgm as a function type.
-- Constant types are represented as nullary functions.
inferSBVFunctionType :: RecordDefMap -> SBVPgm -> ([DagType], DagType)
inferSBVFunctionType rdm (SBVPgm (_,ir,_,_,_,_)) = inferFunctionType rdm ir

-- Code for splitting and joining SBVtypes {{{1

toBool :: WordMonad m => DagType -> MonadTerm m -> m (MonadTerm m)
toBool SymBool = return
toBool (SymInt (widthConstant -> Just 1)) = applyEq (mkCInt 1 1)
toBool _ = throw $ SBVBadFormat "Illegal type for Boolean input"

toInt :: WordMonad m => DagType -> MonadTerm m -> m (MonadTerm m)
toInt SymBool = \x -> applyIte x (mkCInt 1 1) (mkCInt 1 0)
toInt SymInt{} = return
toInt _ = throw $ SBVBadFormat "Illegal type for integer input"


-- | Split a specific argument into list of arguments expected by SBV.
splitInput :: WordMonad m
           => DagType
           -> ([DagType], MonadTerm m -> m [MonadTerm m])
splitInput SymBool = ([SymBool], \x -> return [x])
splitInput tp@SymInt{} = ([tp], \x -> return [x])
splitInput (SymArray (widthConstant -> Just (Wx len)) eltTp) = do
  let (eltTypes, eltParser) = splitInput eltTp
   in ( concat $ replicate (fromIntegral len) eltTypes
      , \arr -> do
          fmap concat $ forM [0..toInteger len-1] $ \i -> do
            eltParser =<< applyGetArrayValue arr (mkCInt 32 i))
splitInput (SymRec recDef recParams) =
  let fieldTypes = recFieldTypes recDef recParams
      fieldOps = recDefFieldOps recDef
      fieldRes = V.map splitInput fieldTypes
   in ( concat (V.toList (V.map fst fieldRes))
      , \t -> do
          let opFns = (V.map (flip mkOp recParams) fieldOps) `V.zip` (V.map snd fieldRes)
          fmap concat $ forM (V.toList opFns) $ \(op,splitFn) -> do
            splitFn =<< applyUnaryOp op t)
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

joinTypesFn :: WordMonad m
            => [DagType]
            -> [DagType]
            -> [MonadTerm m] -> m [MonadTerm m]
joinTypesFn resTypes sbvTypes =
  let typeSizes = map typeSize resTypes
      groupedTypes = groupInputTypesBySize typeSizes sbvTypes
      sizes = map length groupedTypes
      fieldJoinFns = zipWith joinSBVTerm resTypes groupedTypes
   in \args -> sequence $ zipWith ($) fieldJoinFns
                        $ partitionLists sizes args

-- | Join split terms from SBV into single argument
-- for symbolic -- simulator.
joinSBVTerm :: WordMonad m
            => DagType -- ^ Type of result
            -> [DagType] -- ^ Type of inputs
            -> [MonadTerm m] -> m (MonadTerm m)
joinSBVTerm SymBool [resType] =
  let fn = toBool resType
   in \[t] -> fn t
joinSBVTerm SymInt{} exprTypes =
  let joinFns = map toInt exprTypes
   in \args -> do -- Parse arguments as integers
                  (h:l) <- sequence $ zipWith ($) joinFns args
                  -- Append arguments together.
                  foldM applyAppendInt h l
joinSBVTerm (SymArray (widthConstant -> Just (Wx len)) resEltTp)
            sbvTypes = do
  let fn = joinTypesFn (replicate len resEltTp) sbvTypes
   in symbolicArrayFromList resEltTp <=< fn
joinSBVTerm (SymRec recDef recSubst) sbvTypes = do
  let fieldTypes = V.toList $ recFieldTypes recDef recSubst
      op = mkOp (recDefCtor recDef) recSubst
      fn = joinTypesFn fieldTypes sbvTypes
   in \args -> do
         joinedTerms <- fn args
         applyOp op joinedTerms
joinSBVTerm _ _ = error "internal: joinSBVTerm called on invalid types."

-- SBV execution and Type checking computations plus operations {{{1
-- Core definitions {{{2

-- | State monad used for executing SBV Function.
type SBVExecutor m a = StateT (Map NodeId (MonadTerm m)) m a

type ParseResult m = (DagType, SBVExecutor m (MonadTerm m))

data SBVTypeCheckerState m = SBVTS {
    recordDefMap :: RecordDefMap
  , uninterpFnMap :: UninterpFnMap
  , nodeTypeMap :: Map NodeId (ParseResult m)
  , remInputs :: [DagType]
  , revInputNodes :: [NodeId]
  , revOutputs :: [ParseResult m]
  }

type SBVTypeChecker m a = StateT (SBVTypeCheckerState m) Identity a

-- | Bind node identifier to symbolic term.
bindCheck :: WordMonad m
          => NodeId
          -> DagType
          -> SBVExecutor m (MonadTerm m)
          -> SBVTypeChecker m ()
bindCheck node tp eval = do
  modify $ \s -> s { nodeTypeMap = Map.insert node (tp,eval) (nodeTypeMap s) }

-- | Run parser with given inputs.
runChecker :: WordMonad m
           => RecordDefMap
           -> UninterpFnMap
           -> [DagType]
           -> SBVTypeChecker m a
           -> a
runChecker rFn uFn inputs m = runIdentity $ evalStateT m initialState
  where initialState = SBVTS {
            recordDefMap = rFn
          , uninterpFnMap = uFn
          , nodeTypeMap = Map.empty
          , remInputs = inputs
          , revInputNodes = []
          , revOutputs = []
          }

-- Apply code {{{2

-- | Asserts that two types are equal.
assertTypesEqual :: Monad m => String -> DagType -> DagType -> m ()
assertTypesEqual loc xtp ytp =
  unless (xtp == ytp) $
     throw $ SBVBadFormat
           $ "Illegal argument types " ++ ppType xtp ++ " " ++ ppType ytp
                ++ " to operator (" ++ loc ++ ")."

-- | Apply appropriate Boolean operator over Bool or integer bitvectors.
-- Note: On integers, the operation is distributed over the bits.
applyBoolOp :: WordMonad m
            => (MonadTerm m -> MonadTerm m -> m (MonadTerm m))
            -> (MonadTerm m -> MonadTerm m -> m (MonadTerm m))
            -> DagType
            -> DagType
            -> SBVTypeChecker m (DagType, SymbolicFn m)
applyBoolOp bTermFn _iOpFn SymBool SymBool =
  let applyFn [x, y] = bTermFn x y
      applyFn _ = error "internal: illegal arguments to applyBoolOp"
   in return (SymBool, applyFn)
applyBoolOp _bOp iTermFn xtp@(SymInt w) ytp@SymInt{} = do
  assertTypesEqual "applyBoolOp" xtp ytp
  let applyFn [x, y] = iTermFn x y
      applyFn _ = error "internal: illegal arguments to applyBoolOp"
  return (SymInt w, applyFn)
applyBoolOp _ _ _ _ = throw $ SBVBadFormat "Illegal types for Boolean operator"

-- | Apply operator over integers.
applyIntOp :: WordMonad m
           => (MonadTerm m -> MonadTerm m -> m (MonadTerm m))
           -> DagType
           -> DagType
           -> SBVTypeChecker m (DagType, SymbolicFn m)
applyIntOp termFn xTp yTp = do
  assertTypesEqual "applyIntOp" xTp yTp
  let xFn = toInt xTp
  let yFn = toInt yTp
  let applyFn [x,y] = do
        x' <- xFn x
        y' <- yFn y
        termFn x' y'
      applyFn _args = throw $ SBVBadFormat $ "integer applied to incorrect number of arguments."
  return (xTp, applyFn)

-- | Apply operator over integers.
applyShiftOp :: WordMonad m
             => (MonadTerm m -> MonadTerm m -> m (MonadTerm m))
             -> DagType
             -> DagType
             -> SBVTypeChecker m (DagType, SymbolicFn m)
applyShiftOp termFn xTp yTp = do
  let xFn = toInt xTp
  let yFn = toInt yTp
  let applyFn [x,y] = do
        x' <- xFn x
        y' <- yFn y
        termFn x' y'
      applyFn _args = throw $ SBVBadFormat $ "integer applied to incorrect number of arguments."
  return (xTp, applyFn)

-- | Apply operator over integers.
applyIntRel :: WordMonad m
            => (MonadTerm m -> MonadTerm m -> m (MonadTerm m))
            -> DagType
            -> DagType
            -> SBVTypeChecker m (DagType, SymbolicFn m)
applyIntRel termFn xTp yTp = do
  assertTypesEqual "applyIntRel" xTp yTp
  let xFn = toInt xTp
  let yFn = toInt yTp
  let applyFn [x,y] = do
        x' <- xFn x
        y' <- yFn y
        termFn x' y'
      applyFn _args = error "integer applied to incorrect number of arguments."
  return (SymBool, applyFn)

-- | @ceilLgl2 i@ returns @ceil(lgl2(i))@
ceilLgl2 :: Int -> Int
ceilLgl2 val | val > 0 = impl 0 (val-1)
             | otherwise = error "Illegal value given to ceilLgl2"
  where impl :: Int -> Int -> Int
        impl j i | i == 0 = j
                 | otherwise = impl (j + 1) (i `shiftR` 1)

-- | Parse an SBV application
apply :: WordMonad m
      => Operator
      -> [DagType]
      -> SBVTypeChecker m (DagType, SymbolicFn m)
apply BVAdd [x, y] = applyIntOp applyAdd x y
apply BVSub [x, y] = applyIntOp applySub x y
apply BVMul [x, y] = applyIntOp applyMul x y

-- TODO: Support below
apply (BVDiv _) _args = error "BVDiv unsupported"
apply (BVMod _) _args = error "BVMod unsupported"
apply BVPow _args = error "BVPow unsupported"

apply BVIte [cType, tType, fType] = do
  assertTypesEqual "BVIte" tType fType
  let boolConv = toBool cType
  let applyFn [c,t,f] = do
        b <- boolConv c
        applyIte b t f
      applyFn _ = error "internal: BVIte given illegal arguments"
  return (tType, applyFn)

apply BVShl [x, y] = applyShiftOp applyShl x y
apply BVShr [x, y] = applyShiftOp applyUshr x y

-- TODO: Support below
apply BVRol _args = error "BVRol unsupported"
apply BVRor _args = error "BVRor unsupported"

apply (BVExt hi lo) [SymInt (widthConstant -> Just (Wx w))] = do
  let shiftWidth = Wx (ceilLgl2 w)
  let newWidth = Wx $ downcastInt (hi - lo + 1) "BVExt size"
  unless (newWidth >= 0) $
    throw $ SBVBadFormat "Negative size given to BVExt"
  return (SymInt (constantWidth newWidth), \[x] -> do
    -- Shift x to the right by lo bits.
    xred <- applyUshr x (mkCInt shiftWidth lo)
    -- Trunc hi - lo + 1 bits off top.
    applyTrunc newWidth xred)
apply (BVExt hi lo) [SymArray (widthConstant -> Just (Wx arrayLength)) eltType] = do
  let extSize = hi - lo + 1
  let eltSize = typeSize eltType
  unless (extSize == eltSize) $
    throw $ SBVBadFormat
          $ "BVExt only supported on arrays when " ++
            "extracting single array elements. Extracting " ++
            show extSize ++ " bits from array of " ++
            show arrayLength ++ " elements of type " ++ ppType eltType
  let (idx,r) = lo `quotRem` eltSize
  unless (0 <= idx && idx <= fromIntegral arrayLength) $
    throw $ SBVBadFormat "BVExt array index out of range"
  unless (r == 0) $
    throw $ SBVBadFormat "BVExt applied to unaligned array value"
  let cIdx = mkCInt (Wx 32) idx
  let applyFn [arr] = applyGetArrayValue arr cIdx
      applyFn _ = error "internal: Illegal arguments to BVExt"
  return (eltType, applyFn)
apply (BVExt hi lo) [(SymRec recDef recSubst)] = do
  let fieldTypes = recFieldTypes recDef recSubst
      fieldOps = recDefFieldOps recDef
  let fieldSizes = V.map typeSize fieldTypes
  let fieldEnds = V.postscanl (+) 0 fieldSizes
  case V.findIndex (\end -> end > lo) $ fieldEnds of
    Nothing ->
      throw $ SBVBadFormat
            $ "BVExt applied to illegal field index: "
                 ++ show lo ++ " " ++ show hi ++ " " ++ show fieldTypes
    Just i -> do
      let fieldType = fieldTypes V.! i
          fieldSize = fieldSizes V.! i
      let off = (fieldEnds V.! i) - fieldSize
      let op = mkOp (fieldOps V.! i) recSubst
      let extSize = hi - lo + 1
      unless (off <= hi && hi <= off + fieldSize) $
        throw $ SBVBadFormat $
          "BVExt requested " ++ show extSize ++ " bits out of field with type "
            ++ ppType fieldType ++ show off ++ show lo ++ " " ++ show hi
      if extSize == fieldSize
        then
          return (fieldType, \[recVal] -> applyUnaryOp op recVal)
        else do
          (tp,extractFn) <- apply (BVExt (hi - off) (lo - off)) [fieldType]
          return (tp, \[recVal] -> do
                        fieldVal <- applyUnaryOp op recVal
                        extractFn [fieldVal])
apply BVAnd [x, y] = applyBoolOp applyBAnd applyIAnd x y
apply BVOr  [x, y] = applyBoolOp applyBOr  applyIOr  x y
apply BVXor [x, y] = applyBoolOp applyBXor applyIXor x y
apply BVNot [SymBool] =
  let applyFn [x] = applyBNot x
      applyFn _ = error "internal: Illegal arguments to BVNot"
   in return (SymBool, applyFn)
apply BVNot [tp@SymInt{}] =
  let applyFn [x] = applyINot x
      applyFn _ = error "internal: Illegal arguments to BVNot"
   in return (tp, applyFn)

apply BVEq  [xTp, yTp] = do
  let applyFn [x,y] = do
        case (xTp, yTp) of
          (SymBool, _) -> do
            y' <- toBool yTp y
            applyEq x y'
          (_, SymBool) -> do
            x' <- toBool xTp x
            applyEq x' y
          (_, _) | xTp == yTp -> applyEq x y
          _ -> throw $
               SBVBadFormat $ "BVEq applied to incompatible types: " ++
                 show xTp ++ ", " ++ show yTp
      applyFn _args = error "integer applied to incorrect number of arguments."
  return (SymBool, applyFn)
apply BVGeq [x, y] = applyIntRel applyUnsignedLeq y x
apply BVGt  [x, y] = applyIntRel applyUnsignedLt y x
apply BVLeq [x, y] = applyIntRel applyUnsignedLeq x y
apply BVLt  [x, y] = applyIntRel applyUnsignedLt x y
apply BVApp [xTp, yTp] = do
  let xFn = toInt xTp
  let yFn = toInt yTp
  let applyFn [x,y] = do
        x' <- xFn x
        y' <- yFn y
        applyAppendInt y' x' -- Order of arguments is reversed.
      applyFn _ = error "BVApp applied to too many arguments"
  return ( SymInt (constantWidth $ fromInteger $ typeSize xTp + typeSize yTp)
         , applyFn)

-- TODO: Support below
apply (BVLkUp _ _) _args = error "BVLkUp unsupported"

apply (BVUnint (Loc _path _line _col) [] (name,ir)) inputArgTypes = do
  uFn <- gets uninterpFnMap
  rFn <- gets recordDefMap
  -- Compute types expected for uninterpreted function
  let (fnArgTypes,resType) = inferFunctionType rFn ir
  case uFn name fnArgTypes of
    Nothing -> throw $ SBVUnidentifiedFunction name
    Just uOp -> do
      let printType tp = "  " ++ ppType tp ++ "\n"
      unless (V.fromList fnArgTypes == opArgTypes uOp) $
        throw $ SBVCallbackError $
               "Operator returned by uninterpreted function for " ++ show name
               ++ " has different argument types than expected.\n"
               ++ "Expected argument types:\n"
               ++ concat (map printType fnArgTypes)
               ++ "Returned argument types:\n"
               ++ concat (V.toList (V.map printType (opArgTypes uOp)))
      unless (resType == opResultType uOp) $
        throw $ SBVCallbackError $
               "Operator returned by uninterpreted function for " ++ show name
               ++ " has different return type than expected.\n"
               ++ "Expected return type:\n"
               ++ printType resType
               ++ "Returned return type:\n"
               ++ printType (opResultType uOp)
      let joinFn = joinTypesFn fnArgTypes inputArgTypes
      return (resType, applyOp uOp <=< joinFn)

apply op args =
  throw $ SBVBadFormat $
    "apply unexpected op (" ++ show op ++ ") ["
      ++ intercalate ", " (map ppType args) ++ "]"

-- checkSBV {{{2

-- | Check SBV and return type and execution engine.
checkSBV :: WordMonad m
         => SBV
         -> SBVTypeChecker m (ParseResult m)
-- Bool constant case
checkSBV (SBV 1 (Left val)) =
  return (SymBool, return (mkCBool (val /= 0)))
-- Int constant case
checkSBV (SBV w (Left val)) = do
  let w' = downcastInt w "integer width"
  return (SymInt (constantWidth (Wx w')), return $ mkCInt (Wx w') val)
-- Application case
checkSBV (SBV _ (Right node)) = do
  m <- gets nodeTypeMap
  return $ m Map.! node

-- parseSBVCommand {{{2

-- | Run SBV command though typechecker and update outputs and bindings.
parseSBVCommand :: WordMonad m => SBVCommand -> SBVTypeChecker m ()
parseSBVCommand (Decl _p (SBV _ (Right n)) Nothing) = do
  s <- get
  let tp : rest = remInputs s
  put s { remInputs = rest
        , revInputNodes = n : revInputNodes s }
  bindCheck n tp $ do
    m <- get
    return $ m Map.! n
parseSBVCommand (Decl _p (SBV _ (Right n)) (Just (SBVApp sOp sArgs))) = do
  checkedArgs <- mapM checkSBV sArgs
  (tp,applyFn) <- apply sOp (map fst checkedArgs)
  let argEvalFns = map snd checkedArgs
  bindCheck n tp $ do
    m <- get
    case Map.lookup n m of
      Just r -> return r
      Nothing -> do
        r <- (lift . applyFn) =<< sequence argEvalFns
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
parseSBVType :: RecordDefMap -> SBVPgm -> ([DagType], DagType)
parseSBVType recordFn (SBVPgm (_,ir,_c, _v, _w, _ops)) = inferFunctionType recordFn ir

newtype WordEvalFn = WEF (forall sym . WordMonad sym => V.Vector (MonadTerm sym) -> sym (MonadTerm sym))

-- | Parse a SBV file into an action running in an arbitrary word monad.
parseSBV :: RecordDefMap -- ^ Maps record fields to corresponding definiton
         -> UninterpFnMap -- ^ Maps uninterpreted function names to corresponding op.
         -> SBVPgm
         -> WordEvalFn
parseSBV recordFn
         uninterpFn
         pgrm@(SBVPgm ((Version vMajor vMinor),
                       _ir,
                       cmds,
                       _vc,
                       _warnings,
                       _opDecls)) = do
  if (vMajor == 4 && vMinor == 0)
    then WEF $
           let (argTypes,resType) = parseSBVType recordFn pgrm
               (inputTypes, inputFns) = V.unzip $ V.map splitInput (V.fromList argTypes)
            in runChecker recordFn uninterpFn (concat (V.toList inputTypes)) $ do
                 mapM_ parseSBVCommand (reverse cmds)
                 inputNodes <- fmap reverse $ gets revInputNodes
                 (outputTypes, outputEvals) <- fmap (unzip . reverse) $ gets revOutputs
                 let res = joinSBVTerm resType outputTypes
                 return $ \args -> do
                   inputs <- fmap (concat . V.toList)
                           $ V.mapM id
                           $ V.zipWith ($) inputFns args
                   (outputs,_) <- runStateT (sequence outputEvals) $
                                    Map.fromList (inputNodes `zip` inputs)
                   res outputs
           else throw $ SBVBadFileVersion vMajor vMinor

-- | Parse a SBV file into an operator and action.
parseSBVOp :: -- | Maps record fields to corresponding definiton
              RecordDefMap
              -- | Maps uninterpreted function names to corresponding op
           -> UninterpFnMap
           -> String -- ^ Name for new operator
           -> SBVPgm
           -> OpSession (OpDef, WordEvalFn)
parseSBVOp recordFn
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
  let wef@(WEF evalFn) = parseSBV recordFn uninterpFn pgrm
  let (argTypes,resType) = parseSBVType recordFn pgrm
  op <- definedOp opDefName argTypes resType (\_ -> evalFn)
  return (op, wef)

-- SAW declarations {{{1

-- | Information for helping construct Op values.
data OpInfo = OI { oiName :: !String, oiFormatString :: !String, oiPrec :: !OpPrec }

-- | Return or construct a RecordDef and DagType corresponding to
-- the record info passed in.
inferRecordType :: RecordDefMap
                -> OpInfo
                -> [(Name, OpInfo, DagType)]
                -> SymbolicMonad DagType
inferRecordType recordFn _ fields = do
  ftys <- fieldNameTysM fields
  liftOpSession $ do
    case recordFn ftys of
      Just recordDef -> return recordDef
      Nothing -> do
        let fieldNames = Set.fromList [nm | (nm,_,_) <- fields]
        recDef <- getStructuralRecord fieldNames
        let sub = TypeSubst {
                      widthSubst = Map.empty
                    , shapeSubst = Map.fromList [ (nm,tp) | (nm, _, tp) <- fields ]
                    }
        return $ SymRec recDef sub

-- | An empty RecordDefMap
noRecords :: RecordDefMap
noRecords _ = Nothing

fieldNameTysM :: Monad sym
              => [(String, OpInfo, DagType)]
              -> sym [(String, DagType)]
fieldNameTysM fields = return [ (name,ty) | (name,_,ty) <- fields ]

-- | Extend a RecordDefMap with a new record. Raise an error if the record
-- already exists.
extendRecords :: RecordDefMap
              -> OpInfo
              -> [(Name, OpInfo, DagType)]
              -> SymbolicMonad (DagType, RecordDefMap)
extendRecords recordFn recConstrInfo fields = do
  nameTys <- fieldNameTysM fields
  assertIO (isNothing (recordFn nameTys))
    "SBVParser.extendRecordDefMap: record already exists in the RecordDefMap."
  recordTy <- inferRecordType recordFn recConstrInfo fields
  return $ ( recordTy
           , (\fields' ->
                if fields' == nameTys
                  then Just recordTy
                  else recordFn fields')
           )

-- | An empty UninterpFnMap.
noUfuns :: UninterpFnMap
noUfuns _ _ = Nothing

-- | Extend an uninterpreted-function map with a new uninterpreted
-- function operator. Raise an error if the function already exists.
extendUfuns :: UninterpFnMap
            -> (Name,OpInfo,[DagType])
            -> DagType
            -> SymbolicMonad (OpDef, UninterpFnMap)
extendUfuns ufunMap (name,opInfo,tys) resultType = do
  assertIO (isNothing (ufunMap name tys))
    (  "SBVParser.extendUfunMap: "
    ++ "uninterpreted function name " ++ name
    ++ " is already present in the UninterpFnMap argument.")
  uopDef <- liftOpSession $
    uninterpretedOp (oiName opInfo)
                    (V.fromList tys)
                    resultType
  let Just subst = matchSubst (tys `zip` tys)
  return $ (uopDef, \ufunName ufunTys ->
    if ufunName == name
      then do assertErr (ufunTys == tys)
                (  "SBVParser.extendUfuns: "
                ++ "Expected uninterpreted function " ++ name
                ++ " to have argument types " ++ show tys
                ++ ", but parsed an occurrence with argument types "
                ++ show ufunTys ++ "."
                )
                (Just (mkOp uopDef subst))
      else ufunMap ufunName ufunTys)

{- JHx: Commented out until I better understand how this is used.
-- | Create a bitblastable input term of the given SBV type.
makeSBVInput :: DagType -> SymbolicMonad SymbolicTerm
makeSBVInput ty = do
  let tys = fst (splitInput ty)
  let joinFn = joinSBVTerm ty
   in (joinFn =<< mapM (\tp -> freshVar tp =<< liftAigMonad (makeLitvecInput tp)) tys)
  where
    makeLitvecInput :: DagType -> AigMonad LitVector
    makeLitvecInput SymBool    = fmap (LV . LV.singleton) makeInputLit
    makeLitvecInput (SymInt (Wx w)) = fmap LV $ LV.replicateM w makeInputLit
    makeLitvecInput bad_ty =
      fail $ "SBVParser.makeSBVInput: bug -- unrecognized type " ++ show bad_ty
      -}
