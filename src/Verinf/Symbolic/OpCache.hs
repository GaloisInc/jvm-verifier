{- |
Module           : $Header$
Description      :
License          : BSD3
Stability        : stable
Point-of-contact : jhendrix
-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# OPTIONS_GHC -O0 #-}
module Verinf.Symbolic.OpCache (
  -- * OpCache
    OpIndex(..)
  , OpCache
  , mkOpCache
  , defineOp
  , uninterpretedOp
  , getStructuralRecord
  -- * Utilities
  , groundOp
  , mkWidthSubst
  -- * Predefined operations.
  -- ** Common operations.
  , eqOp
  , iteOp
  , truncOp 
  , signedExtOp
  , unsignedExtOp
  -- ** Boolean operations
  , bNotOpDef
  , bAndOpDef
  , bOrOpDef
  , bXorOpDef
  , bImpliesOpDef
  , bNotOp
  , bAndOp
  , bOrOp
  , bXorOp
  , bImpliesOp
  -- ** Integer bitwise operations
  , iNotOp
  , iAndOp
  , iOrOp
  , iXorOp
  -- ** Integer shift operations
  , shlOp
  , shrOp
  , ushrOp
  , appendIntOp
  -- ** Integer arithmetic operations
  , addOp
  , mulOp
  , negOp
  , subOp
  , signedDivOp
  , signedRemOp
  , unsignedDivOp
  , unsignedRemOp
  -- ** Arithmetic comparison operations
  , signedLeqOp
  , signedLtOp
  , unsignedLeqOp
  , unsignedLtOp
  -- ** Array operations.
  , getArrayValueOpDef
  , setArrayValueOpDef
  , getArrayValueOp
  , setArrayValueOp
  , splitOp
  , joinOp
  , mkArrayOp
  ) where

-- Imports {{{1

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (assert)
import Control.Monad(liftM2)
import Data.Bits
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as LV
import System.IO (fixIO)
import System.IO.Unsafe (unsafePerformIO)

import Verinf.Symbolic.Common as Op
import Verinf.Symbolic.Dag as Op
import Verinf.Symbolic.Lit.Functional

-- OpCache {{{1


data OpState = OpState {
    nextOpDefIndex :: !Int
  , opIdMap :: !(Map.Map OpIndex OpDef)
  , structuralRecordMap :: !(Map.Map (Set.Set String) SymRecDef)
  }

newtype OpCache = OC (IORef OpState)

mkOpCache :: IO OpCache
mkOpCache = OC <$>
  newIORef OpState { nextOpDefIndex = 70
                   , opIdMap  = Map.empty
                   , structuralRecordMap = Map.empty
                   }

{-# NOINLINE cachedOpDef #-}
-- | Function used for creating a cached operator.
cachedOpDef :: OpCache -> OpIndex -> (OpIndex -> OpDef) -> OpDef
cachedOpDef (OC r) opId opFn = unsafePerformIO $ do
  s <- readIORef r
  case Map.lookup opId (opIdMap s) of
    Just op -> return op
    Nothing -> do
      let opDef = opFn opId
      seq opDef $ do
        writeIORef r $! s { opIdMap = Map.insert opId opDef (opIdMap s) }
        return opDef

-- | A function used for definition a (possibly recursive function).
type OpDefinition 
  = DagEngine -- ^ Dag engine to use for creating term
    -> Op -- ^ Operator (for recursive ops).
    -> V.Vector DagTerm -- ^ Inputs for operator definition.
    -> IO DagTerm

-- | Create a defined operator with given name.
defineOp :: OpCache
         -> String
         -> V.Vector DagType
         -> DagType
         -> OpDefinition
            -- ^ Function for defining right hand side of operation.
         -> IO OpDef
defineOp (OC r) nm argTypes resType rhsFn = do
  s <- readIORef r
  let ix = nextOpDefIndex s
  writeIORef r $! s { nextOpDefIndex = ix + 1 }
  let mkOpDef rhs = OpDef { opDefIndex = Op.Dynamic ix
                          , opDefName = nm
                          , opDefPrec = defaultPrec
                          , opDefArgTypes = argTypes
                          , opDefResultType = resType
                          , opDefKind = DefinedOp rhs
                          , opDefEval = VectorOpEval concreteEval
                          , opDefLitFn = bitBlastEval
                          }
        where concreteEval sub args = evalDagTerm inputFn ts rhs
                where inputFn i _ = args V.! i
                      ts = applyTypeSubstToSemantics sub evalTermSemantics
              bitBlastEval sub args = evalDagTerm inputFn ts rhs
                where inputFn i _ = return (args V.! i)
                      ts = applyTypeSubstToSemantics sub $
                             mkBitBlastTermSemantics ?be
  rhs <- fixIO $ \rhs -> do
    let opd = mkOpDef rhs
    de <- mkExactDagEngine
    inps <- V.mapM (deFreshInput de) argTypes
    rhsFn de (mkOp opd emptySubst) inps
  return (mkOpDef rhs)

-- | Create uninterpreted operator with given name and index.
uninterpretedOp :: OpCache 
                -> String
                -> V.Vector DagType
                -> DagType
                -> IO OpDef
uninterpretedOp (OC r) opDefName opDefArgTypes opDefResultType = do
  s <- readIORef r
  let idx = nextOpDefIndex s
  writeIORef r $! s { nextOpDefIndex = idx + 1 }
  return OpDef { opDefIndex = Op.Dynamic idx
               , opDefName
               , opDefPrec = defaultPrec
               , opDefArgTypes
               , opDefResultType
               , opDefKind = UninterpOp
               , opDefEval = NoOpEval
               , opDefLitFn = error $ "Cannot bitblast uninterpreted op " ++ opDefName
               }

-- | Returns a record definition that is unique for a given set of
-- field names.  The type of each field is shape variable with the same
-- name as the field name.
getStructuralRecord :: OpCache -> Set.Set String -> SymRecDef
getStructuralRecord (OC r) names = unsafePerformIO $ do
  rm <- structuralRecordMap <$> readIORef r
  case Map.lookup names rm of
    Just def -> return def
    Nothing -> do -- Create new record
      let namesList = Set.toList names
      let namesVector = V.fromList namesList
      let ctorName = "{ " ++ intercalate ", " namesList ++ " }"
      idx <- nextOpDefIndex <$> readIORef r
      let fieldCount = V.length namesVector
      let fieldTypes = V.map SymShapeVar namesVector
      let ctorOp = OpDef { opDefIndex = Op.Dynamic idx
                          -- XXX: Could add a record constructor op...
                         , opDefName = ctorName
                         , opDefPrec = defaultPrec
                         , opDefArgTypes = fieldTypes
                         , opDefResultType = recType
                         , opDefKind = RecCtor namesList
                         , opDefEval = evalStrictPureV (CRec recDef)
                         , opDefLitFn = litStrictPureV LVN
                         }
          fieldOpFn i =
            let opDefName = namesVector V.! i
            -- XXX: Could add a record selector op...
             in OpDef { opDefIndex = Op.Dynamic (idx + fromIntegral i + 1)
                      , opDefName
                      , opDefPrec = defaultPrec
                      , opDefArgTypes = V.singleton recType
                      , opDefResultType = SymShapeVar opDefName
                      , opDefKind = PrimOp
                      , opDefEval = evalStrictPure1 isCRec $ \_ (_,_,rf) ->
                                                                    rf V.! i
                      , opDefLitFn = litStrictPure1 isLVN $ \lv -> lv V.! i
                      }
          fieldOps = V.map fieldOpFn (V.enumFromN 0 fieldCount)
          recDef   = SymRecDef { recDefCtor = ctorOp
                               , recDefFieldOps = fieldOps }
          sub = emptySubst { shapeSubst = Map.fromList
                                        $ map (\nm -> (nm, SymShapeVar nm))
                                        $ namesList }
          recType  = SymRec recDef sub
      s <- readIORef r
      writeIORef r $!
        s { nextOpDefIndex = idx + fromIntegral fieldCount + 1
          , structuralRecordMap = Map.insert names recDef (structuralRecordMap s) }
      return recDef

-- Utilities {{{1

-- | Makes operator from ground op definition.
groundOp :: OpDef -> Op
groundOp opDef = mkOp opDef emptySubst

-- | Create a binary operator with given bitwidth.
mkWidthSubst :: WidthExpr -> TypeSubst
mkWidthSubst opWidth =
  emptySubst { widthSubst = Map.fromList [("x", opWidth)] }

-- | Create a binary operator with given shape.
shapeOp :: OpDef -> DagType -> Op
shapeOp d tp = mkOp d emptySubst { shapeSubst = Map.fromList [("x", tp)] }

-- Predefined Symbolic operators {{{1
-- Precedences {{{2
{- Operator precedence:
   Lvl | Prec name  | Operators
   --------------------------------------------------------------------
    85 | arrayPrec  | getArrayValueOp, setArrayValueOp
    80 | unaryPrec  | truncOp, iNotOp, bNotOp, negOp
    75 | mulPrec    | mulOp
    70 | addPrec    | addOp, subOp
    65 | shiftPrec  | shlOp, shrOp, ushrOp
    60 | appendPrec | appendIntOp
    55 | compPrec   | signedLeqOp signedLtOp unsignedLeqOp unsignedLtOp
    50 | eqPrec     | eqOp
    45 | iAndPrec   | iAndOp
    40 | iXorPrec   | iXorOp
    35 | iOrPrec    | iOrOp
    30 | bAndPrec   | bAndOp
    25 | bXorPrec   | bXorOp
    20 | bOrPrec    | bOrOp
    15 | itePrec    | iteOp
-}

arrayPrec :: OpPrec
arrayPrec = 85

unaryPrec :: OpPrec
unaryPrec = 80

mulPrec :: OpPrec
mulPrec = 75

addPrec :: OpPrec
addPrec = 70

shiftPrec :: OpPrec
shiftPrec = 65

appendPrec :: OpPrec
appendPrec = 60

compPrec :: OpPrec
compPrec = 55

eqPrec :: OpPrec
eqPrec = 50

iAndPrec :: OpPrec
iAndPrec = 45

iXorPrec :: OpPrec
iXorPrec = 40

iOrPrec :: OpPrec
iOrPrec = 35

_bAndPrec :: OpPrec
_bAndPrec = 30

_bXorPrec :: OpPrec
_bXorPrec = 25

_bOrPrec :: OpPrec
_bOrPrec = 20

itePrec :: OpPrec
itePrec = 15

-- Common operations {{{2

-- | Returns true if two compatible-length results are equal.
eqOpDef :: OpDef
eqOpDef =
  OpDef { opDefIndex = Op.Eq
        , opDefName = "=="
        , opDefPrec = eqPrec
        , opDefArgTypes = V.replicate 2 (SymShapeVar "x")
        , opDefResultType = SymBool
        , opDefKind = PrimOp
        , opDefEval = evalStrictPure2 Just Just $ \_ -> cEq
        , opDefLitFn = litStrictPure2 Just Just $ \x y ->
             litToLitResult (lEqLitResult x y)
        }

eqOp :: DagType -> Op
eqOp = shapeOp eqOpDef

-- | @iteOp@ is a ternary argument taking the condition, true case, and false case.
iteOpDef :: OpDef
iteOpDef =
  OpDef { opDefIndex = Op.ITE
        , opDefName = "ite"
        , opDefPrec = itePrec
        , opDefArgTypes = V.fromListN 3 [SymBool, SymShapeVar "x", SymShapeVar "x"]
        , opDefResultType = SymShapeVar "x"
        , opDefKind = PrimOp
        , opDefEval = TernaryOpEval $ \_ -> eITE
        , opDefLitFn = litStrictPure3 isLV Just Just $ \c x y ->
              assert (LV.length c == 1) $
              lIteLitResult (c LV.! 0) x y
        }

iteOp :: DagType -> Op
iteOp = shapeOp iteOpDef

-- | @truncOp inputWidth resultWidth@ returns truncation unary operator
-- that takes input with given width and truncates to new width.
truncOpDef :: OpCache -> BitWidth -> OpDef
truncOpDef oc w = cachedOpDef oc (Op.Trunc w) mkTrucOpDef

mkTrucOpDef :: OpIndex -> OpDef
mkTrucOpDef ix =
    OpDef { opDefIndex = ix
          , opDefName = nm
          , opDefPrec = unaryPrec
          , opDefArgTypes = V.singleton (SymInt (varWidth "x"))
          , opDefResultType = SymInt (constantWidth (Wx wr))
          , opDefKind = PrimOp
          , opDefEval = evalStrictPure1 getUVal $ \_ -> mkCInt (Wx wr)
          , opDefLitFn = litStrictPure1 isLV $ \lx -> LV (LV.take wr lx)
          }
    where
    nm = "trunc:" ++ show wr
    Op.Trunc (Wx wr) = ix

truncOp :: OpCache -> WidthExpr -> BitWidth -> Op
truncOp oc iw rw = mkOp (truncOpDef oc rw) (mkWidthSubst iw)

-- | @signedExtOp inputWidth resultWidth@ returns unary operator that
-- returns sign extended bitvector of input with result.
signedExtOpDef :: OpCache -> BitWidth -> OpDef
signedExtOpDef oc w = cachedOpDef oc (Op.SignedExt w) mkSignedExtOpDef

mkSignedExtOpDef :: OpIndex -> OpDef
mkSignedExtOpDef ix =
    OpDef { opDefIndex = ix
          , opDefName = nm
          , opDefPrec = unaryPrec
          , opDefArgTypes = V.singleton (SymInt (varWidth "x"))
          , opDefResultType = SymInt (constantWidth (Wx wr))
          , opDefKind = PrimOp
          , opDefEval = evalStrictPure1 getSVal $ \_ -> mkCInt (Wx wr)
          , opDefLitFn = litStrictPure1 isLV $ \vx ->
              let wi = LV.length vx
                  msb = LV.last vx
              in LV $ vx LV.++ LV.replicate (wr - wi) msb
          }
  where nm = "zext:" ++ show wr
        SignedExt (Wx wr) = ix

signedExtOp :: OpCache -> WidthExpr -> BitWidth -> Op
signedExtOp oc iw rw = mkOp (signedExtOpDef oc rw) (mkWidthSubst iw)

-- | @unsignedExtOp inputWidth resultWidth@ returns unary operator that
-- returns extends bitvector of input with result.
unsignedExtOpDef :: OpCache -> BitWidth -> OpDef
unsignedExtOpDef oc w = cachedOpDef oc (Op.UnsignedExt w) mkUnsignedExtOpDef

mkUnsignedExtOpDef :: OpIndex -> OpDef
mkUnsignedExtOpDef ix =
    OpDef { opDefIndex = ix
          , opDefName = nm
          , opDefPrec = unaryPrec
          , opDefArgTypes = V.singleton (SymInt (varWidth "x"))
          , opDefResultType = SymInt (constantWidth (Wx wr))
          , opDefKind = PrimOp
          , opDefEval = evalStrictPure1 getUVal $ \_ v -> mkCInt (Wx wr) v
          , opDefLitFn = litStrictPure1 isLV $ \vx ->
             let wi = LV.length vx
              in LV $ vx LV.++ LV.replicate (wr - wi) lFalse
          }
  where nm = "uext:" ++ show wr
        UnsignedExt (Wx wr) = ix

unsignedExtOp :: OpCache -> WidthExpr -> BitWidth -> Op
unsignedExtOp oc iw rw = mkOp (unsignedExtOpDef oc rw) (mkWidthSubst iw)


-- Boolean operations {{{2

-- | Negate boolean argument.
bNotOpDef :: OpDef
bNotOpDef =
  OpDef { opDefIndex = Op.Not
        , opDefName = "not"
        , opDefPrec = unaryPrec
        , opDefArgTypes = V.singleton SymBool
        , opDefResultType = SymBool
        , opDefKind = PrimOp
        , opDefEval = evalStrictPure1 Just $ \_  -> cNot
        , opDefLitFn = litStrictPure1 isLV $ \vx -> LV (LV.map lNeg vx)
        }

bNotOp :: Op
bNotOp = groundOp bNotOpDef

-- | Create a binary operator with strict bit-blasting.
binaryBoolOpDef :: OpIndex
                -> String
                -> (LazyCValue -> LazyCValue -> LazyCValue)
                -> (forall l . (?be :: BitEngine l) => l -> l -> l)
                -> OpDef
binaryBoolOpDef opDefIndex opDefName evalFn litFn =
  OpDef { opDefIndex
        , opDefName
        , opDefPrec = defaultPrec
        , opDefArgTypes = V.replicate 2 SymBool
        , opDefResultType = SymBool
        , opDefKind = PrimOp
        , opDefEval = BinaryOpEval (\_ x y -> evalFn x y)
        , opDefLitFn = litStrictPure2 isLV isLV $ \vx vy ->
            assert (LV.length vx == 1 && LV.length vy == 1) $
              LV $ LV.singleton $ litFn (vx LV.! 0) (vy LV.! 0)
        }

eITE :: LazyCValue -> LazyCValue -> LazyCValue -> LazyCValue
eITE c t e =
  do CBool x <- c
     if x then t else e

-- | And operator applied to Boolean values.
bAndOpDef :: OpDef
bAndOpDef = binaryBoolOpDef Op.And "&&" eAnd lAnd
  where eAnd x y = eITE x y (return (CBool False))

bAndOp :: Op
bAndOp = groundOp bAndOpDef

-- | Or of two boolean arguments.
bOrOpDef :: OpDef
bOrOpDef = binaryBoolOpDef Op.Or "||" eOr lOr
  where eOr x y = eITE x (return (CBool True)) y

bOrOp :: Op
bOrOp = groundOp bOrOpDef

-- | Exclusive or of arguments.
bXorOpDef :: OpDef
bXorOpDef = binaryBoolOpDef Op.Xor "xor" eXor lXor
  where eXor mx my = liftM2 (\x y -> CBool (not (x == y))) mx my

bXorOp :: Op
bXorOp = groundOp bXorOpDef

-- | Operator definition for implication.
bImpliesOpDef :: OpDef
bImpliesOpDef = binaryBoolOpDef Op.Implies "implies" eImplies lImplies
  where eImplies x y = eITE x y (return (CBool True))

-- | Operator for implication.
bImpliesOp :: Op
bImpliesOp = groundOp bImpliesOpDef

-- Integer bitwise operations {{{2

-- | A unary operator definition with a single width type.
unaryIntOpDef :: OpIndex
              -> String
              -> OpPrec
              -> (CValue -> CValue)
              -> (forall l . (?be :: BitEngine l, LV.Storable l)
                    => LV.Vector l -> LV.Vector l)
              -> OpDef
unaryIntOpDef opDefIndex opDefName opDefPrec intFn litFn =
  let argType = SymInt (varWidth "x")
   in OpDef { opDefIndex
            , opDefName
            , opDefPrec
            , opDefArgTypes = V.singleton argType
            , opDefResultType = argType
            , opDefKind = PrimOp
            , opDefEval = evalStrictPure1 Just $ \_ -> intFn
            , opDefLitFn = litStrictPure1 isLV $ \vx -> LV (litFn vx)
            }

-- | A binary operator definition with a single width type.
binaryIntOpDef :: OpIndex
               -> String
               -> OpPrec
               -> (CValue -> CValue -> CValue)
               -> (forall l . (?be :: BitEngine l, LV.Storable l)
                     => LV.Vector l -> LV.Vector l -> LV.Vector l)
               -> OpDef
binaryIntOpDef opDefIndex opDefName opDefPrec intFn litFn =
  let argType = SymInt (varWidth "x")
   in OpDef { opDefIndex
            , opDefName
            , opDefPrec
            , opDefArgTypes = V.replicate 2 argType
            , opDefResultType = argType
            , opDefKind = PrimOp
            , opDefEval = evalStrictPure2 Just Just $ \_ -> intFn
            , opDefLitFn = litStrictPure2 isLV isLV $ \vx vy ->
                 assert (LV.length vx == LV.length vy) $
                 LV (litFn vx vy)
            }

iNotOpDef :: OpDef
iNotOpDef = unaryIntOpDef INot "~" unaryPrec cNot (LV.map lNeg)

-- | Negate bits in integer argument.
iNotOp :: WidthExpr -> Op
iNotOp w = mkOp iNotOpDef (mkWidthSubst w)

iAndOpDef :: OpDef
iAndOpDef = binaryIntOpDef IAnd "&" iAndPrec cAnd (LV.zipWith lAnd)

-- | Bitwise-and of arguments.
iAndOp :: WidthExpr -> Op
iAndOp w = mkOp iAndOpDef (mkWidthSubst w)

iOrOpDef :: OpDef
iOrOpDef = binaryIntOpDef IOr "|" iOrPrec cOr (LV.zipWith lOr)

-- | Bitwise-and of arguments.
iOrOp :: WidthExpr -> Op
iOrOp w = mkOp iOrOpDef (mkWidthSubst w)

iXorOpDef :: OpDef
iXorOpDef = binaryIntOpDef IXor "xor" iXorPrec cXor (LV.zipWith lXor)

-- | Bitwise exclusive or of arguments.
iXorOp :: WidthExpr -> Op
iXorOp w = mkOp iXorOpDef (mkWidthSubst w)

-- Integer shift operations {{{2

shiftOpDef :: OpIndex
           -> String
           -> (CValue -> Int -> Integer) -- ^ Evaluation function.
           -> (forall l . (?be :: BitEngine l, LV.Storable l) =>
                 LV.Vector l -> LV.Vector l -> LV.Vector l)
           -> OpDef
shiftOpDef opDefIndex opDefName intFn litFn =
  OpDef { opDefIndex
        , opDefName
        , opDefPrec = shiftPrec
        , opDefArgTypes = V.fromListN 2 [SymInt (varWidth "v"), SymInt (varWidth "s")]
        , opDefResultType = SymInt (varWidth "v")
        , opDefKind = PrimOp
        , opDefEval = evalStrictPure2 Just getUVal $ \sub x y ->
            let Just (Wx valueWidth) = widthConstant (widthSubst sub Map.! "v")
                val = if y < toInteger valueWidth
                              then fromInteger y
                              else valueWidth
             in mkCInt (Wx valueWidth) $ intFn x val
        , opDefLitFn = litStrictPure2 isLV isLV $ \vx vy ->
            LV (litFn vx vy)
        }

shiftOp :: OpDef -> WidthExpr -> WidthExpr -> Op
shiftOp opDef vw sw = mkOp opDef sub
  where sub = emptySubst { widthSubst = Map.fromList [ ("v", vw), ("s", sw) ] }

-- | @shlOpDef x y@ shifts @x@ to the left by @y@-bits where @y@ is treated as
-- an unsigned integer.
shlOpDef :: OpDef
shlOpDef = shiftOpDef Op.Shl "<<" evalFn lShl
  where evalFn (getSVal -> Just x) y = x `shiftL` y
        evalFn _ _ = error "internal: shlOp eval"

shlOp :: WidthExpr -> WidthExpr -> Op
shlOp =  shiftOp shlOpDef

-- | @shrOpDef x y@ shifts @x@ to the right by @y@-bits where @x@ is treated as
-- a signed integer and @y@ is treated as an unsigned integer.
shrOpDef :: OpDef
shrOpDef = shiftOpDef Op.Shr ">>" evalFn lSignedShr
  where evalFn (getSVal -> Just x) y = x `shiftR` y
        evalFn _ _ = error "internal: shrOp eval"

shrOp :: WidthExpr -> WidthExpr -> Op
shrOp =  shiftOp shrOpDef

-- | @ushrOpDef x y@ shifts @x@ to the right by @y@-bits where @x@ and @y@ are
-- treated as unsigned integers.
ushrOpDef :: OpDef
ushrOpDef = shiftOpDef Op.Ushr ">>>" evalFn lUnsignedShr
  where evalFn (getUVal -> Just x) y = x `shiftR` y
        evalFn _ _ = error "internal: ushrOp eval"

ushrOp :: WidthExpr -> WidthExpr -> Op
ushrOp =  shiftOp ushrOpDef

-- | @appendIntOp@ applied to integers @x@ & @y@ returns
-- @x .|. (y `shiftL` width x)@.
appendIntOpDef :: OpDef
appendIntOpDef =
  let xvw = varWidth "x"
      yvw = varWidth "y"
   in OpDef { opDefIndex = Op.AppendInt
            , opDefName = "#"
            , opDefPrec = appendPrec
            , opDefArgTypes = V.fromListN 2 [SymInt xvw, SymInt yvw]
            , opDefResultType = SymInt (addWidth xvw yvw)
            , opDefKind = PrimOp
            , opDefEval = evalStrictPure2 getUVal getUVal $ \sub xv yv ->
                let Just (Wx xw) = widthConstant (widthSubst sub Map.! "x")
                    Just (Wx yw) = widthConstant (widthSubst sub Map.! "y")
                 in mkCInt (Wx $ xw + yw) $ xv .|. (yv `shiftL` xw)

            , opDefLitFn = litStrictPure2 isLV isLV $ \vx vy ->
                LV (vx LV.++ vy)
            }

appendIntOp :: WidthExpr -> WidthExpr -> Op
appendIntOp wx wy =
  mkOp appendIntOpDef emptySubst { widthSubst = Map.fromList [("x", wx), ("y", wy)] }

-- Integer arithmetic operations {{{2

-- | Return twos-complement negation of integer argument.
negOpDef :: OpDef
negOpDef = unaryIntOpDef Op.Neg "unary-" unaryPrec cNegate lNegate

negOp :: WidthExpr -> Op
negOp w = mkOp negOpDef (mkWidthSubst w)

-- | Add arguments.
addOpDef :: OpDef
addOpDef = binaryIntOpDef Op.Add "+" addPrec cAdd (\x y -> snd (lFullAdd x y))

addOp :: WidthExpr -> Op
addOp w = mkOp addOpDef (mkWidthSubst w)

-- | Subtract arguments.
subOpDef :: OpDef
subOpDef = binaryIntOpDef Op.Sub "-" addPrec cSub (\x y -> snd (lFullSub x y))

subOp :: WidthExpr -> Op
subOp w = mkOp subOpDef (mkWidthSubst w)

-- | Multiply two @n@-bit arguments to obtain a @2n@-bit argument.
mulOpDef :: OpDef
mulOpDef = binaryIntOpDef Op.Mul "*" mulPrec cMul lMul

mulOp :: WidthExpr -> Op
mulOp w = mkOp mulOpDef (mkWidthSubst w)

signedDivOpDef :: OpDef
signedDivOpDef = binaryIntOpDef Op.SignedDiv "div" mulPrec cSignedQuot lQuot 

-- | Performs signed integer division.
signedDivOp :: WidthExpr -> Op
signedDivOp w = mkOp signedDivOpDef (mkWidthSubst w)

signedRemOpDef :: OpDef
signedRemOpDef = binaryIntOpDef Op.SignedRem "rem" mulPrec cSignedRem lRem

-- | Performs signed integer remainder.
signedRemOp :: WidthExpr -> Op
signedRemOp w = mkOp signedRemOpDef (mkWidthSubst w)

-- | Performs unsigned integer division
unsignedDivOpDef :: OpDef
unsignedDivOpDef = binaryIntOpDef Op.UnsignedDiv "udiv" mulPrec cUnsignedQuot lUnsignedQuot

-- | Performs unsigned integer division
unsignedDivOp :: WidthExpr -> Op
unsignedDivOp w = mkOp unsignedDivOpDef (mkWidthSubst w)

unsignedRemOpDef :: OpDef
unsignedRemOpDef = binaryIntOpDef Op.UnsignedRem "urem" mulPrec cUnsignedRem lUnsignedRem

-- | Performs unsigned integer remainder.
unsignedRemOp :: WidthExpr -> Op
unsignedRemOp w = mkOp unsignedRemOpDef (mkWidthSubst w)

-- Integer comparison relations {{{2

-- | Create a binary operator with strict bit-blasting.
intRelDef :: OpIndex
          -> String
          -> (CValue -> CValue -> CValue)
          -> (forall l . (?be :: BitEngine l, LV.Storable l)
                 => LV.Vector l -> LV.Vector l -> l)
          -> OpDef
intRelDef opDefIndex opDefName evalFn litFn =
  OpDef { opDefIndex
        , opDefName
        , opDefPrec = compPrec
        , opDefArgTypes = V.replicate 2 (SymInt (varWidth "x"))
        , opDefResultType = SymBool
        , opDefKind = PrimOp
        , opDefEval = evalStrictPure2 Just Just $ \_ -> evalFn
        , opDefLitFn = litStrictPure2 isLV isLV $ \vx vy ->
              assert (LV.length vx == LV.length vy) $
                LV $ LV.singleton $ litFn vx vy
        }

-- | Signed less than or equal comparison.
signedLeqOpDef :: OpDef
signedLeqOpDef = intRelDef Op.SignedLeq "<=s" cSignedLeq lSignedLeq

signedLeqOp :: WidthExpr -> Op
signedLeqOp w = mkOp signedLeqOpDef (mkWidthSubst w)

-- | Signed less than comparison.
signedLtOpDef :: OpDef
signedLtOpDef = intRelDef Op.SignedLt "<s" cSignedLt lSignedLt

signedLtOp :: WidthExpr -> Op
signedLtOp w = mkOp signedLtOpDef (mkWidthSubst w)

-- | Unsigned less than or equal comparison.
unsignedLeqOpDef :: OpDef
unsignedLeqOpDef = intRelDef Op.UnsignedLeq "<=u" cUnsignedLeq lUnsignedLeq

unsignedLeqOp :: WidthExpr -> Op
unsignedLeqOp w = mkOp unsignedLeqOpDef (mkWidthSubst w)

-- | Unsigned less than comparison.
unsignedLtOpDef :: OpDef
unsignedLtOpDef = intRelDef Op.UnsignedLt "<u" cUnsignedLt lUnsignedLt

unsignedLtOp :: WidthExpr -> Op
unsignedLtOp w = mkOp unsignedLtOpDef (mkWidthSubst w)

-- Array operations {{{2

-- | Returns value if litvector equals constant.
lEqConstant :: (?be :: BitEngine l, LV.Storable l)
            => LV.Vector l -> Int -> l
lEqConstant v i =
  LV.foldl' (\r j -> let l = v LV.! j 
                      in r `lAnd` (if i `testBit` j then l else lNeg l))
            lTrue
            (LV.enumFromN 0 (LV.length v))

-- | ternary operation that returns an after updating a value at a given location.
--   First argument: A zero-based array indexed by 32-bit integers.
--   Second argument: A 32-bit wide integer used as the index.
--   Third argument: Value to store in array
setArrayValueOpDef :: OpDef
setArrayValueOpDef =
    let arrayType = SymArray (varWidth "l") eltType
        idxType = SymInt (varWidth "i")
        eltType = SymShapeVar "e"
     in OpDef { opDefIndex = Op.SetArrayValue
              , opDefName = "set"
              , opDefPrec = arrayPrec
              , opDefArgTypes = V.fromListN 3 [arrayType, idxType, eltType]
              , opDefResultType = arrayType
              , opDefKind = PrimOp
              , opDefEval = evalStrictPure3 Just Just Just $ \_ ->cSetArrayValue
              , opDefLitFn = litStrictPure3 isLVN isLV Just $ \va vi vv ->
                  LVN $ V.generate (V.length va) $ \i ->
                       lIteLitResult (lEqConstant vi i) vv (va V.! i)
              }

setArrayValueOp :: WidthExpr -> WidthExpr -> DagType -> Op
setArrayValueOp len idxType eltType =
  mkOp setArrayValueOpDef
       TypeSubst { shapeSubst = Map.fromList [("e", eltType)]
                 , widthSubst = Map.fromList [("l", len), ("i", idxType)]
                 }

-- | binary operation that returns the value of at a given index in an array.
--   First argument: A zero-based array indexed by signed 32-bit integers.
--   Second argument: A signed 32-bit integer used as the index.
getArrayValueOpDef :: OpDef
getArrayValueOpDef =
  OpDef { opDefIndex = Op.GetArrayValue
        , opDefName = "get"
        , opDefPrec = arrayPrec
        , opDefArgTypes = V.fromListN 2
            [ SymArray (varWidth "l") (SymShapeVar "e")
            , SymInt (varWidth "i") ]
        , opDefResultType = SymShapeVar "e"
        , opDefKind = PrimOp
        , opDefEval = evalStrictPure2 Just Just $ \_ -> cGetArrayValue
        , opDefLitFn = litStrictPure2 isLVN isLV $ \va vi ->
             let len = V.length va
              in case len of 
                   0 -> error "internal: Current implementation does not allow bitblasting a 0 length vector"
                   _ -> lMuxInteger lIteLitResult (len-1) vi (va V.!)
        }

getArrayValueOp :: WidthExpr -> WidthExpr -> DagType -> Op
getArrayValueOp len idxType eltType =
  mkOp getArrayValueOpDef
       TypeSubst { shapeSubst = Map.fromList [("e", eltType)]
                 , widthSubst = Map.fromList [("l", len), ("i", idxType)]
                 }

-- | @splitOpDef oc l w@ returns join operator takes an integer with type @[l*w]@ and returns
-- an array with type @[l][w]@.
splitOpDef :: OpCache -> BitWidth -> BitWidth -> OpDef
splitOpDef oc w1 w2 = cachedOpDef oc (Op.Split w1 w2) mkSplitOpDef


mkSplitOpDef :: OpIndex -> OpDef
mkSplitOpDef ix =
    OpDef { opDefIndex = ix
          , opDefName = "split"
          , opDefPrec = defaultPrec
          , opDefArgTypes = V.singleton $
              SymInt (constantWidth (Wx (l * w)))
          , opDefResultType =
              SymArray (constantWidth (Wx l)) (SymInt (constantWidth (Wx w)))
          , opDefKind = PrimOp
          , opDefEval =
              evalStrictPure1 getUVal $ \_ v ->
                let getVal i = mkCInt (Wx w) $ v `shiftR` (i * w)
                 in CArray $ V.map getVal (V.enumFromN 0 l)

          , opDefLitFn = litStrictPure1 isLV $ \lx ->
              LVN $ V.generate l $ \i -> LV (LV.slice (w*i) w lx)
          }
  where Op.Split (Wx l) (Wx w) = ix

-- | @splitOp oc l w@ returns split operator takes an integer with type @[l*w]@ and returns
-- an array with type @[l][w]@.
splitOp :: OpCache -> BitWidth -> BitWidth -> Op
splitOp oc l w = groundOp (splitOpDef oc l w)

-- | @joinOpDef oc l w@ returns join operator takes an array with type @[l][w]@
-- and returns a single integer with type @l*w@.
-- N.B. Making this operator into a constant requires supporting multiplication
-- in width variables.
joinOpDef :: OpCache -> BitWidth -> BitWidth -> OpDef
joinOpDef oc w1 w2 = cachedOpDef oc (Op.Join w1 w2) mkJoinOpDef

mkJoinOpDef :: OpIndex -> OpDef
mkJoinOpDef ix =
    OpDef { opDefIndex = ix
          , opDefName = "join"
          , opDefPrec = defaultPrec
          , opDefArgTypes = V.singleton $
              SymArray (constantWidth (Wx l)) (SymInt (constantWidth (Wx w)))
          , opDefResultType = SymInt (constantWidth (Wx (l * w)))
          , opDefKind = PrimOp
          , opDefEval = evalStrictPure1 getCArray $ \_ v ->
              let getVal i =
                    let Just uv = getUVal (v V.! i)
                     in uv `shiftL` (i * w)
               in mkCInt (Wx (l * w)) $
                   V.foldl (.|.) 0 (V.map getVal (V.enumFromN 0 l))

          , opDefLitFn = litStrictPure1 isLVN $ \lx ->
                LV (LV.concat (V.toList (V.map toLsbfV lx)))
                {- This is written like this instead of concatMap because
                    we are making an LV.vector. -}
          }
  where Op.Join (Wx l) (Wx w) = ix


type CValuePat a      = CValue -> Maybe a
type LitResultPat l a = LitResult l -> Maybe a

isLVN :: LitResultPat l (V.Vector (LitResult l))
isLVN (LVN x) = Just x
isLVN _       = Nothing

isLV :: LitResultPat l (LV.Vector l)
isLV (LV x) = Just x
isLV _      = Nothing

isCRec :: CValuePat (SymRecDef, TypeSubst, (V.Vector CValue))
isCRec (CRec x y z) = Just (x,y,z)
isCRec _ = Nothing


evalStrictPure1 :: CValuePat a -> (TypeSubst -> a -> CValue) -> DagOpEval
evalStrictPure1 check f = UnaryOpEval $ \s x ->
  do v1 <- x
     case check v1 of
       Just ok1 -> return (f s ok1)
       Nothing  -> fail "internal error: type error in evalStrictPure1"

evalStrictPure2 :: CValuePat a -> CValuePat b ->
                   (TypeSubst -> a -> b -> CValue) -> DagOpEval
evalStrictPure2 check1 check2 f = BinaryOpEval $ \s x y ->
  do v1 <- x
     v2 <- y
     case (check1 v1, check2 v2) of
       (Just ok1, Just ok2) -> return (f s ok1 ok2)
       _ -> fail "internal error: type error in evalStrictPure2"


evalStrictPure3 :: CValuePat a -> CValuePat b -> CValuePat c ->
                   (TypeSubst -> a -> b -> c -> CValue) ->
                   DagOpEval
evalStrictPure3 check1 check2 check3 f = TernaryOpEval $ \s x y z ->
  do v1 <- x
     v2 <- y
     v3 <- z
     case (check1 v1, check2 v2, check3 v3) of
       (Just ok1, Just ok2, Just ok3) -> return (f s ok1 ok2 ok3)
       _ -> fail "internal error: type error in evalStrictPure3"


evalStrictPureV :: (TypeSubst -> V.Vector CValue -> CValue) -> DagOpEval
evalStrictPureV f = VectorOpEval (\s x -> f s `fmap` V.sequence x)



litStrictPure1 :: LitResultPat l a -> (a -> LitResult l) ->
                  TypeSubst -> V.Vector (LitResult l) -> IO (LitResult l)
litStrictPure1 check f _tySubst args =
  assert (V.length args == 1) $
  case check (args V.! 0) of
    Just ok -> return (f ok)
    Nothing -> fail "internal error: type error in litStrictPure1"

litStrictPure2 :: LitResultPat l a -> LitResultPat l b ->
                  (a -> b -> LitResult l) ->
                  TypeSubst -> V.Vector (LitResult l) -> IO (LitResult l)
litStrictPure2 check1 check2 f _tySubst args =
  assert (V.length args == 2) $
  case (check1 (args V.! 0), check2 (args V.! 1)) of
    (Just ok1, Just ok2) -> return (f ok1 ok2)
    _ -> fail "internal error: type error in litStrictPure2"


litStrictPure3 :: LitResultPat l a -> LitResultPat l b -> LitResultPat l c ->
                  (a -> b -> c -> LitResult l) ->
                  TypeSubst -> V.Vector (LitResult l) -> IO (LitResult l)
litStrictPure3 check1 check2 check3 f _tySubst args =
  assert (V.length args == 3) $
  case (check1 (args V.! 0), check2 (args V.! 1), check3 (args V.! 2)) of
    (Just ok1, Just ok2, Just ok3) -> return (f ok1 ok2 ok3)
    _ -> fail "internal error: type error in litStrictPure3"







litStrictPureV :: (V.Vector (LitResult l) -> LitResult l) ->
                  TypeSubst -> V.Vector (LitResult l) -> IO (LitResult l)
litStrictPureV f _tySubst args = return (f args)


-- | @joinOp oc l w@ returns join operator takes an array with type @[l][w]@
-- and returns a single integer with type @l*w@.
joinOp :: OpCache -> BitWidth -> BitWidth -> Op
joinOp oc l w = groundOp (joinOpDef oc l w)

-- | @mkArrayOpDef l@ returns an operator definition that takes @l@ arguments,
-- and joins them into a single array with @l@ elements.
arrayOpDef :: OpCache -> Int -> OpDef
arrayOpDef oc l = cachedOpDef oc (Op.MkArray l) mkArrayOpDef

mkArrayOpDef :: OpIndex -> OpDef
mkArrayOpDef ix =
    OpDef { opDefIndex = ix
          , opDefName = "mkArray"
          , opDefPrec = defaultPrec
          , opDefArgTypes = V.replicate l (SymShapeVar "e")
          , opDefResultType =
              SymArray (constantWidth (Wx l)) (SymShapeVar "e")
          , opDefKind = PrimOp
          , opDefEval = evalStrictPureV $ \_ -> CArray
          , opDefLitFn = litStrictPureV LVN
          }
  where Op.MkArray l = ix

-- | @mkArrayOp l eltType@ returns the mkArray operator that takes @l@ arguments
-- with type @eltType@ and joins them into a single array with @l@ elements of
-- type @eltType@.
mkArrayOp :: OpCache -> Int -> DagType -> Op
mkArrayOp oc l eltType =
  mkOp (arrayOpDef oc l)
       emptySubst { shapeSubst = Map.fromList [("e", eltType)] }
