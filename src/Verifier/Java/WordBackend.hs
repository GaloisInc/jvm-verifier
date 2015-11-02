{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : jhendrix
-}
module Verifier.Java.WordBackend 
       ( -- * Re-exports from Verifier infrastructure.         
         DagTerm
       , mkOpCache
       , mkCInt
       , concreteEvalFn
       , toLsbfV
       , toLsbf_lit
       , CValue
       , getSVal
       , BitEngine(..)
       , evalAig
       , writeAiger
         -- * Utilities
       , constInt
       , constLong
       , intToBoolSeq
         -- * Backend Exports
       , module Verifier.Java.Backend
       , SymbolicMonad
       , mkSymbolicMonadState
       , withSymbolicMonadState
       , withFreshBackend
       , SymbolicMonadState(..)
       , symbolicBackend
       , evalAigArgs8
       , evalAigArgs32
       , evalAigArgs64
       ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception (assert, bracket)
import Control.Monad (void)
import Data.Bits
import Data.Int
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import Verinf.Symbolic.Common
import Verinf.Symbolic

import Verifier.Java.Backend
import Verifier.Java.Utils

data SymbolicMonadState = SMS {
    smsOpCache :: OpCache
  , smsBitEngine :: BitEngine Lit
  , smsDagEngine :: DagEngine
  , smsVerbosityRef :: IORef Int
  , smsInputLitRef :: IORef (Map.Map InputIndex (LitResult Lit))
  , smsBitBlastFn :: DagTerm -> IO (LitResult Lit)
  }

mkSymbolicMonadState :: OpCache
                     -> BitEngine Lit
                     -> DagEngine
                     -> IO SymbolicMonadState
mkSymbolicMonadState oc be de = do
  vr <- newIORef 1
  lr <- newIORef Map.empty
  let bitBlastInput i _ = do
        m <- readIORef lr
        case Map.lookup i m of
          Just l -> return l
          Nothing -> error "Cannot bitblast uninterpreted input variable"
  bbFn <- evalDagTermFn bitBlastInput (mkBitBlastTermSemantics be)
  return SMS { smsOpCache = oc
             , smsBitEngine = be
             , smsDagEngine = de
             , smsVerbosityRef = vr
             , smsInputLitRef = lr
             , smsBitBlastFn = bbFn
             }

data SymbolicMonad
instance AigOps SymbolicMonad where
type instance SBETerm SymbolicMonad = DagTerm

withBitEngine :: (BitEngine Lit -> IO a) -> IO a
withBitEngine = bracket createBitEngine beFree

-- | Create a fresh symbolic backend with a new op cache, and execute it.
withFreshBackend :: (Backend SymbolicMonad -> IO a) -> IO a
withFreshBackend f = do
  oc <- mkOpCache
  withBitEngine $ \be -> do
    de <- mkConstantFoldingDagEngine
    sms <- mkSymbolicMonadState oc be de
    f (symbolicBackend sms)

withSymbolicMonadState :: OpCache -> (SymbolicMonadState -> IO a) -> IO a
withSymbolicMonadState oc f =
  withBitEngine $ \be -> do
    de <- mkConstantFoldingDagEngine
    f =<< mkSymbolicMonadState oc be de

symbolicBackend :: SymbolicMonadState -> Backend SymbolicMonad
symbolicBackend sms = do
  let ?be = smsBitEngine sms
  let be = smsBitEngine sms
  let de = smsDagEngine sms
  let oc = smsOpCache sms
  let lr = smsInputLitRef sms 
  let getTermLit = smsBitBlastFn sms
  let freshTerm tp lv = do  
        n@(InputTerm i _) <- deFreshInput (smsDagEngine sms) tp
        m <- readIORef lr
        writeIORef lr $! (Map.insert i (LV lv) m)
        return n
  let termTrunc resultWidth t = do
        case termType t of
          SymInt iw@(widthConstant -> Just inputWidth)
            | inputWidth >= resultWidth ->
              deApplyUnary de (truncOp oc iw resultWidth) t
          _ -> error "internal: illegal arguments to termTrunc"
  let termShift opFn name w x y = do
        case (termType x, termType y) of
          (SymInt vw, SymInt sw@(widthConstant -> Just inputWidth))
            | inputWidth >= w -> do
                y' <- deApplyUnary de (truncOp oc sw w) y
                deApplyBinary de (opFn vw (constantWidth w)) x y'
          _ -> error $ "internal: illegal value to " ++ name ++ ": " ++ show x
  let termIteFn typedIte b mt mf =
        case getBool b of
          Just True -> mt
          Just False -> mf
          Nothing -> do
            t <- mt
            f <- mf
            case t == f of
              True -> return t
              False -> deApplyTernary de typedIte b t f
  let w32 = constantWidth 32
      w64 = constantWidth 64
  let getArray _ a i =
        case termType a of
          SymArray len eltType ->
            deApplyBinary de (getArrayValueOp len w32 eltType) a i
          _ -> error $ "internal: illegal arguments to getArray "
                        ++ prettyTerm a ++ "\n" ++ show (termType a)
  let setArray _ a i v =
        case termType a of
          SymArray len eltType ->
            deApplyTernary de (setArrayValueOp len w32 eltType) a i v
          _ -> error "internal: illegal arguments to setArray"
  Backend {
      freshBool = do
        l <- beMakeInputLit be
        freshTerm int32Type (SV.singleton l)
    , freshByte = do
        inputs <- SV.replicateM 7 (beMakeInputLit be)
        msb <- beMakeInputLit be
        let lv = inputs SV.++ SV.replicate 25 msb
        freshTerm int32Type lv
    , freshChar = do
        inputs <- SV.replicateM 15 (beMakeInputLit be)
        msb <- beMakeInputLit be
        let lv = inputs SV.++ SV.replicate 17 msb
        freshTerm int32Type lv
    , freshShort = do
        inputs <- SV.replicateM 15 (beMakeInputLit be)
        msb <- beMakeInputLit be
        let lv = inputs SV.++ SV.replicate 17 msb
        freshTerm int32Type lv
    , freshInt = do
        lv <- SV.replicateM 32 (beMakeInputLit be)
        freshTerm int32Type lv
    , freshLong = do
        lv <- SV.replicateM 64 (beMakeInputLit be)
        freshTerm int64Type lv
    , asBool = getBool
    , asInt = \x -> fromInteger <$> getSVal x
    , asLong = \x -> fromInteger <$> getSVal x
    , termBool = return . mkCBool
    , termInt  = return . mkCInt 32 . toInteger
    , termLong = return . mkCInt 64 . toInteger
    , termByteFromInt = \x -> termTrunc 8 x
    , termLongFromInt = \t ->
        case termType t of
          SymInt iw@(widthConstant -> Just inputWidth)
            | inputWidth <= 64  -> 
              deApplyUnary de (signedExtOp oc iw 64) t
          _ -> error "internal: illegal value to termLongFromInt"
    , termIntFromLong = \x -> termTrunc 32 x
    , termNot  = deApplyUnary de bNotOp
    , termAnd  = deApplyBinary de bAndOp
    , termEq   = \x y -> deApplyBinary de (eqOp (termType x)) x y
    , termIte  = \b t f -> termIteFn (iteOp (termType t)) b (return t) (return f)
    , termILeq  = deApplyBinary de (signedLeqOp w32)
    , termIAnd  = deApplyBinary de (iAndOp w32)
    , termIOr   = deApplyBinary de (iOrOp w32)
    , termIXor  = deApplyBinary de (iXorOp w32)
    , termIShl  = termShift shlOp  "termIShl"  5 
    , termIShr  = termShift shrOp  "termIShr"  5
    , termIUshr = termShift ushrOp "termIUshr" 5 
    , termINeg  = deApplyUnary de (negOp w32)
    , termIAdd  = deApplyBinary de (addOp w32)
    , termISub  = deApplyBinary de (subOp w32)
    , termIMul  = deApplyBinary de (mulOp w32)
    , termIDiv  = deApplyBinary de (signedDivOp w32)
    , termIRem  = deApplyBinary de (signedRemOp w32)

    , termLCompare = \x y -> do
        eqXY   <- deApplyBinary de (eqOp int64Type) x y
        let ite = termIteFn (iteOp int32Type)
        ite eqXY
            (return (mkCInt 32 0))
            (do ltXY <- deApplyBinary de (signedLtOp (constantWidth 64)) x y
                ite ltXY
                    (return (mkCInt 32 (negate 1)))
                    (return (mkCInt 32 1)))
    , termLAnd  = deApplyBinary de (iAndOp w64)
    , termLOr   = deApplyBinary de (iOrOp w64)
    , termLXor  = deApplyBinary de (iXorOp w64)
    , termLShl  = termShift shlOp  "termLShl"  6
    , termLShr  = termShift shrOp  "termLShr"  6
    , termLUshr = termShift ushrOp "termLUshr" 6
    , termLNeg  = deApplyUnary de (negOp w64)
    , termLAdd  = deApplyBinary de (addOp w64)
    , termLSub  = deApplyBinary de (subOp w64)
    , termLMul  = deApplyBinary de (mulOp w64)
    , termLDiv  = deApplyBinary de (signedDivOp w64)
    , termLRem  = deApplyBinary de (signedRemOp w64)

    , termIntArray = \l ->
        case fromIntegral <$> getSVal l of
          Just c ->
            Just <$> deApplyOp de (mkArrayOp oc c int32Type)
                       (V.replicate c (mkCInt 32 0))  
          Nothing -> return Nothing
    , termLongArray = \l ->
        case fromIntegral <$> getSVal l of
          Just c ->
            Just <$> deApplyOp de (mkArrayOp oc c int64Type)
                       (V.replicate c (mkCInt 64 0))  
          Nothing -> return Nothing
    , termGetIntArray  = getArray
    , termGetLongArray = getArray
    , termSetIntArray  = setArray
    , termSetLongArray = setArray
    , blastTerm = \v -> do
        LV lv <- getTermLit v
        let l = assert (SV.length lv == 1) $ SV.head lv
        if l == (beTrue be) then
          return (Just True)
        else if l == (beFalse be) then
          return (Just False)
        else
          return Nothing
    , satTerm = \v -> do
        LV lv <- getTermLit v
        let l = assert (SV.length lv == 1) $ SV.head lv
        case beCheckSat be of
          Just f -> (/= UnSat) `fmap` f l
          Nothing -> return True
    , evalAigIntegral = \f ins out -> do
        outLits <- getTermLit out
        bbits <- beEvalAigV be (SV.fromList (concatMap (f . intToBoolSeq . fromJust . termConst) ins))
                               (toLsbfV outLits)
        return $ mkCInt (Wx (SV.length bbits)) $ boolSeqToValue (SV.toList bbits)
    , evalAigArray = \n ins outs -> do
        -- TODO: Report sensible error if ins is not constants.
        let bits = case n of
                     8  -> evalAigArgs8  $ map (fromInteger . fromJust . getSVal) ins
                     32 -> evalAigArgs32 $ map (fromInteger . fromJust . getSVal) ins
                     64 -> evalAigArgs64 $ map (fromInteger . fromJust . getSVal) ins
                     _  -> error $ "evalAigArray: input array elements have unexpected bit width"
        outLits <- mapM getTermLit outs
        rs <- beEvalAigV be (SV.fromList bits)
                            (SV.concat (map toLsbfV outLits))
        let rsl = SV.toList rs
        case n of
          8  -> return $ map (mkCInt 32 . boolSeqToValue) $ splitN 8 rsl
          32 -> return $ map (mkCInt 32 . boolSeqToValue) $ splitN 32 rsl
          64 -> return $ map (mkCInt 64 . boolSeqToValue) $ splitN 64 rsl
          _  -> error $ "evalAigArray: input array elements have unexpected bit width"
   , writeAigToFile = \fname ins res -> do
       case ins of
         [] -> do
            e <- mapM getTermLit res
            let elts = concatMap (SV.toList . toLsbfV) e
            allIns <- beInputLits be
            beWriteAigerV be fname allIns (SV.fromList elts)
         _ -> do
            fail "FIXME: word backends do not support explict input specification when writing AIGER files"
   , writeCnfToFile = \fname res -> do
       LV l <- getTermLit res
       void $ beWriteCNF be fname mempty (SV.head l)
   , Verifier.Java.Backend.prettyTermD = Verinf.Symbolic.prettyTermD
   }

-- Misc utility functions {{{1

int32Type :: DagType
int32Type = SymInt (constantWidth 32)

int64Type :: DagType
int64Type = SymInt (constantWidth 64)

intToBoolSeq :: CValue -> [Bool]
intToBoolSeq (getSValW -> Just (Wx w, c)) = map (testBit c) [0..w-1]
intToBoolSeq _ = error "internal: intToBoolSeq undefined"

constInt :: Int32 -> CValue
constInt = mkCInt 32 . fromIntegral

constLong :: Int64 -> CValue
constLong = mkCInt 64 . fromIntegral

evalAigArgs8 :: [Int32] -> [Bool]
evalAigArgs8 = concatMap (\c -> map (testBit c) [0..7])

evalAigArgs32 :: [Int32] -> [Bool]
evalAigArgs32 = concatMap (\c -> map (testBit c) [0..31]) 

evalAigArgs64 :: [Int64] -> [Bool]
evalAigArgs64 = concatMap (\c -> map (testBit c) [0..63])
