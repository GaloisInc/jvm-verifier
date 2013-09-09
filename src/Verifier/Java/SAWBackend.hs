{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.Java.SAWBackend
  ( mkSharedContext
  , sawBackend
  , withFreshBackend
  ) where

import Control.Applicative
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Text.PrettyPrint.HughesPJ
import Data.Traversable (traverse)
import Data.Word

import Verifier.SAW
import qualified Verifier.SAW.Recognizer as R
import Verifier.Java.Backend
import qualified Verifier.SAW.BitBlast as BB
import Verifier.SAW.Conversion
import Verifier.SAW.Rewriter
import Verifier.SAW.TypedAST (mkModuleName, findDef, FlatTermF(..), ExtCns(..))
import qualified Verinf.Symbolic as BE


instance Typeable (SharedTerm s) where

instance AigOps (SharedContext s) where

type instance SBETerm (SharedContext s) = SharedTerm s
type instance SBELit (SharedContext s) = BE.Lit

qualify :: String -> Ident
qualify name = mkIdent preludeModuleName name
  where preludeModuleName = mkModuleName ["Prelude"]

basic_ss :: SharedContext s -> IO (Simpset (SharedTerm s))
basic_ss sc = do
  rs1 <- concat <$> traverse defRewrites defs
  rs2 <- scEqsRewriteRules sc eqs
  return $ addConvs procs (addRules (rs1 ++ rs2) emptySimpset)
  where
    eqs = map qualify
      ["get_single", "get_set", "get_bvAnd", "get_bvOr", "get_bvXor", "get_bvNot",
       "not_not", "get_slice", "bvAddZeroL", "bvAddZeroR", "eq_Fin", "eq_bitvector"]
    defs = map qualify
      ["not", "and", "or", "xor", "boolEq", "ite", "addNat", "mulNat", "compareNat",
       "finSucc", "finFst"]
    procs = bvConversions ++ natConversions ++ finConversions ++ vecConversions
    defRewrites ident =
      case findDef (scModule sc) ident of
        Nothing -> return []
        Just def -> scDefRewriteRules sc def

withFreshBackend :: (Backend (SharedContext s) -> IO a) -> IO a
withFreshBackend f = do
  sc <- mkSharedContext preludeModule
  be <- BE.createBitEngine
  backend <- sawBackend sc Nothing be
  r <- f backend
  BE.beFree be
  return r

sawBackend :: forall s. SharedContext s
           -> Maybe (IORef [SharedTerm s]) -- ^ For storing the list of generated ExtCns inputs
           -> BE.BitEngine BE.Lit -- TODO: make this argument optional
           -> IO (Backend (SharedContext s))
sawBackend sc0 mr be = do
  ss <- basic_ss sc0
  let sc = rewritingSharedContext sc0 ss
  let apply2 op x y   = scApplyAll sc op [x,y]
      apply3 op x y z = scApplyAll sc op [x,y,z]
      apply4 op w x y z = scApplyAll sc op [w,x,y,z]
  let getBuiltin name = scGlobalDef sc (qualify name)

  boolType  <- scBoolType sc
  trueCtor  <- scBool sc True
  falseCtor <- scBool sc False
  iteOp <- getBuiltin "ite"

  eqOp <- getBuiltin "eq"

  nat0  <- scNat sc 0
  nat8  <- scNat sc 8
  nat32 <- scNat sc 32
  nat64 <- scNat sc 64

  nat1 <- scNat sc 1
  nat7 <- scNat sc 7
  nat24 <- scNat sc 24
  nat31 <- scNat sc 31
  nat63 <- scNat sc 63

  bitvector8 <- scBitvector sc 8
  bitvector32 <- scBitvector sc 32
  bitvector64 <- scBitvector sc 64

  -- bvNat :: (x :: Nat) -> Nat -> bitvector x;
  bvNat <- getBuiltin "bvNat"
  bvNat32 <- scApply sc bvNat nat32
  bvNat64 <- scApply sc bvNat nat64

  let asBvNat f t =
        do (g, x) <- R.asApp t
           if f == g then R.asNatLit x else Nothing

  zero32 <- scApply sc bvNat32 nat0
  zero64 <- scApply sc bvNat64 nat0

  -- bvTrunc :: (x y :: Nat) -> bitvector (addNat y x) -> bitvector y;
  bvTrunc <- getBuiltin "bvTrunc"
  bvTrunc64to32 <- apply2 bvTrunc nat32 nat32
  bvTrunc32to8 <- apply2 bvTrunc nat24 nat8

  -- bvSExt :: (x y :: Nat) -> bitvector (Succ y) -> bitvector (addNat (Succ y) x);
  bvSExt <- getBuiltin "bvSExt"
  bvSExt32to64 <- apply2 bvSExt nat31 nat32
  bvSExt8to32 <- apply2 bvSExt nat7 nat24

  -- bvUExt :: (x y :: Nat) -> bitvector y -> bitvector (addNat y x);
  bvUExt <- getBuiltin "bvUExt"
  bvUExt8to32 <- apply2 bvUExt nat24 nat8

  boolAndOp <- getBuiltin "and"

  -- bvsle :: (n :: Nat) -> bitvector (Succ n) -> bitvector (Succ n) -> Bool;
  bvsle <- getBuiltin "bvsle"
  bvsle32 <- scApply sc bvsle nat31

  -- bvslt :: (n :: Nat) -> bitvector (Succ n) -> bitvector (Succ n) -> Bool;
  bvslt <- getBuiltin "bvslt"
  bvslt64 <- scApply sc bvslt nat63

  -- bvAnd :: (n :: Nat) -> bitvector n -> bitvector n -> bitvector n;
  bvAnd <- getBuiltin "bvAnd"
  bvAnd32 <- scApply sc bvAnd nat32
  bvAnd64 <- scApply sc bvAnd nat64

  -- bvOr :: (n :: Nat) -> bitvector n -> bitvector n -> bitvector n;
  bvOr <- getBuiltin "bvOr"
  bvOr32 <- scApply sc bvOr nat32
  bvOr64 <- scApply sc bvOr nat64

  -- bvXor :: (n :: Nat) -> bitvector n -> bitvector n -> bitvector n;
  bvXor <- getBuiltin "bvXor"
  bvXor32 <- scApply sc bvXor nat32
  bvXor64 <- scApply sc bvXor nat64

  -- bvShl :: (x :: Nat) -> bitvector x -> Nat -> bitvector x;
  bvShl <- getBuiltin "bvShl"
  bvShl32 <- scApply sc bvShl nat32
  bvShl64 <- scApply sc bvShl nat64

  -- bvShr :: (w :: Nat) -> bitvector w -> Nat -> bitvector w;
  bvShr <- getBuiltin "bvShr"
  bvShr32 <- scApply sc bvShr nat32
  bvShr64 <- scApply sc bvShr nat64

  -- bvSShr :: (w :: Nat) -> bitvector (Succ w) -> Nat -> bitvector (Succ w);
  bvSShr <- getBuiltin "bvSShr"
  bvSShr32 <- scApply sc bvSShr nat31
  bvSShr64 <- scApply sc bvSShr nat63

  -- bvNeg :: (x :: Nat) -> bitvector x -> bitvector x;
  bvNeg <- getBuiltin "bvNeg"
  bvNeg32 <- scApply sc bvNeg nat32
  bvNeg64 <- scApply sc bvNeg nat64

  -- bvAdd :: (x :: Nat) -> bitvector x -> bitvector x -> bitvector x;
  bvAdd <- getBuiltin "bvAdd"
  bvAdd32 <- scApply sc bvAdd nat32
  bvAdd64 <- scApply sc bvAdd nat64

  -- bvSub :: (x :: Nat) -> bitvector x -> bitvector x -> bitvector x;
  bvSub <- getBuiltin "bvSub"
  bvSub32 <- scApply sc bvSub nat32
  bvSub64 <- scApply sc bvSub nat64

  -- bvMul :: (x :: Nat) -> bitvector x -> bitvector x -> bitvector x;
  bvMul <- getBuiltin "bvMul"
  bvMul32 <- scApply sc bvMul nat32
  bvMul64 <- scApply sc bvMul nat64

  -- bvSDiv :: (x :: Nat) -> bitvector (Succ x) -> bitvector (Succ x) -> bitvector (Succ x);
  bvSDiv <- getBuiltin "bvSDiv"
  bvSDiv32 <- scApply sc bvSDiv nat31
  bvSDiv64 <- scApply sc bvSDiv nat63

  -- bvSRem :: (x :: Nat) -> bitvector (Succ x) -> bitvector (Succ x) -> bitvector (Succ x);
  bvSRem <- getBuiltin "bvSRem"
  bvSRem32 <- scApply sc bvSRem nat31
  bvSRem64 <- scApply sc bvSRem nat63

  -- bvToNat :: (n :: Nat) -> bitvector n -> Nat;
  bvToNat <- getBuiltin "bvToNat"
  bvToNat32 <- scApply sc bvToNat nat32
  bvToNat64 <- scApply sc bvToNat nat64

  let mkBvToNat32 t =
        case asBvNat bvNat32 t of
          Just n -> scNat sc n
          Nothing -> scApply sc bvToNat32 t

  let mkBvToNat64 t =
        case asBvNat bvNat64 t of
          Just n -> scNat sc n
          Nothing -> scApply sc bvToNat64 t

  -- replicate :: (n :: Nat) -> (e :: sort 0) -> e -> Vec n e;
  replicateOp <- getBuiltin "replicate"
  let mkArray eltType eltValue l = do
        il <- mkBvToNat32 l
        Just <$> apply3 replicateOp il eltType eltValue

  -- An ugly hack to coerce Nat values to type Fin n
  dummyNat <- getBuiltin "dummyNat"

  -- get :: (n :: Nat) -> (e :: sort 0) -> Vec n e -> Fin n -> e;
  getOp <- getBuiltin "get"
  let getArray eltType l a i = do
        lnat <- mkBvToNat32 l
        inat <- mkBvToNat32 i
        ifin <- scFinVal sc inat dummyNat
        scApplyAll sc getOp [lnat,eltType,a,ifin]

  -- set :: (n :: Nat) -> (e :: sort 0) -> Vec n e -> Fin n -> e -> Vec n e;
  setOp <- getBuiltin "set"
  let setArray eltType l a i v = do
        lnat <- mkBvToNat32 l
        inat <- mkBvToNat32 i
        ifin <- scFinVal sc inat dummyNat
        scApplyAll sc setOp [lnat,eltType,a,ifin,v]

  let mkBvShl32 x y = do
        ynat <- mkBvToNat32 y
        apply2 bvShl32 x ynat

  let mkBvShl64 x y = do
        ynat <- mkBvToNat64 y
        apply2 bvShl64 x ynat

  let mkBvShr32 x y = do
        ynat <- mkBvToNat32 y
        apply2 bvShr32 x ynat

  let mkBvShr64 x y = do
        ynat <- mkBvToNat64 y
        apply2 bvShr64 x ynat

  let mkBvSShr32 x y = do
        ynat <- mkBvToNat32 y
        apply2 bvSShr32 x ynat

  let mkBvSShr64 x y = do
        ynat <- mkBvToNat64 y
        apply2 bvSShr64 x ynat

  -- | Compare two 64bit integers (x & y), and return one of three 32-bit integers:
  -- if x < y then return -1; if x == y then return 0; if x > y then return 1
  let termLCompareFn x y = do
        ite32 <- scApply sc iteOp bitvector32
        one32 <- scApply sc bvNat32 nat1
        minusone32 <- scApply sc bvNat32 =<< scNat sc (2^32 - 1)
        eq64 <- scApply sc eqOp bitvector64
        eqXY <- scApplyAll sc eq64 [x, y]
        ltXY <- scApplyAll sc bvslt64 [x, y]
        t <- scApplyAll sc ite32 [ltXY, minusone32, one32]
        scApplyAll sc ite32 [eqXY, zero32, t]

  inputsRef <- newIORef M.empty

  let blastTermFn :: SharedTerm s -> IO (Maybe Bool)
      blastTermFn t = return (R.asBool t)

  let bitblast :: SharedTerm s -> IO (SV.Vector BE.Lit)
      bitblast t =
        do env <- readIORef inputsRef
           mbterm <- BB.bitBlastWithEnv be env t
           case mbterm of
             Left msg -> fail $ "Can't bitblast term: " ++ msg
             Right bterm -> return $ BB.flattenBValue bterm

  let writeAigToFileFn :: FilePath -> SV.Vector BE.Lit -> IO ()
      writeAigToFileFn fname outs = do
        ins <- BE.beInputLits be
        BE.beWriteAigerV be fname ins outs

  let getVarLitFn :: SharedTerm s -> IO (SV.Vector BE.Lit)
      getVarLitFn t = bitblast t

  let maybeCons =
        case mr of
          Nothing -> \t -> return ()
          Just r -> \t -> modifyIORef r (t :)

  return Backend { freshByte = do
                     i <- scFreshGlobalVar sc
                     t <- scFlatTermF sc (ExtCns (EC i "_" bitvector8))
                     v <- BB.BVector <$> V.replicateM 8 (BB.BBool <$> BE.beMakeInputLit be)
                     modifyIORef inputsRef (M.insert i v)
                     maybeCons t
                     scApply sc bvSExt8to32 t
                 , freshInt  = do
                     i <- scFreshGlobalVar sc
                     t <- scFlatTermF sc (ExtCns (EC i "_" bitvector32))
                     v <- BB.BVector <$> V.replicateM 32 (BB.BBool <$> BE.beMakeInputLit be)
                     modifyIORef inputsRef (M.insert i v)
                     maybeCons t
                     return t
                 , freshLong = do
                     i <- scFreshGlobalVar sc
                     t <- scFlatTermF sc (ExtCns (EC i "_" bitvector64))
                     v <- BB.BVector <$> V.replicateM 64 (BB.BBool <$> BE.beMakeInputLit be)
                     modifyIORef inputsRef (M.insert i v)
                     maybeCons t
                     return t
                 , asBool = R.asBool
                 , asInt  = fmap fromIntegral . asBvNat bvNat32 -- Maybe Int32
                 , asLong = fmap fromIntegral . asBvNat bvNat64 -- Maybe Int64
                 , termBool = \b -> return $ if b then trueCtor else falseCtor
                 , termInt  = \w -> do
                                x <- scNat sc (fromIntegral (fromIntegral w :: Word32))
                                scApply sc bvNat32 x
                 , termLong = \w -> do
                                x <- scNat sc (fromIntegral (fromIntegral w :: Word64))
                                scApply sc bvNat64 x
                 , termByteFromInt = scApply sc bvTrunc32to8
                 , termLongFromInt = scApply sc bvSExt32to64
                 , termIntFromLong = scApply sc bvTrunc64to32

                 , termNot   = scNot sc
                 , termAnd   = apply2 boolAndOp

                 , termEq    = \x y -> do
                     xTp <- scTypeOf sc x
                     apply3 eqOp xTp x y
                 , termIte   = \b x y -> do
                     tp <- scTypeOf sc x
                     apply4 iteOp tp b x y

                 , termILeq  = apply2 bvsle32
                 , termIAnd  = apply2 bvAnd32
                 , termIOr   = apply2 bvOr32
                 , termIXor  = apply2 bvXor32
                 , termIShl  = mkBvShl32
                 , termIShr  = mkBvSShr32
                 , termIUshr = mkBvShr32

                 , termINeg  = scApply sc bvNeg32
                 , termIAdd  = apply2 bvAdd32
                 , termISub  = apply2 bvSub32
                 , termIMul  = apply2 bvMul32
                 , termIDiv  = apply2 bvSDiv32
                 , termIRem  = apply2 bvSRem32

                 , termLCompare = termLCompareFn
                 , termLAnd  = apply2 bvAnd64
                 , termLOr   = apply2 bvOr64
                 , termLXor  = apply2 bvXor64
                 , termLShl  = mkBvShl64
                 , termLShr  = mkBvSShr64
                 , termLUshr = mkBvShr64

                 , termLNeg  = scApply sc bvNeg64
                 , termLAdd  = apply2 bvAdd64
                 , termLSub  = apply2 bvSub64
                 , termLMul  = apply2 bvMul64
                 , termLDiv  = apply2 bvSDiv64
                 , termLRem  = apply2 bvSRem64

                 , termIntArray  = mkArray bitvector32 zero32
                 , termLongArray = mkArray bitvector64 zero64
                 , termGetIntArray    = getArray bitvector32
                 , termGetLongArray   = getArray bitvector64
                 , termSetIntArray    = setArray bitvector32
                 , termSetLongArray   = setArray bitvector64
                 , blastTerm          = blastTermFn
                 , evalAigIntegral    = \_ _ _ -> error "evalAigIntegral unimplemented"
                 , evalAigArray       = \_ _ _ -> error "evalAigArray unimplemented"
                 , writeAigToFile     = writeAigToFileFn
                 , writeCnfToFile     = \_ _ -> error "writeCnfToFile unimplemented"
                 , getVarLit          = getVarLitFn
                 , satTerm            = error "satTerm unimplemented"
                 -- TODO: refactor to use the same Doc everywhere
                 , prettyTermD        = text . show . scPrettyTermDoc
                 }
