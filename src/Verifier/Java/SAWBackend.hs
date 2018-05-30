{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module           : Verifier.Java.SAWBackend
Description      :
License          : BSD3
Stability        : provisional
Point-of-contact : atomb
-}
module Verifier.Java.SAWBackend
  ( SharedContext
  , mkSharedContext
  , sawBackend
  , scLoadPreludeModule
  , scLoadCryptolModule
  , scLoadJavaModule
  , basic_ss
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Traversable (traverse)
#endif
import Control.Monad (void, unless)
import Data.AIG (IsAIG)
import qualified Data.AIG as AIG
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word
import Text.PrettyPrint.HughesPJ

import Verifier.SAW
import Verifier.SAW.ParserUtils
import Verifier.SAW.Prelude (scEq)
import Verifier.SAW.Cryptol.Prelude (scLoadCryptolModule)
import qualified Verifier.SAW.Recognizer as R
import Verifier.Java.Backend
import Verifier.Java.SAWImport
import qualified Verifier.SAW.Simulator.BitBlast as BB
import Verifier.SAW.Conversion
import Verifier.SAW.Rewriter

instance AigOps SharedContext where

type instance SBETerm SharedContext = Term

qualify :: String -> Ident
qualify name = mkIdent preludeModuleName name
  where preludeModuleName = mkModuleName ["Prelude"]

cqualify :: String -> Ident
cqualify name = mkIdent cryptolModuleName name
  where cryptolModuleName = mkModuleName ["Cryptol"]

basic_ss :: SharedContext -> IO Simpset
basic_ss sc = do
  rs1 <- concat <$> traverse defRewrites (defs ++ cdefs)
  rs2 <- scEqsRewriteRules sc eqs
  return $ addConvs procs (addRules (rs1 ++ rs2) emptySimpset)
  where
    eqs = map qualify
      [ "not_not", "bvAddZeroL", "bvAddZeroR"
      , "eq_bitvector", "eq_Bool", "eq_VecBool"
      , "ite_true"
      , "ite_false"
      , "ite_not"
      , "ite_nest1"
      , "ite_nest2"
      , "ite_eq"
      , "ite_bit_false_1"
      , "ite_bit_true_1"
      , "ite_bit"
      , "not_not"
      , "and_True"
      , "and_False"
      , "and_True2"
      , "and_False2"
      , "and_idem"
      , "or_True"
      , "or_False"
      , "or_True2"
      , "or_False2"
      , "or_idem"
      , "not_or"
      , "not_and"
      , "boolEq__eq"
      ] ++
      map cqualify [ "seq_TCNum", "seq_TCInf" ]
    defs = map qualify []
    cdefs = map cqualify [ "ecEq" ]
    procs = bvConversions ++ natConversions ++ vecConversions
    defRewrites ident =
      scFindDef sc ident >>= \maybe_def ->
      case maybe_def of
        Nothing -> return []
        Just def -> scDefRewriteRules sc def

sawBackend :: forall l g
            . IsAIG l g
           => SharedContext
           -> Maybe (IORef [Term]) -- ^ For storing the list of generated ExtCns inputs
           -> AIG.Proxy l g
           -> IO (Backend SharedContext)
sawBackend sc0 mr proxy = do
  ss <- basic_ss sc0
  let sc = rewritingSharedContext sc0 ss
  let apply2 op x y   = scApplyAll sc op [x,y]
      apply3 op x y z = scApplyAll sc op [x,y,z]
      apply4 op w x y z = scApplyAll sc op [w,x,y,z]
  let getBuiltin name = scGlobalDef sc (qualify name)

  trueCtor  <- scBool sc True
  falseCtor <- scBool sc False
  iteOp <- getBuiltin "ite"

  bvEqOp <- getBuiltin "bvEq"

  nat0  <- scNat sc 0
  nat8  <- scNat sc 8
  nat32 <- scNat sc 32
  nat64 <- scNat sc 64

  nat1 <- scNat sc 1
  nat7 <- scNat sc 7
  nat15 <- scNat sc 15
  nat16 <- scNat sc 16
  nat24 <- scNat sc 24
  nat31 <- scNat sc 31
  nat63 <- scNat sc 63

  bitvector32 <- scBitvector sc 32
  bitvector64 <- scBitvector sc 64

  -- bvNat :: (x :: Nat) -> Nat -> bitvector x;
  bvNat <- getBuiltin "bvNat"
  bvNat32 <- scApply sc bvNat nat32
  bvNat64 <- scApply sc bvNat nat64

  let asBvNat f t =
        do (g, x) <- R.asApp t
           if alphaEquiv f g then R.asNat x else Nothing

  zero32 <- scApply sc bvNat32 nat0
  zero64 <- scApply sc bvNat64 nat0

  -- bvTrunc :: (x y :: Nat) -> bitvector (addNat y x) -> bitvector y;
  bvTrunc <- getBuiltin "bvTrunc"
  bvTrunc64to32 <- apply2 bvTrunc nat32 nat32
  bvTrunc32to8 <- apply2 bvTrunc nat24 nat8

  -- bvSExt :: (x y :: Nat) -> bitvector (Succ y) -> bitvector (addNat x (Succ y));
  bvSExt <- getBuiltin "bvSExt"
  bvSExt32to64 <- apply2 bvSExt nat32 nat31
  bvSExt1to32 <- apply2 bvSExt nat31 nat0
  bvSExt8to32 <- apply2 bvSExt nat24 nat7
  bvSExt16to32 <- apply2 bvSExt nat16 nat15

  -- bvUExt :: (x y :: Nat) -> bitvector y -> bitvector (addNat y x);
  -- bvUExt <- getBuiltin "bvUExt"
  -- bvUExt8to32 <- apply2 bvUExt nat24 nat8

  boolAndOp <- getBuiltin "and"

  -- bvsle :: (n :: Nat) -> bitvector (Succ n) -> bitvector (Succ n) -> Bool;
  bvsle <- getBuiltin "bvsle"
  bvsle32 <- scApply sc bvsle nat32

  -- bvslt :: (n :: Nat) -> bitvector (Succ n) -> bitvector (Succ n) -> Bool;
  bvslt <- getBuiltin "bvslt"
  bvslt64 <- scApply sc bvslt nat64

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

  -- bvShl :: (n :: Nat) -> bitvector n -> Nat -> bitvector n;
  bvShl <- getBuiltin "bvShl"
  bvShl32 <- scApply sc bvShl nat32
  bvShl64 <- scApply sc bvShl nat64

  -- bvShr :: (n :: Nat) -> bitvector n -> Nat -> bitvector n;
  bvShr <- getBuiltin "bvShr"
  bvShr32 <- scApply sc bvShr nat32
  bvShr64 <- scApply sc bvShr nat64

  -- bvSShr :: (n :: Nat) -> bitvector (Succ n) -> Nat -> bitvector (Succ n);
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
          Nothing -> fail "mkBvToNat32 applied to symbolic length"

  -- replicate :: (n :: Nat) -> (e :: sort 0) -> e -> Vec n e;
  replicateOp <- getBuiltin "replicate"
  let mkArray eltType eltValue l = do
        il <- mkBvToNat32 l
        Just <$> apply3 replicateOp il eltType eltValue

  -- bvAt :: (n :: Nat) -> (e :: sort 0) -> (i :: Nat) -> Vec n e
  --      -> bitvector i -> e;
  getOp <- getBuiltin "bvAt"
  let getArray eltType l a i = do
        lnat <- mkBvToNat32 l
        scApplyAll sc getOp [lnat,eltType,nat32,a,i]

  -- bvUpd :: (n :: Nat) -> (e :: sort 0) -> (i :: Nat) -> Vec n e
  --       -> bitvector i -> e -> Vec n e;
  setOp <- getBuiltin "bvUpd"
  let setArray eltType l a i v = do
        lnat <- mkBvToNat32 l
        scApplyAll sc setOp [lnat,eltType,nat32,a,i,v]

  let mkBvShl32 x y = apply2 bvShl32 x =<< scApply sc bvToNat32 y
  let mkBvShl64 x y = apply2 bvShl64 x =<< scApply sc bvToNat64 y
  let mkBvShr32 x y = apply2 bvShr32 x =<< scApply sc bvToNat32 y
  let mkBvShr64 x y = apply2 bvShr64 x =<< scApply sc bvToNat64 y
  let mkBvSShr32 x y = apply2 bvSShr32 x =<< scApply sc bvToNat32 y
  let mkBvSShr64 x y = apply2 bvSShr64 x =<< scApply sc bvToNat64 y

  -- Compare two 64bit integers (x & y), and return one of three 32-bit integers:
  -- if x < y then return -1; if x == y then return 0; if x > y then return 1
  let termLCompareFn x y = do
        ite32 <- scApply sc iteOp bitvector32
        one32 <- scApply sc bvNat32 nat1
        minusone32 <- scApply sc bvNat32 =<< scNat sc (2^(32::Integer) - 1)
        eq64 <- scApply sc bvEqOp nat64
        eqXY <- scApplyAll sc eq64 [x, y]
        ltXY <- scApplyAll sc bvslt64 [x, y]
        t <- scApplyAll sc ite32 [ltXY, minusone32, one32]
        scApplyAll sc ite32 [eqXY, zero32, t]

  let blastTermFn :: Term -> IO (Maybe Bool)
      blastTermFn t = return (R.asBool t)

  -- Lambda abstract term @t@ over all symbolic variables.
  let abstract :: Term -> IO Term
      abstract t = scAbstractExts sc (Set.toList (getAllExtSet t)) t

  let satTermFn :: Term -> IO Bool
      satTermFn t = do
        t' <- abstract t
        BB.withBitBlastedPred proxy sc (\_ -> Map.empty) t' $ \be l _domTys -> do
            r <- AIG.checkSat be l
            case r of
              AIG.Sat _ -> return True
              AIG.Unsat -> return False
              AIG.SatUnknown -> fail "SAT solver returned 'unknown'"

  let extractEC :: Term -> IO (ExtCns Term)
      extractEC x = do
         x' <- scWhnf sc x
         case getAllExts x' of
           [ec] -> return ec
           [] -> fail $ "input value is not an external constant: " ++ showTerm x'
           ecs  -> fail $ "input value does not uniquely determine an external constant for abstraction: " ++ show x' ++ "\n" ++ show (map ecName ecs)

  let writeAigToFileFn :: FilePath -> [Term] -> [Term] -> IO ()
      writeAigToFileFn fname ins outs = do
        -- Usage of this function in 'Verifier.Java.Simulator' is only
        -- at arrays for parameter @outs@, so that 'scVector' would be
        -- appropriate below. However, seems safer to not worry about
        -- this invariant and so use 'scTuple'; the result of bit
        -- blasting should be the same.
        t <- scTuple sc outs

        -- If inputs are supplied, abstract over them explicitly.  Otherwise,
        -- abstract over all ECs appearing in the term.
        t' <- case ins of
                [] -> abstract t
                _ -> do
                   ecs <- mapM extractEC ins
                   t' <- scAbstractExts sc ecs t
                   unless (null (getAllExts t'))
                          (fail "Term depends on an input not supplied")
                   return t'
        BB.withBitBlastedTerm proxy sc (\_ -> Map.empty) t' $ \be ls -> do
        AIG.writeAiger fname (AIG.Network be (AIG.bvToList ls))

  -- Very similar to 'SAWScript.Builtins.writeCNF' and
  -- 'Verifier.LLVM.Backend.SAW.scWriteCNF'.
  let writeCnfToFileFn :: FilePath -> Term -> IO ()
      writeCnfToFileFn fname out = do
        t' <- abstract out
        BB.withBitBlastedPred proxy sc (\_ -> Map.empty) t' $ \be l _domTys -> do
        void $ AIG.writeCNF be l fname

  let maybeCons =
        case mr of
          Nothing -> \_ -> return ()
          Just r -> \t -> modifyIORef r (t :)

  let freshVar n ext = do
        ty <- scBitvector sc n
        i <- scFreshGlobalVar sc
        let ec = EC i ("_"++show i) ty
        t <- scFlatTermF sc (ExtCns ec)
        maybeCons t
        ext t

  return Backend { freshBool  = freshVar 1 (scApply sc bvSExt1to32)
                 , freshByte  = freshVar 8 (scApply sc bvSExt8to32)
                 , freshChar  = freshVar 16 (scApply sc bvSExt16to32)
                 , freshShort = freshVar 16 (scApply sc bvSExt16to32)
                 , freshInt   = freshVar 32 return
                 , freshLong  = freshVar 64 return
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

                 , termEq    = \x y ->
                     case (x, y) of
                         (STApp{ stAppIndex = ix }, STApp{ stAppIndex = iy })
                            | ix == iy -> scBool sc True
                         _ -> scEq sc x y

                 , termIte   = \b x y -> do
                     case R.asBool b of
                       Just True -> return x
                       Just False -> return y
                       _ -> case (x, y) of
                              (STApp{ stAppIndex = ix}, STApp{ stAppIndex = iy})
                                 | ix == iy -> return x
                              _ -> do
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
                 , writeCnfToFile     = writeCnfToFileFn
                 , satTerm            = satTermFn
                 -- TODO: refactor to use the same Doc everywhere
                 , prettyTermD        = text . showTerm
                 }
