{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Verifier.Java.SAWBackend
  ( mkSharedContext
  , sawBackend
  , withFreshBackend
  , javaModule
  ) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map

import Verifier.SAW
import Verifier.SAW.ParserUtils
import Verifier.SAW.Prelude
import qualified Verifier.SAW.Recognizer as R
import Verifier.Java.Backend

$(runDecWriter $ do
    prelude <- defineImport [|preludeModule|] preludeModule
    java <- defineModuleFromFile [prelude] "javaModule" "saw/Java.sawcore"
    declareDefTermF prelude "ite"
    declareSharedModuleFns "Java" (decVal java)
 )

instance Typeable (SharedTerm s) where

instance AigOps (SharedContext s) where

type instance SBETerm (SharedContext s) = SharedTerm s
type instance SBELit (SharedContext s) = Int --BH FIXME: what should this be?

withFreshBackend :: (Backend (SharedContext s) -> IO a) -> IO a
withFreshBackend f = do
  sc <- mkSharedContext preludeModule
  be <- sawBackend sc (error "FIXME :: Map String (SharedTerm s)")
  f be

sawBackend :: SharedContext s
              -- Maps symbol names to the term.
              -- The term names are defined in SAW.types 
           -> Map String (SharedTerm s)
           -> IO (Backend (SharedContext s))
sawBackend sc preludeSyms = do
  let apply2 op x y   = scApplyAll sc op [x,y]
      apply3 op x y z = scApplyAll sc op [x,y,z]
      apply4 op w x y z = scApplyAll sc op [w,x,y,z]
  let getBuiltin nm =
        case Map.lookup nm preludeSyms of
          Nothing -> fail $ "Could not find symbol " ++ show nm ++ " in prelude."
          Just t -> return t

  boolType  <- getBuiltin "Bool"
  trueCtor  <- getBuiltin "True"
  falseCtor <- getBuiltin "False"
  iteOp <- getBuiltin "ite"

  atomicProof <- getBuiltin "AtomicProof"

  eqOp <- getBuiltin "eq"

  int0  <- scNat sc 0
  int8  <- scNat sc 8
  int32 <- scNat sc 32
  int64 <- scNat sc 64

  signedType <- getBuiltin "Signed"
  signed8  <- scApply sc signedType int8
  signed32 <- scApply sc signedType int32
  signed64 <- scApply sc signedType int64

  signedToInt <- getBuiltin "signedToInteger"
  signed32ToInt <- scApply sc signedToInt int32

  intToSigned <- getBuiltin "integerToSigned"
  intToSigned32 <- scApply sc intToSigned int32
  intToSigned64 <- scApply sc intToSigned int64

  signed32_0 <- scApply sc intToSigned32 int0
  signed64_0 <- scApply sc intToSigned64 int0

  resizeSignedOp <- getBuiltin "resizeSigned"
  byteFromIntFn <- apply3 resizeSignedOp int32 int8  atomicProof
  longFromIntFn <- apply3 resizeSignedOp int32 int64 atomicProof
  intFromLongFn <- apply3 resizeSignedOp int64 int32 atomicProof
  
  -- Ordering operations.

  leqOp <- getBuiltin "leq"
 
  signedOrdInstance <- getBuiltin "signedOrdInstance"
  signed32OrdInstance <- scApply sc signedOrdInstance int32  
 
  signed32LeqOp <- apply2 leqOp signed32 signed32OrdInstance

  longCompare <- getBuiltin "javaLongCompare"

  -- Generic Bits operations.

  -- Boolean operations.
  notOp <- getBuiltin "not"
  andOp <- getBuiltin "and"

  boolBitsInstance <- getBuiltin "boolBitsInstance"
  boolNotOp <- apply2 notOp boolType boolBitsInstance
  boolAndOp <- apply2 andOp boolType boolBitsInstance

  -- Signed integer bit operations.

  signedBitsInstance <- getBuiltin "signedBitsInstance"
  signed32BitsInstance <- scApply sc signedBitsInstance int32
  signed64BitsInstance <- scApply sc signedBitsInstance int64
  
  let getBitsOp nm = do
        op <- getBuiltin nm
        op32 <- apply2 op signed32 signed32BitsInstance
        op64 <- apply2 op signed64 signed64BitsInstance
        return (op32, op64)

  (signed32AndOp, signed64AndOp) <- getBitsOp "and"
  (signed32OrOp,  signed64OrOp)  <- getBitsOp "or"
  (signed32XorOp, signed64XorOp) <- getBitsOp "xor"
  (signed32ShlOp, signed64ShlOp) <- getBitsOp "shl"
  (signed32ShrOp, signed64ShrOp) <- getBitsOp "shr"
  signedUshrOp  <- getBuiltin "signedUShr"
  signed32UshrOp <- apply2 signedUshrOp int32 atomicProof
  signed64UshrOp <- apply2 signedUshrOp int32 atomicProof

  -- Signed arithmetic operations.
  signedNumInstance <- getBuiltin "signedNumInstance"

  signed32NumInstance <- scApply sc signedNumInstance int32
  signed64NumInstance <- scApply sc signedNumInstance int64

  let getNumOp nm = do
        op <- getBuiltin nm
        op32 <- apply2 op signed32 signed32NumInstance
        op64 <- apply2 op signed64 signed64NumInstance
        return (op32, op64)

  (signed32NegOp, signed64NegOp) <- getNumOp "neg"
  (signed32AddOp, signed64AddOp) <- getNumOp "add"
  (signed32SubOp, signed64SubOp) <- getNumOp "sub"
  (signed32MulOp, signed64MulOp) <- getNumOp "mul"
  (signed32DivOp, signed64DivOp) <- getNumOp "div"
  (signed32RemOp, signed64RemOp) <- getNumOp "rem"

  replicateOp <- getBuiltin "replicate"

  let applyDiv op x y = apply3 op x y atomicProof
  let mkArray eltType eltValue l = do
        il <- scApply sc signed32ToInt l
        Just <$> apply4 replicateOp il atomicProof eltType eltValue

  getOp <- getBuiltin "get"
  let getArray eltType l a i = do
        lint <- scApply sc signed32ToInt l
        iint <- scApply sc signed32ToInt i
        scApplyAll sc getOp [lint,eltType,a,iint,atomicProof]

  setOp <- getBuiltin "set"
  let setArray eltType l a i v = do
        lint <- scApply sc signed32ToInt l
        iint <- scApply sc signed32ToInt i
        scApplyAll sc setOp [lint,eltType,a,iint,atomicProof,v]
  return Backend { freshByte = scFreshGlobal sc "_" signed8
                 , freshInt  = scFreshGlobal sc "_" signed32
                 , freshLong = scFreshGlobal sc "_" signed64
                 , asBool = R.asBool
                 , asInt  = \t -> fmap fromIntegral $ R.asNatLit t -- Maybe Int32
                 , asLong = \t -> fmap fromIntegral $ R.asNatLit t -- Maybe Int64
                 , termBool = \b -> return $ if b then trueCtor else falseCtor
                 , termInt  = \w -> scApply sc intToSigned32 =<< scNat sc (fromIntegral w)
                 , termLong = \w -> scApply sc intToSigned64 =<< scNat sc (fromIntegral w)
                 , termByteFromInt = scApply sc byteFromIntFn 
                 , termLongFromInt = scApply sc longFromIntFn
                 , termIntFromLong = scApply sc intFromLongFn

                 , termNot   = scApply sc boolNotOp
                 , termAnd   = apply2 boolAndOp

                 , termEq    = \x y -> do
                     xTp <- scTypeOf sc x
                     apply3 eqOp xTp x y
                 , termIte   = \b x y -> do
                     tp <- scTypeOf sc x
                     apply4 iteOp tp b x y
                 , termILeq  = apply2 signed32LeqOp

                 , termIAnd  = apply2 signed32AndOp
                 , termIOr   = apply2 signed32OrOp
                 , termIXor  = apply2 signed32XorOp
                 , termIShl  = apply2 signed32ShlOp
                 , termIShr  = apply2 signed32ShrOp
                 , termIUshr = apply2 signed32UshrOp

                 , termINeg  = scApply sc signed32NegOp
                 , termIAdd  = apply2 signed32AddOp
                 , termISub  = apply2 signed32SubOp
                 , termIMul  = apply2 signed32MulOp
                 , termIDiv  = applyDiv signed32DivOp
                 , termIRem  = applyDiv signed32RemOp

                 , termLCompare = apply2 longCompare
                 , termLAnd  = apply2 signed64AndOp
                 , termLOr   = apply2 signed64OrOp
                 , termLXor  = apply2 signed64XorOp
                 , termLShl  = apply2 signed64ShlOp
                 , termLShr  = apply2 signed64ShrOp
                 , termLUshr = apply2 signed64UshrOp

                 , termLNeg  = scApply sc signed64NegOp
                 , termLAdd  = apply2 signed64AddOp
                 , termLSub  = apply2 signed64SubOp
                 , termLMul  = apply2 signed64MulOp
                 , termLDiv  = applyDiv signed64DivOp
                 , termLRem  = applyDiv signed64RemOp

                 , termIntArray  = mkArray signed32 signed32_0
                 , termLongArray = mkArray signed64 signed64_0
                 , termGetIntArray    = getArray signed32
                 , termGetLongArray   = getArray signed64
                 , termSetIntArray    = setArray signed32
                 , termSetLongArray   = setArray signed64
                 , blastTerm          = \_ -> return Nothing
                 , evalAigIntegral    = \_ _ _ -> undefined
                 , evalAigArray       = \_ _ _ -> undefined
                 , writeAigToFile     = \_ _ -> undefined
                 , writeCnfToFile     = \_ _ -> undefined
                 , getVarLit          = \_ -> undefined
                 , satTerm            = error "satTerm unimplemented"
                 , prettyTermD        = error "prettyTermD unimplemented"
                 }
