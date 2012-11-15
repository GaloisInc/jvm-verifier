{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.Java.SAWBackend
  ( mkSharedContext
  , sawBackend
  , withFreshBackend
  ) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map

import Verifier.SAW
import Verifier.Java.Backend

instance PrettyTerm (SharedTerm s) where
instance Show (SharedTerm s) where
instance Typeable (SharedTerm s) where

instance AigOps (SharedContext s) where

type instance MonadTerm (SharedContext s) = SharedTerm s

withFreshBackend :: (Backend (SharedContext s) -> IO a) -> IO a
withFreshBackend _f = undefined

sawBackend :: SharedContext s
              -- Maps symbol names to the term.
              -- The term names are defined in SAW.types 
           -> Map String (SharedTerm s)
           -> IO (Backend (SharedContext s))
sawBackend sc preludeSyms = do
  let ?sc = sc
  let apply2 op x y   = scApplyAll op [x,y]
      apply3 op x y z = scApplyAll op [x,y,z]
      apply4 op w x y z = scApplyAll op [w,x,y,z]
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

  int0  <- scInteger 0
  int8  <- scInteger 8
  int32 <- scInteger 32
  int64 <- scInteger 64

  signedType <- getBuiltin "Signed"
  signed8  <- scApply signedType int8
  signed32 <- scApply signedType int32
  signed64 <- scApply signedType int64

  signedToInt <- getBuiltin "signedToInteger"
  signed32ToInt <- scApply signedToInt int32

  intToSigned <- getBuiltin "integerToSigned"
  intToSigned32 <- scApply intToSigned int32
  intToSigned64 <- scApply intToSigned int64

  signed32_0 <- scApply intToSigned32 int0
  signed64_0 <- scApply intToSigned64 int0

  resizeSignedOp <- getBuiltin "resizeSigned"
  byteFromIntFn <- apply3 resizeSignedOp int32 int8  atomicProof
  longFromIntFn <- apply3 resizeSignedOp int32 int64 atomicProof
  intFromLongFn <- apply3 resizeSignedOp int64 int32 atomicProof
  
  -- Ordering operations.

  leqOp <- getBuiltin "leq"
 
  signedOrdInstance <- getBuiltin "signedOrdInstance"
  signed32OrdInstance <- scApply signedOrdInstance int32  
 
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
  signed32BitsInstance <- scApply signedBitsInstance int32
  signed64BitsInstance <- scApply signedBitsInstance int64
  
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

  signed32NumInstance <- scApply signedNumInstance int32
  signed64NumInstance <- scApply signedNumInstance int64

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
        il <- scApply signed32ToInt l
        Just <$> apply4 replicateOp il atomicProof eltType eltValue

  getOp <- getBuiltin "get"
  let getArray eltType l a i = do
        lint <- scApply signed32ToInt l
        iint <- scApply signed32ToInt i
        scApplyAll getOp [lint,eltType,a,iint,atomicProof]

  setOp <- getBuiltin "set"
  let setArray eltType l a i v = do
        lint <- scApply signed32ToInt l
        iint <- scApply signed32ToInt i
        scApplyAll setOp [lint,eltType,a,iint,atomicProof,v]
  return Backend { freshByte = scFreshGlobal (mkIdent "_") signed8
                 , freshInt  = scFreshGlobal (mkIdent "_") signed32
                 , freshLong = scFreshGlobal (mkIdent "_") signed64
                 , asBool = scViewAsBool
                 , asInt  = \t -> fmap fromIntegral $ scViewAsNum t
                 , asLong = \t -> fmap fromIntegral $ scViewAsNum t
                 , termBool = \b -> return $ if b then trueCtor else falseCtor
                 , termInt  = \w -> scApply intToSigned32 =<< scInteger (toInteger w)
                 , termLong = \w -> scApply intToSigned64 =<< scInteger (toInteger w)
                 , termByteFromInt = scApply byteFromIntFn 
                 , termLongFromInt = scApply longFromIntFn
                 , termIntFromLong = scApply intFromLongFn

                 , termNot   = scApply boolNotOp
                 , termAnd   = apply2 boolAndOp

                 , termEq    = \x y -> do
                     xTp <- scTypeOf x
                     apply3 eqOp xTp x y
                 , termIte   = \b x y -> do
                     tp <- scTypeOf x
                     apply4 iteOp tp b x y
                 , termILeq  = apply2 signed32LeqOp

                 , termIAnd  = apply2 signed32AndOp
                 , termIOr   = apply2 signed32OrOp
                 , termIXor  = apply2 signed32XorOp
                 , termIShl  = apply2 signed32ShlOp
                 , termIShr  = apply2 signed32ShrOp
                 , termIUshr = apply2 signed32UshrOp

                 , termINeg  = scApply signed32NegOp
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

                 , termLNeg  = scApply signed64NegOp
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
                 , blastTerm          = \_ -> undefined
                 , evalAigIntegral    = \_ _ _ -> undefined
                 , evalAigArray       = \_ _ _ -> undefined
                 , writeAigToFile     = \_ _ -> undefined
                 , getVarLit          = \_ -> undefined
                 }