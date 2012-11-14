{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.Java.SAWBackend
  ( mkSharedContext
  , sawBackend
  ) where

import Control.Applicative

import Verifier.SAW
import Verifier.Java.Backend

instance PrettyTerm (SharedTerm s) where
instance Show (SharedTerm s) where
instance Typeable (SharedTerm s) where

instance AigOps (SharedContext s) where

type instance MonadTerm (SharedContext s) = SharedTerm s

sawBackend :: SharedContext s -> IO (Backend (SharedContext s))
sawBackend sc = do
  let ?sc = sc
  let apply2 op x y   = scApplyAll op [x,y]
      apply3 op x y z = scApplyAll op [x,y,z]
      apply4 op w x y z = scApplyAll op [w,x,y,z]
  boolType  <- scBuiltin BoolType
  trueCtor  <- scBuiltin TrueCtor
  falseCtor <- scBuiltin FalseCtor
  iteOp <- scBuiltin IteFn

  int8  <- scInteger  8
  int32 <- scInteger 32
  int64 <- scInteger 64

  signedType <- scBuiltin SignedType
  signed8  <- scApply signedType int8
  signed32 <- scApply signedType int32
  signed64 <- scApply signedType int64

  intToSigned <- scBuiltin IntegerToSigned
  intToSigned32 <- scApply intToSigned int32
  intToSigned64 <- scApply intToSigned int64

  atomicProof <- scBuiltin AtomicProof
  resizeSignedOp <- scBuiltin ResizeSigned
  byteFromIntFn <- apply3 resizeSignedOp int32 int8  atomicProof
  longFromIntFn <- apply3 resizeSignedOp int32 int64 atomicProof
  intFromLongFn <- apply3 resizeSignedOp int64 int32 atomicProof

  notOp <- scBuiltin NotFn
  andOp <- scBuiltin AndFn
  orOp  <- scBuiltin OrFn
  xorOp <- scBuiltin XorFn
  shlOp <- scBuiltin ShlFn
  shrOp <- scBuiltin ShrFn
  
  boolBitsInstance <- scBuiltin BoolBitsInstance
  boolNotOp <- apply2 notOp boolType boolBitsInstance
  boolAndOp <- apply2 andOp boolType boolBitsInstance
 
  signedOrdInstance <- scBuiltin SignedOrdInstance
  int32OrdInstance <- scApply signedOrdInstance int32  
 
  eqOp <- scBuiltin EqFn

  leqOp <- scBuiltin LeqFn
  int32LeqOp <- apply2 leqOp signed32 int32OrdInstance

  signedBitsInstance <- scBuiltin SignedBitsInstance
  int32BitsInstance <- scApply signedBitsInstance int32
  int64BitsInstance <- scApply signedBitsInstance int64

  int32AndOp <- apply2 andOp int32 int32BitsInstance
  int32OrOp  <- apply2 orOp  int32 int32BitsInstance
  int32XorOp <- apply2 xorOp int32 int32BitsInstance
  int32ShlOp <- apply2 shlOp int32 int32BitsInstance
  int32ShrOp <- apply2 shrOp int32 int32BitsInstance
 

  int64AndOp <- apply2 andOp int64 int64BitsInstance
  int64OrOp  <- apply2 orOp  int64 int64BitsInstance
  int64XorOp <- apply2 xorOp int64 int64BitsInstance

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
                 , termILeq  = apply2 int32LeqOp
                 , termIAnd  = apply2 int32AndOp
                 , termIOr   = apply2 int32OrOp
                 , termIXor  = apply2 int32XorOp
                 , termIShl  = apply2 int32ShlOp
                 , termIShr  = apply2 int32ShrOp
                 , termIUshr = \_ _ -> undefined
                 , termINeg  = \_   -> undefined
                 , termIAdd  = \_ _ -> undefined
                 , termISub  = \_ _ -> undefined
                 , termIMul  = \_ _ -> undefined
                 , termIDiv  = \_ _ -> undefined
                 , termIRem  = \_ _ -> undefined

                 , termLCompare = \_ _ -> undefined
                 , termLAnd  = \_ _ -> undefined
                 , termLOr   = \_ _ -> undefined
                 , termLXor  = \_ _ -> undefined
                 , termLShl  = \_ _ -> undefined
                 , termLShr  = \_ _ -> undefined
                 , termLUshr = \_ _ -> undefined
                 , termLNeg  = \_ -> undefined
                 , termLAdd  = \_ _ -> undefined
                 , termLSub  = \_ _ -> undefined
                 , termLMul  = \_ _ -> undefined
                 , termLDiv  = \_ _ -> undefined
                 , termLRem  = \_ _ -> undefined

                 , termIntArray  = \_ -> undefined
                 , termLongArray = \_ -> undefined
                 , applyGetArrayValue = \_ _ -> undefined
                 , applySetArrayValue = \_ _ _ -> undefined
                 , blastTerm          = \_ -> undefined
                 , evalAigIntegral    = \_ _ _ -> undefined
                 , evalAigArray       = \_ _ _ -> undefined
                 , writeAigToFile     = \_ _ -> undefined
                 , getVarLit          = \_ -> undefined
                 }