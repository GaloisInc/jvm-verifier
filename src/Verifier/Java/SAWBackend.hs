{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.Java.SAWBackend
  ( sawBackend
  ) where

import Verifier.SAW
import Verifier.Java.Backend

type instance MonadTerm (DagEngine s) = DagTerm s

sawBackend :: DagEngine s -> IO (Backend (DagEngine s))
sawBackend de = do
  let ?de = de
  i8  <- deGroundSignedType  8
  i32 <- deGroundSignedType 32
  i64 <- deGroundSignedType 64
  return Backend { freshByte = deFreshGlobal  i8
                 , freshInt  = deFreshGlobal i32
                 , freshLong = deFreshGlobal i64
                 , asBool = \_t -> undefined
                 , asInt = \_ -> undefined
                 , asLong = \_ -> undefined
                 , termBool = \_ -> undefined
                 , termInt = \_ -> undefined
                 , termLong = \_ -> undefined
                 , termByteFromInt = \_ -> undefined
                 , termLongFromInt = \_ -> undefined
                 , termIntFromLong = \_ -> undefined
                 , termNot   = \_ -> undefined
                 , termAnd   = \_ _ -> undefined
                 , termEq    = \_ _ -> undefined
                 , termIte   = \_ _ _ -> undefined
                 , termIAnd  = \_ _ -> undefined
                 , termIOr   = \_ _ -> undefined
                 , termIXor  = \_ _ -> undefined
                 , termIShl  = \_ _ -> undefined
                 , termIShr  = \_ _ -> undefined
                 , termIUshr = \_ _ -> undefined
                 , termLShl  = \_ _ -> undefined
                 , termLShr  = \_ _ -> undefined
                 , termLUshr = \_ _ -> undefined
                 , termLeq   = \_ _ -> undefined
                 , termLt    = \_ _ -> undefined
                 , termNeg   = \_ -> undefined
                 , termAdd   = \_ _ -> undefined
                 , termSub   = \_ _ -> undefined
                 , termMul   = \_ _ -> undefined
                 , termDiv   = \_ _ -> undefined
                 , termRem   = \_ _ -> undefined
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