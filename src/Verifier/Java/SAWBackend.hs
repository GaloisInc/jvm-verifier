{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
module Verifier.Java.SAWBackend
  ( mkDagEngine
  , sawBackend
  ) where

import Control.Applicative

import Verifier.SAW
import Verifier.Java.Backend

instance PrettyTerm (DagTerm s) where
instance Show (DagTerm s) where
instance Typeable (DagTerm s) where

instance AigOps (DagEngine s) where

type instance MonadTerm (DagEngine s) = DagTerm s

sawBackend :: DagEngine s -> IO (Backend (DagEngine s))
sawBackend de = do
  let ?de = de
  int8  <- deInteger  8
  int32 <- deInteger 32
  int64 <- deInteger 64
  signed8  <- deGroundSignedType  8
  signed32 <- deGroundSignedType 32
  signed64 <- deGroundSignedType 64
  trueCtor <- deBuiltin TrueCtor
  falseCtor <- deBuiltin FalseCtor
  intFn  <- deGroundSignedValueFn 32
  longFn <- deGroundSignedValueFn 64
  Just s2sFn <- deGlobal signedToSignedIdent
  atomicProof <- deBuiltin AtomicProof
  byteFromIntFn <- deApplyAll s2sFn [ int32, int8, atomicProof ]
  longFromIntFn <- deApplyAll s2sFn [ int32, int64, atomicProof ]
  intFromLongFn <- deApplyAll s2sFn [ int64, int32, atomicProof ]
  let fAsBool :: TermF (DagTerm s) -> Maybe Bool
      fAsBool (BuiltinLit TrueCtor)  = Just True
      fAsBool (BuiltinLit FalseCtor) = Just False
      fAsBool _ = Nothing
  let fAsNum :: Num i => TermF (DagTerm s) -> Maybe i
      fAsNum (IntegerLit i) = Just (fromInteger i)
      fAsNum _ = Nothing
  return Backend { freshByte = deFreshGlobal "_" signed8
                 , freshInt  = deFreshGlobal "_" signed32
                 , freshLong = deFreshGlobal "_" signed64
                 , asBool = \t -> fAsBool $ deProject t
                 , asInt  = \t -> fAsNum  $ deProject t
                 , asLong = \t -> fAsNum  $ deProject t
                 , termBool = \b -> return $ if b then trueCtor else falseCtor
                 , termInt = intFn . toInteger
                 , termLong = longFn . toInteger
                 , termByteFromInt = deApply byteFromIntFn 
                 , termLongFromInt = deApply longFromIntFn
                 , termIntFromLong = deApply intFromLongFn
                 , termNot   = \_ ->   undefined
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