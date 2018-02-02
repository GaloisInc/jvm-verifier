{- |
Module           : $Header$
Description      : JVM instruction semantics
License          : BSD3
Stability        : stable
Point-of-contact : jhendrix, acfoltzer

This module defines the semantics of individual JVM instructions in
terms of the 'Execution.JavaSemantics.JavaSemantics' operators. We
assume our input has been translated into the @Data.JVM.Symbolic@ AST,
and so the instructions that affect control flow are only partially
implemented here.
-}

{-# LANGUAGE OverloadedStrings #-}

module Execution.Stepper (step) where

import Control.Monad
import Control.Monad.Trans (liftIO)

import Text.PrettyPrint ()

import Execution.JavaSemantics
import Verifier.Java.Codebase

-- -- Step function {{{1

-- | Execute a single instruction
step :: JavaSemantics m => Instruction -> m ()

step Aaload = {-# SCC "Aaload" #-}  do
  index    <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  pushArrayValue arrayRef index

step Aastore = {-# SCC "Aastore" #-}  do
  value    <- rPop
  index    <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  assertTrueM (isValidEltOfArray value arrayRef) "java/lang/ArrayStoreException"
  setArrayValue arrayRef index (RValue value)

step Aconst_null = {-# SCC "Aconst_null" #-}  do
  pushValue . RValue =<< rNull

step (Aload index) = {-# SCC "Aload" #-}  do
  pushValue =<< getLocal index

step Arraylength = {-# SCC "Arraylength" #-}  do
  arrayRef <- rPop
  throwIfRefNull arrayRef
  iPush =<< arrayLength arrayRef

step (Astore index) = {-# SCC "Astore" #-}  do
  setLocal index =<< popValue

step Athrow = {-# SCC "Athrow" #-}  do
  objectRef <- rPop
  throwIfRefNull objectRef
  throw objectRef

step Baload = {-# SCC "Baload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  pushArrayValue arrayRef index

step Bastore = {-# SCC "Bastore" #-}  do
  value <- iPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  fixedVal <- byteArrayVal arrayRef value
  setArrayValue arrayRef index (IValue fixedVal)

step Caload = {-# SCC "Caload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  pushArrayValue arrayRef index

step Castore = {-# SCC "Castore" #-}  do
  value <- iPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  setArrayValue arrayRef index . IValue =<< charFromInt value

step (Checkcast tp) = {-# SCC "Checkcast" #-}  do
  objectRef <- rPop
  assertTrueM (isNull objectRef ||| objectRef `hasType` tp)
    "java/lang/ClassCastException"
  pushValue $ RValue objectRef

step D2f = {-# SCC "D2f" #-}  dPop >>= floatFromDouble >>= fPush

step D2i = {-# SCC "D2i" #-}  dPop >>=   intFromDouble >>= iPush

step D2l = {-# SCC "D2l" #-}  dPop >>=  longFromDouble >>= lPush

step Dadd = {-# SCC "Dadd" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< dAdd value1 value2

step Daload = {-# SCC "Daload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  pushArrayValue arrayRef index

step Dastore = {-# SCC "Dastore" #-}  do
  value <- dPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  setArrayValue arrayRef index (DValue value)

step Dcmpg = {-# SCC "Dcmpg" #-}  do
  value2 <- dPop
  value1 <- dPop
  iPush =<< value1 `dCmpg` value2

step Dcmpl = {-# SCC "Dcmpl" #-}  do
  value2 <- dPop
  value1 <- dPop
  iPush =<< value1 `dCmpl` value2

step Ddiv = {-# SCC "Ddiv" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< value1 `dDiv` value2

step (Dload index) = {-# SCC "Dload" #-}  do
  pushValue =<< getLocal index

step Dmul = {-# SCC "Dmul" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< value1 `dMul` value2

step Dneg = {-# SCC "Dneg" #-}  dPop >>= dNeg >>= dPush

step Drem = {-# SCC "Drem" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< value1 `dRem` value2

step (Dstore index) = {-# SCC "Dstore" #-}  do
  setLocal index =<< popValue

step Dsub = {-# SCC "Dsub" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< value1 `dSub` value2

step Dup = {-# SCC "Dup" #-}  do
  value <- popType1
  pushValue value
  pushValue value

step Dup_x1 = {-# SCC "Dup_x1" #-}  do
  value1 <- popType1
  value2 <- popType1
  pushValue value1
  pushValue value2
  pushValue value1

step Dup_x2 = {-# SCC "Dup_x2" #-}  do
  value1 <- popType1
  value2 <- popType2
  pushValue value1
  pushValues value2
  pushValue value1

step Dup2 = {-# SCC "Dup2" #-}  do
  value <- popType2
  pushValues value
  pushValues value

step Dup2_x1 = {-# SCC "Dup2_x1" #-}  do
  value1 <- popType2
  value2 <- popType1
  pushValues value1
  pushValue value2
  pushValues value1

step Dup2_x2 = {-# SCC "Dup2_x2" #-}  do
  value1 <- popType2
  value2 <- popType2
  pushValues value1
  pushValues value2
  pushValues value1

step F2d = {-# SCC "F2d" #-}  fPop >>= doubleFromFloat >>= dPush

step F2i = {-# SCC "F2i" #-}  fPop >>=    intFromFloat >>= iPush

step F2l = {-# SCC "F2l" #-}  fPop >>=   longFromFloat >>= lPush

step Fadd = {-# SCC "Fadd" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< fAdd value1 value2

step Faload = {-# SCC "Faload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  pushArrayValue arrayRef index

step Fastore = {-# SCC "Fastore" #-}  do
  value <- fPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  setArrayValue arrayRef index (FValue value)

step Fcmpg = {-# SCC "Fcmpg" #-}  do
  value2 <- fPop
  value1 <- fPop
  iPush =<< value1 `fCmpg` value2

step Fcmpl = {-# SCC "Fcmpl" #-}  do
  value2 <- fPop
  value1 <- fPop
  iPush =<< value1 `fCmpl` value2

step Fdiv = {-# SCC "Fdiv" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< value1 `fDiv` value2

step (Fload index) = {-# SCC "Fload" #-}  do
  pushValue =<< getLocal index

step Fmul = {-# SCC "Fmul" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< value1 `fMul` value2

step Fneg = {-# SCC "Fneg" #-}  fPop >>= fNeg >>= fPush

step Frem = {-# SCC "Frem" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< value1 `fRem` value2

step (Fstore index) = {-# SCC "Fstore" #-}  do
  setLocal index =<< popValue

step Fsub = {-# SCC "Fsub" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< value1 `fSub` value2

step (Getfield fldId) = {-# SCC "Getfield" #-}  do
  objectRef <- rPop
  throwIfRefNull objectRef
  cb <- getCodebase
  pushInstanceFieldValue objectRef =<< liftIO (locateField cb fldId)


step (Getstatic fieldId) = {-# SCC "Getstatic" #-}  do
  initializeClass $ fieldIdClass fieldId
  pushStaticFieldValue fieldId

step I2b = {-# SCC "I2b" #-}  iPop >>=   byteFromInt >>= iPush

step I2c = {-# SCC "I2c" #-}  iPop >>=   charFromInt >>= iPush

step I2d = {-# SCC "I2d" #-}  iPop >>= doubleFromInt >>= dPush

step I2f = {-# SCC "I2f" #-}  iPop >>=  floatFromInt >>= fPush

step I2l = {-# SCC "I2l" #-}  iPop >>=   longFromInt >>= lPush

step I2s = {-# SCC "I2s" #-}  iPop >>=  shortFromInt >>= iPush

step Iadd = {-# SCC "Iadd" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iAdd` value2

step Iaload = {-# SCC "Iaload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  pushArrayValue arrayRef index

step Iand = {-# SCC "Iand" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iAnd` value2

step Iastore = {-# SCC "Iastore" #-}  do
  value <- iPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  setArrayValue arrayRef index (IValue value)

step Idiv = {-# SCC "Idiv" #-}  do
  value2 <- iPop
  value1 <- iPop
  zero   <- iConst 0
  assertFalseM (value2 `iEq` zero) "java/lang/ArithmeticException"
  iPush =<< value1 `iDiv` value2

step (Iinc index constant) = {-# SCC "Iinc" #-}  do
  IValue value <- getLocal index
  constValue   <- iConst (fromIntegral constant)
  setLocal index . IValue =<< value `iAdd` constValue

step (Iload index) = {-# SCC "Iload" #-}  do
  pushValue =<< getLocal index

step Imul = {-# SCC "Imul" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iMul` value2

step Ineg = {-# SCC "Ineg" #-}  iPop >>= iNeg >>= iPush

step (Instanceof tp) = {-# SCC "Instanceof" #-}  do
  objectRef <- rPop
  objectRef `instanceOf` tp

{-
step (Invokeinterface iName key) = {-# SCC "Invokeinterface" #-}  do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef   <- rPop
  dynBind iName key objectRef $ \iName' -> do
    gotoNextInstruction
    invokeInstanceMethod iName' key objectRef (reverse reverseArgs)

step (Invokespecial (ClassType methodClass) key) = {-# SCC "Invokespecial" #-}  do
  currentClassName <- getCurrentClassName
  reverseArgs      <- replicateM (length (methodKeyParameterTypes key)) popValue
  cb               <- getCodebase
  currentClass     <- liftIO $ lookupClass cb currentClassName
  objectRef        <- rPop
  objectRefAsCC    <- coerceRef objectRef $ ClassType (className currentClass)
  b                <- liftIO $ isStrictSuper cb methodClass currentClass
  let args          = reverse reverseArgs
      call cl       = do gotoNextInstruction
                         invokeInstanceMethod cl key objectRef args
  if classHasSuperAttribute currentClass && b && methodKeyName key /= "<init>"
    then do
      dynBind' methodClass key objectRef $ \cl ->
        objectRefAsCC `superHasType` cl |-> call cl
    else
      forkM (isNull objectRef)
            (createAndThrow "java/lang/NullPointerException")
            (call methodClass)

step (Invokespecial (ArrayType _methodType) key) = {-# SCC "Invokespecial" #-}  do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef <- rPop
  forkM (isNull objectRef)
        (createAndThrow "java/lang/NullPointerException")
        (do gotoNextInstruction
            invokeInstanceMethod "java/lang/Object" key objectRef $ reverse reverseArgs)

step (Invokespecial _ _) = error "internal: unexpected Invokespecial form"

-- Invoking virtual methods on an array type just reduces to object type.
step (Invokevirtual (ArrayType _methodType) key) = {-# SCC "Invokevirtual" #-}  do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef <- rPop
  forkM (isNull objectRef)
        (createAndThrow "java/lang/NullPointerException")
        (do gotoNextInstruction
            invokeInstanceMethod "java/lang/Object" key objectRef $ reverse reverseArgs)

step (Invokevirtual (ClassType cName) key) = {-# SCC "Invokevirtual" #-} do
  reverseArgs <- replicateM (length (methodKeyParameterTypes key)) popValue
  objectRef   <- rPop
  dynBind cName key objectRef $ \cName' -> do
    gotoNextInstruction
    invokeInstanceMethod cName' key objectRef (reverse reverseArgs)

step (Invokevirtual _ _) = error "internal: unexpected Invokevirtual form"
-}

step Ior = {-# SCC "Ior" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iOr` value2

step Irem = {-# SCC "Irem" #-}  do
  value2 <- iPop
  value1 <- iPop
  assertFalseM (iEq value2 =<< iConst 0) "java/lang/ArithmeticException"
  iPush =<< value1 `iRem` value2

step Ishl = {-# SCC "Ishl" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iShl` value2

step Ishr = {-# SCC "Ishr" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iShr` value2

step (Istore index) = {-# SCC "Istore" #-}  do
  setLocal index . IValue =<< iPop

step Isub = {-# SCC "Isub" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iSub` value2

step Iushr = {-# SCC "Iushr" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iUshr` value2

step Ixor = {-# SCC "Ixor" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iXor` value2

step L2d = {-# SCC "L2d" #-}  lPop >>= doubleFromLong >>= dPush

step L2f = {-# SCC "L2f" #-}  lPop >>=  floatFromLong >>= fPush

step L2i = {-# SCC "L2i" #-}  lPop >>=    intFromLong >>= iPush

step Ladd = {-# SCC "Ladd" #-}  do
  value2 <- lPop
  value1 <- lPop
  lPush =<< value1 `lAdd` value2

step Laload = {-# SCC "Laload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  pushArrayValue arrayRef index

step Land = {-# SCC "Land" #-}  do
  value2 <- lPop
  value1 <- lPop
  lPush =<< value1 `lAnd` value2

step Lastore = {-# SCC "Lastore" #-}  do
  value <- lPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  setArrayValue arrayRef index (LValue value)

step Lcmp = {-# SCC "Lcmp" #-}  do
  value2 <- lPop
  value1 <- lPop
  iPush =<< value1 `lCmp` value2

step (Ldc (Double v))  = {-# SCC "Ldc" #-}  dConst v >>= dPush
step (Ldc (Float v))   = {-# SCC "Ldc" #-}  fConst v >>= fPush
step (Ldc (Integer v)) = {-# SCC "Ldc" #-}  iConst v >>= iPush
step (Ldc (Long v))    = {-# SCC "Ldc" #-}  lConst v >>= lPush
step (Ldc (String v))  = {-# SCC "Ldc" #-}  do
  pushValue . RValue =<< refFromString v
step (Ldc (ClassRef c)) = {-# SCC "Ldc" #-} do
  pushValue . RValue =<< getClassObject c

step Ldiv = {-# SCC "Ldiv" #-}  do
  value2 <- lPop
  value1 <- lPop
  assertFalseM (lEq value2 =<< lConst 0) "java/lang/ArithmeticException"
  lPush =<< value1 `lDiv` value2

step (Lload index) = {-# SCC "Lload" #-}  do
  pushValue =<< getLocal index

step Lmul = {-# SCC "Lmul" #-}  do
  value2 <- lPop
  value1 <- lPop
  lPush =<< value1 `lMul` value2

step Lneg = {-# SCC "Lneg" #-}  do
  lPush =<< lNeg =<< lPop

step Lor = {-# SCC "Lor" #-}  do
  value2 <- lPop
  value1 <- lPop
  lPush =<< value1 `lOr` value2

step Lrem = {-# SCC "Lrem" #-}  do
  value2 <- lPop
  value1 <- lPop
  assertFalseM (lEq value2 =<< lConst 0) "java/lang/ArithmeticException"
  lPush =<< value1 `lRem` value2

step Lshl = {-# SCC "Lshl" #-}  do
  value2 <- longFromInt =<< iPop -- promote for lShl
  value1 <- lPop
  lPush =<< value1 `lShl` value2

step Lshr = {-# SCC "Lshr" #-}  do
  value2 <- longFromInt =<< iPop -- promote for lShr
  value1 <- lPop
  lPush =<< value1 `lShr` value2

step (Lstore index) = {-# SCC "Lstore" #-}  do
  setLocal index . LValue =<< lPop

step Lsub = {-# SCC "Lsub" #-}  do
  value2 <- lPop
  value1 <- lPop
  lPush =<< value1 `lSub` value2

step Lushr = {-# SCC "Lushr" #-}  do
  value2 <- longFromInt =<< iPop -- promote for lUshr
  value1 <- lPop
  lPush =<< value1 `lUshr` value2

step Lxor = {-# SCC "Lxor" #-}  do
  value2 <- lPop
  value1 <- lPop
  lPush =<< value1 `lXor` value2

step Monitorenter = {-# SCC "Monitorenter" #-} void rPop

step Monitorexit = {-# SCC "Monitorexit" #-} void rPop

step (Multianewarray arrayType dimensions) = {-# SCC "Multianewarray" #-}  do
  counts <- return . reverse =<< sequence (replicate (fromIntegral dimensions) iPop)
  zero <- iConst 0
  forM_ counts $ \count -> do
    assertFalseM (count `iLt` zero) "java/lang/NegativeArraySizeException"
  pushValue . RValue =<< newMultiArray arrayType counts

step (New name) = {-# SCC "New" #-}  do
   pushValue . RValue =<< newObject name

step (Newarray arrayType) = {-# SCC "Newarray" #-}  do
  count <- iPop
  zero <- iConst 0
  assertFalseM (count `iLt` zero) "java/lang/NegativeArraySizeException"
  pushValue . RValue =<< newMultiArray arrayType [count]

step Nop = {-# SCC "Nop" #-}  return ()

step Pop  = {-# SCC "Pop" #-}  void $ popType1

step Pop2 = {-# SCC "Pop2" #-}  void $ popType2

step (Putfield fldId) = {-# SCC "Putfield" #-}  do
  val <- popValue
  objectRef <- rPop
  throwIfRefNull objectRef
  cb <- getCodebase
  value <- case (fieldIdType fldId, val) of
              (BooleanType, IValue i) -> return . IValue =<< boolFromInt  i
              (ByteType,    IValue i) -> return . IValue =<< byteFromInt  i
              (CharType,    IValue i) -> return . IValue =<< charFromInt  i
              (ShortType,   IValue i) -> return . IValue =<< shortFromInt i
              _ -> return val
  fld <- liftIO $ locateField cb fldId
  setInstanceFieldValue objectRef fld value

step (Putstatic fieldId) = {-# SCC "Putstatic" #-}  do
  initializeClass $ fieldIdClass fieldId
  value <-
    case fieldIdType fieldId of
      BooleanType -> return . IValue =<<  boolFromInt =<< iPop
      ByteType    -> return . IValue =<<  byteFromInt =<< iPop
      CharType    -> return . IValue =<<  charFromInt =<< iPop
      ShortType   -> return . IValue =<< shortFromInt =<< iPop
      _           -> popValue
  setStaticFieldValue fieldId value

step (Ret _index) = {-# SCC "Ret" #-}  do
  warning "jsr/ret not implemented"
  return ()

step Return = {-# SCC "Return" #-}  return ()

step Saload = {-# SCC "Saload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  pushArrayValue arrayRef index

step Sastore = {-# SCC "Sastore" #-}  do
  value <- iPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
  setArrayValue arrayRef index . IValue =<< shortFromInt value

step Swap = {-# SCC "Swap" #-}  do
  value1 <- popType1
  value2 <- popType1
  pushValue value1
  pushValue value2

step inst = error $ "invalid instruction " ++ show inst
-- }}}1
