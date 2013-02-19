{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix, jstanley
-}

module Execution.Stepper (step) where

import Control.Monad
import Control.Monad.Trans (liftIO)

import Data.Maybe

import Execution.JavaSemantics
import Verifier.Java.Codebase
import Verifier.Java.Simulator

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
  (`choice` createAndThrow "java/lang/ArrayStoreException") $
    arrayGuards arrayRef index
    ++
    [ isValidEltOfArray value arrayRef |-> do
        setArrayValue arrayRef index (RValue value)
        gotoNextInstruction
    ]

step Aconst_null = {-# SCC "Aconst_null" #-}  do
  pushValue . RValue =<< rNull
  gotoNextInstruction

step (Aload index) = {-# SCC "Aload" #-}  do
  pushValue =<< getLocal index
  gotoNextInstruction

step Areturn = {-# SCC "Areturn" #-}  execReturn . Just . RValue =<< rPop

step Arraylength = {-# SCC "Arraylength" #-}  do
  arrayRef <- rPop
  forkM (isNull arrayRef)
        (createAndThrow "java/lang/NullPointerException")
        (do iPush =<< arrayLength arrayRef
            gotoNextInstruction)

step (Astore index) = {-# SCC "Astore" #-}  do
  setLocal index =<< popValue
  gotoNextInstruction

step Athrow = {-# SCC "Athrow" #-}  do
  objectRef <- rPop
  forkM (isNull objectRef)
        (createAndThrow "java/lang/NullPointerException")
        (throw objectRef)

step Baload = {-# SCC "Baload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do pushArrayValue arrayRef index
             gotoNextInstruction)

step Bastore = {-# SCC "Bastore" #-}  do
  value <- iPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do fixedVal <- singleForkM (arrayRef `hasType` (ArrayType BooleanType))
                                     (boolFromInt value)
                                     (byteFromInt value)
             setArrayValue arrayRef index (IValue fixedVal)
             gotoNextInstruction)

step Caload = {-# SCC "Caload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do pushArrayValue arrayRef index
             gotoNextInstruction)

step Castore = {-# SCC "Castore" #-}  do
  value <- iPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do setArrayValue arrayRef index . IValue =<< charFromInt value
             gotoNextInstruction)

step (Checkcast tp) = {-# SCC "Checkcast" #-}  do
  objectRef <- rPop
  forkM (isNull objectRef ||| objectRef `hasType` tp)
        (do pushValue $ RValue objectRef
            gotoNextInstruction)
        (createAndThrow "java/lang/ClassCastException")

step D2f = {-# SCC "D2f" #-}  dPop >>= floatFromDouble >>= fPush >> gotoNextInstruction

step D2i = {-# SCC "D2i" #-}  dPop >>=   intFromDouble >>= iPush >> gotoNextInstruction

step D2l = {-# SCC "D2l" #-}  dPop >>=  longFromDouble >>= lPush >> gotoNextInstruction

step Dadd = {-# SCC "Dadd" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< dAdd value1 value2
  gotoNextInstruction

step Daload = {-# SCC "Daload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do pushArrayValue arrayRef index
             gotoNextInstruction)

step Dastore = {-# SCC "Dastore" #-}  do
  value <- dPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do setArrayValue arrayRef index (DValue value)
             gotoNextInstruction)

step Dcmpg = {-# SCC "Dcmpg" #-}  do
  value2 <- dPop
  value1 <- dPop
  iPush =<< value1 `dCmpg` value2
  gotoNextInstruction

step Dcmpl = {-# SCC "Dcmpl" #-}  do
  value2 <- dPop
  value1 <- dPop
  iPush =<< value1 `dCmpl` value2
  gotoNextInstruction

step Ddiv = {-# SCC "Ddiv" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< value1 `dDiv` value2
  gotoNextInstruction

step (Dload index) = {-# SCC "Dload" #-}  do
  pushValue =<< getLocal index
  gotoNextInstruction

step Dmul = {-# SCC "Dmul" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< value1 `dMul` value2
  gotoNextInstruction

step Dneg = {-# SCC "Dneg" #-}  dPop >>= dNeg >>= dPush >> gotoNextInstruction

step Drem = {-# SCC "Drem" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< value1 `dRem` value2
  gotoNextInstruction

step Dreturn = {-# SCC "Dreturn" #-}  execReturn . Just . DValue =<< dPop

step (Dstore index) = {-# SCC "Dstore" #-}  do
  setLocal index =<< popValue
  gotoNextInstruction

step Dsub = {-# SCC "Dsub" #-}  do
  value2 <- dPop
  value1 <- dPop
  dPush =<< value1 `dSub` value2
  gotoNextInstruction

step Dup = {-# SCC "Dup" #-}  do
  value <- popType1
  pushValue value
  pushValue value
  gotoNextInstruction

step Dup_x1 = {-# SCC "Dup_x1" #-}  do
  value1 <- popType1
  value2 <- popType1
  pushValue value1
  pushValue value2
  pushValue value1
  gotoNextInstruction

step Dup_x2 = {-# SCC "Dup_x2" #-}  do
  value1 <- popType1
  value2 <- popType2
  pushValue value1
  pushValues value2
  pushValue value1
  gotoNextInstruction

step Dup2 = {-# SCC "Dup2" #-}  do
  value <- popType2
  pushValues value
  pushValues value
  gotoNextInstruction

step Dup2_x1 = {-# SCC "Dup2_x1" #-}  do
  value1 <- popType2
  value2 <- popType1
  pushValues value1
  pushValue value2
  pushValues value1
  gotoNextInstruction

step Dup2_x2 = {-# SCC "Dup2_x2" #-}  do
  value1 <- popType2
  value2 <- popType2
  pushValues value1
  pushValues value2
  pushValues value1
  gotoNextInstruction

step F2d = {-# SCC "F2d" #-}  fPop >>= doubleFromFloat >>= dPush >> gotoNextInstruction

step F2i = {-# SCC "F2i" #-}  fPop >>=    intFromFloat >>= iPush >> gotoNextInstruction

step F2l = {-# SCC "F2l" #-}  fPop >>=   longFromFloat >>= lPush >> gotoNextInstruction

step Fadd = {-# SCC "Fadd" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< fAdd value1 value2
  gotoNextInstruction

step Faload = {-# SCC "Faload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do pushArrayValue arrayRef index
             gotoNextInstruction)

step Fastore = {-# SCC "Fastore" #-}  do
  value <- fPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do setArrayValue arrayRef index (FValue value)
             gotoNextInstruction)

step Fcmpg = {-# SCC "Fcmpg" #-}  do
  value2 <- fPop
  value1 <- fPop
  iPush =<< value1 `fCmpg` value2
  gotoNextInstruction

step Fcmpl = {-# SCC "Fcmpl" #-}  do
  value2 <- fPop
  value1 <- fPop
  iPush =<< value1 `fCmpl` value2
  gotoNextInstruction

step Fdiv = {-# SCC "Fdiv" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< value1 `fDiv` value2
  gotoNextInstruction

step (Fload index) = {-# SCC "Fload" #-}  do
  pushValue =<< getLocal index
  gotoNextInstruction

step Fmul = {-# SCC "Fmul" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< value1 `fMul` value2
  gotoNextInstruction

step Fneg = {-# SCC "Fneg" #-}  fPop >>= fNeg >>= fPush >> gotoNextInstruction

step Frem = {-# SCC "Frem" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< value1 `fRem` value2
  gotoNextInstruction

step Freturn = {-# SCC "Freturn" #-}  execReturn . Just . FValue =<< fPop

step (Fstore index) = {-# SCC "Fstore" #-}  do
  setLocal index =<< popValue
  gotoNextInstruction

step Fsub = {-# SCC "Fsub" #-}  do
  value2 <- fPop
  value1 <- fPop
  fPush =<< value1 `fSub` value2
  gotoNextInstruction

step (Getfield fldId) = {-# SCC "Getfield" #-}  do
  objectRef <- rPop
  cb <- getCodebase
  forkM (isNull objectRef)
        (createAndThrow "java/lang/NullPointerException")
        (do pushInstanceFieldValue objectRef =<< liftIO (locateField cb fldId)
            gotoNextInstruction
        )

step (Getstatic fieldId) = {-# SCC "Getstatic" #-}  do
  initializeClass $ fieldIdClass fieldId
  pushStaticFieldValue fieldId
  gotoNextInstruction

step (Goto addr) = {-# SCC "Goto" #-}  setPc addr

step I2b = {-# SCC "I2b" #-}  iPop >>=   byteFromInt >>= iPush >> gotoNextInstruction

step I2c = {-# SCC "I2c" #-}  iPop >>=   charFromInt >>= iPush >> gotoNextInstruction

step I2d = {-# SCC "I2d" #-}  iPop >>= doubleFromInt >>= dPush >> gotoNextInstruction

step I2f = {-# SCC "I2f" #-}  iPop >>=  floatFromInt >>= fPush >> gotoNextInstruction

step I2l = {-# SCC "I2l" #-}  iPop >>=   longFromInt >>= lPush >> gotoNextInstruction

step I2s = {-# SCC "I2s" #-}  iPop >>=  shortFromInt >>= iPush >> gotoNextInstruction

step Iadd = {-# SCC "Iadd" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iAdd` value2
  gotoNextInstruction

step Iaload = {-# SCC "Iaload" #-}  do
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do pushArrayValue arrayRef index
             gotoNextInstruction)

step Iand = {-# SCC "Iand" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iAnd` value2
  gotoNextInstruction

step Iastore = {-# SCC "Iastore" #-}  do
  value <- iPop
  index <- iPop
  arrayRef <- rPop
  guardArray arrayRef index
         (do setArrayValue arrayRef index (IValue value)
             gotoNextInstruction)

step Idiv = {-# SCC "Idiv" #-}  do
  value2 <- iPop
  value1 <- iPop
  zero   <- iConst 0
  forkM (value2 `iEq` zero)
        (createAndThrow "java/lang/ArithmeticException")
        (do iPush =<< value1 `iDiv` value2
            gotoNextInstruction)

step (If_acmpeq target) = {-# SCC "If_acmpeq" #-}  do
  value2 <- rPop
  value1 <- rPop
  forkM (value1 `rEq` value2)
        (setPc target)
        (gotoNextInstruction)

step (If_acmpne target) = {-# SCC "If_acmpne" #-}  do
  value2 <- rPop
  value1 <- rPop
  forkM (value1 `rEq` value2)
        (gotoNextInstruction)
        (setPc target)

step (If_icmpeq target) = {-# SCC "If_icmpeq" #-}  do
  value2 <- iPop
  value1 <- iPop
  forkM (value1 `iEq` value2)
        (setPc target)
        (gotoNextInstruction)

step (If_icmpne target) = {-# SCC "If_icmpne" #-}  do
  value2 <- iPop
  value1 <- iPop
  forkM (value1 `iEq` value2)
        (gotoNextInstruction)
        (setPc target)

step (If_icmplt target) = {-# SCC "If_icmplt" #-}  do
  value2 <- iPop
  value1 <- iPop
  forkM (value1 `iLt` value2)
        (setPc target)
        (gotoNextInstruction)

step (If_icmpge target) = {-# SCC "If_icmpge" #-}  do
  value2 <- iPop
  value1 <- iPop
  forkM (value2 `iLeq` value1)
        (setPc target)
        (gotoNextInstruction)

step (If_icmpgt target) = {-# SCC "If_icmpgt" #-}  do
  value2 <- iPop
  value1 <- iPop
  forkM (value2 `iLt` value1)
        (setPc target)
        (gotoNextInstruction)

step (If_icmple target) = {-# SCC "If_icmple" #-}  do
  value2 <- iPop
  value1 <- iPop
  forkM (value1 `iLeq` value2)
        (setPc target)
        (gotoNextInstruction)

step (Ifeq target) = {-# SCC "Ifeq" #-}  do
  value <- iPop
  zero <- iConst 0
  forkM (value `iEq` zero)
        (setPc target)
        (gotoNextInstruction)

step (Ifne target) = {-# SCC "Ifne" #-}  do
  value <- iPop
  zero <- iConst 0
  forkM (value `iEq` zero)
        (gotoNextInstruction)
        (setPc target)

step (Iflt target) = {-# SCC "Iflt" #-}  do
  value <- iPop
  zero <- iConst 0
  forkM (value `iLt` zero)
        (setPc target)
        (gotoNextInstruction)

step (Ifge target) = {-# SCC "Ifge" #-}  do
  value <- iPop
  zero <- iConst 0
  forkM (zero `iLeq` value)
        (setPc target)
        (gotoNextInstruction)

step (Ifgt target) = {-# SCC "Ifgt" #-}  do
  value <- iPop
  zero <- iConst 0
  forkM (zero `iLt` value)
        (setPc target)
        (gotoNextInstruction)

step (Ifle target) = {-# SCC "Ifle" #-}  do
  value <- iPop
  zero <- iConst 0
  forkM (value `iLeq` zero)
        (setPc target)
        (gotoNextInstruction)

step (Ifnonnull target) = {-# SCC "Ifnonnull" #-}  do
  value <- rPop
  forkM (isNull value)
        (gotoNextInstruction)
        (setPc target)

step (Ifnull target) = {-# SCC "Ifnull" #-}  do
  value <- rPop
  forkM (isNull value)
        (setPc target)
        (gotoNextInstruction)

step (Iinc index constant) = {-# SCC "Iinc" #-}  do
  IValue value <- getLocal index
  constValue   <- iConst (fromIntegral constant)
  setLocal index . IValue =<< value `iAdd` constValue
  gotoNextInstruction

step (Iload index) = {-# SCC "Iload" #-}  do
  pushValue =<< getLocal index
  gotoNextInstruction

step Imul = {-# SCC "Imul" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iMul` value2
  gotoNextInstruction

step Ineg = {-# SCC "Ineg" #-}  iPop >>= iNeg >>= iPush

step (Instanceof tp) = {-# SCC "Instanceof" #-}  do
  objectRef <- rPop
  instanceOf objectRef

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

step Ior = {-# SCC "Ior" #-}  do
  value2 <- iPop
  value1 <- iPop
  iPush =<< value1 `iOr` value2

step Irem = {-# SCC "Irem" #-}  do
  value2 <- iPop
  value1 <- iPop
  whenM (iEq value2 =<< iConst 0)
    $ createAndThrow "java/lang/ArithmeticException"
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
  whenM (lEq value2 =<< lConst 0)
    $ createAndThrow "java/lang/ArithmeticException"
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
  whenM (lEq value2 =<< lConst 0)
    $ createAndThrow "java/lang/ArithmeticException"
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
    whenM (count `iLt` zero)
      $ createAndThrow "java/lang/NegativeArraySizeException"
  pushValue . RValue =<< newMultiArray arrayType counts
  
step (New name) = {-# SCC "New" #-}  do
   pushValue . RValue =<< newObject name

step (Newarray arrayType) = {-# SCC "Newarray" #-}  do
  count <- iPop
  zero <- iConst 0
  whenM (count `iLt` zero)
    $ createAndThrow "java/lang/NegativeArraySizeException"
  pushValue . RValue =<< newMultiArray arrayType [count]

step Nop = {-# SCC "Nop" #-}  return ()

step Pop  = {-# SCC "Pop" #-}  popType1

step Pop2 = {-# SCC "Pop2" #-}  popType2

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

step (Ret index) = {-# SCC "Ret" #-}  do
  AValue newPc <- getLocal index
  setPc newPc

step Return = {-# SCC "Return" #-}  execReturn Nothing

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
