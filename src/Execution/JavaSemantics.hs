{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix, jstanley
-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}

module Execution.JavaSemantics where

import Control.Monad
import Control.Monad.State

import Data.Int
import Data.Maybe

import Execution.Codebase
import JavaParser

import System.IO.Unsafe (unsafePerformIO)
dbug :: String -> a -> a
dbug = seq . unsafePerformIO . putStrLn
-- dbug _ = id
dbugM :: Monad m => String -> m ()
dbugM s = dbug s $ return ()
-- dbugM _ = return ()
_nowarn_dbug :: IO ()
_nowarn_dbug = undefined dbug >> dbugM undefined

data AtomicValue double float int long ref
  = DValue double
  | FValue float
  | IValue { unIValue :: int }
  | LValue { unLValue :: long }
  | RValue { unRValue :: ref }
  | AValue PC

type family JSDouble (m :: * -> *)
type family JSFloat  (m :: * -> *)
type family JSInt    (m :: * -> *)
type family JSLong   (m :: * -> *)
type family JSRef    (m :: * -> *)
type family JSBool   (m :: * -> *)
type family JSRslt   (m :: * -> *)

type JSValue m = AtomicValue (JSDouble m) (JSFloat m) (JSInt m) (JSLong m) (JSRef m)


class (Functor m, MonadIO m) => HasCodebase m where
  getCodebase :: m Codebase

-- | This typeclass defines the underlying semantics for the parameterized JVM
-- step function.
class ( Monad m
      , MonadIO m
      , HasCodebase m
      , Functor m
      , Show (JSDouble m)
      , Show (JSFloat m)
      , Show (JSInt m)  -- , Integral int
      , Show (JSLong m) -- , Integral long
      , Show (JSRef m)
      , Show (JSBool m)
      , Show (JSRslt m)
      )
  => JavaSemantics m where
  -- Control related functions {{{1

  -- | Returns True if execution is finished.
  isFinished :: m Bool

  -- | Fork that executes true execution if condition is true, and false
  --   execution otherwise.  Yields one or more resulting executions (as it is
  --   at the discretion of instances if they want to consider more than one
  --   branch).

  fork :: JSBool m -- ^ condition
       -> m ()     -- ^ true execution
       -> m ()     -- ^ false execution
       -> m ()

  -- | Resolve the monadic condition to exactly one of its branches
  singleForkM :: m (JSBool m)
              -> m v
              -> m v
              -> m v

  -- | Returns the result of running the simulator
  getResult :: m (JSRslt m)

  -- | Returns name of current class in
  getCurrentClassName :: m String

  -- | Returns current method
  getCurrentMethod :: m Method

  -- | Returns current program counter value
  getPc :: m PC

  -- | Sets program counter in top frame.
  setPc :: PC -> m ()

  -- | Ensures class with given name is initialized.
  initializeClass :: String -> m ()

  -- | Pushes an instance method call.
  pushInstanceMethodCall :: String -> Method -> JSRef m -> [JSValue m] -> m ()

  -- | Runs an instance method call.
  runInstanceMethodCall :: String -> Method -> JSRef m -> [JSValue m] -> m ()

  -- | Pushes a static method call to the stack.
  pushStaticMethodCall :: String -> Method -> [JSValue m] -> m ()

  -- | Pops frame from stack and pushes value to operand stack.
  execReturn :: Maybe (JSValue m) -> m ()

  -- | Throws ref as an exception.
  --
  -- Requires that reference is non-null and an instance of @Exception@.
  throw :: JSRef m -> m ()

  -- | Reports a fatal error and halts execution
  fatal :: String -> m a

  -- | Negate a Boolean value
  bNot :: JSBool m -> m (JSBool m)

  -- | Returns logical and of inputs
  --
  -- Arguments are monads so that we can potentially short-circuit evaluation.
  (&&&) :: m (JSBool m) -> m (JSBool m) -> m (JSBool m)
  infixr 3 &&&

  -- | Returns logical or of inputs
  --
  -- Arguments are monads so that we can potentially short-circuit evaluation.
  (|||) :: m (JSBool m) -> m (JSBool m) -> m (JSBool m)
  infixr 2 |||
  mx ||| my = bNot =<< (bNot =<< mx) &&& (bNot =<< my)

  -- Conversion functions {{{1

  -- | Truncates double to float type.
  floatFromDouble :: JSDouble m -> m (JSFloat m)
  floatFromDouble = error "floatFromDouble"

  -- | Truncates double to int type.
  intFromDouble :: JSDouble m -> m (JSInt m)
  intFromDouble = error "intFromDouble"

  -- | Truncates double to long type.
  longFromDouble :: JSDouble m -> m (JSLong m)
  longFromDouble = error "longFromDouble"

  -- | Converts float to double type.
  doubleFromFloat :: JSFloat m -> m (JSDouble m)
  doubleFromFloat = error "doubleFromFloat"

  -- | Truncates float to int type.
  intFromFloat :: JSFloat m -> m (JSInt m)
  intFromFloat = error "intFromFloat"

  -- | Truncates float to long type.
  longFromFloat :: JSFloat m -> m (JSLong m)
  longFromFloat = error "longFromFloat"

  -- | Truncates int to lower-order bit, then zero-extends back to int.
  boolFromInt :: JSInt m -> m (JSInt m)
  boolFromInt i = do
    one <- iConst 1
    i `iAnd` one

  -- | Truncates int to byte, then sign-extends back to int.
  byteFromInt :: JSInt m -> m (JSInt m)
  byteFromInt i = do
     shiftVal <- iConst 24
     shlValue <- i `iShl` shiftVal
     shlValue `iShr` shiftVal

  -- | Truncates int to char, then zero-extends back to int.
  charFromInt :: JSInt m -> m (JSInt m)
  charFromInt i = do
    v <- iConst 0xFFFF
    i `iAnd` v

  -- | Converts int to double
  doubleFromInt :: JSInt m -> m (JSDouble m)
  doubleFromInt = error "doubleFromInt"

  -- | Converts int to float
  floatFromInt :: JSInt m -> m (JSFloat m)
  floatFromInt = error "floatFromInt"

  -- | Converts int to long by sign-extending
  longFromInt :: JSInt m -> m (JSLong m)

  -- | Converts int to short by truncating then sign extends result.
  shortFromInt :: JSInt m -> m (JSInt m)
  shortFromInt i = do
    shiftVal <- iConst 16
    shlValue <- i `iShl` shiftVal
    shlValue `iShr` shiftVal

  -- | Converts long to double using IEEE 754 round to nearest.
  doubleFromLong :: JSLong m -> m (JSDouble m)
  doubleFromLong = error "doubleFromLong"

  -- | Converts long to float using IEEE 754 round to nearest.
  floatFromLong :: JSLong m -> m (JSFloat m)
  floatFromLong = error "floatFromLong"

  -- | Converts long to int by truncating high-order bits.
  intFromLong :: JSLong m -> m (JSInt m)

  -- Double functions {{{1

  -- | Returns sum of two inputs according to IEEE754 arithmetic
  dAdd :: JSDouble m -> JSDouble m -> m (JSDouble m)
  dAdd = error "dAdd"

  -- | Performs comparison of inputs by rules of dcmpg instruction.
  dCmpg :: JSDouble m -> JSDouble m -> m (JSInt m)
  dCmpg = error "dCmpg"

  -- | Performs comparison of inputs by rules of dcmpl instruction.
  dCmpl :: JSDouble m -> JSDouble m -> m (JSInt m)
  dCmpl = error "dCmpl"

  -- | Returns double value represented by constant.
  dConst :: Double -> m (JSDouble m)
  dConst = error "dConst"

  -- | Returns first argument divided by second according to IEEE754 arithmetic
  dDiv :: JSDouble m -> JSDouble m -> m (JSDouble m)
  dDiv = error "dDiv"

  -- | Returns product of two inputs according to IEEE754 arithmetic
  dMul :: JSDouble m -> JSDouble m -> m (JSDouble m)
  dMul = error "dMul"

  -- | Returns negation of input according to IEEE754 arithmetic
  dNeg :: JSDouble m -> m (JSDouble m)
  dNeg = error "dNeg"

  -- | Returns IEEE754 remainder of two inputs.
  dRem :: JSDouble m -> JSDouble m -> m (JSDouble m)
  dRem = error "dRem"

  -- | Returns difference of two inputs according to IEEE754 arithmetic
  dSub :: JSDouble m -> JSDouble m -> m (JSDouble m)
  dSub = error "dSub"

  -- Float functions {{{1

  -- | Returns sum of two single precision inputs according to IEEE754 arithmetic
  fAdd :: JSFloat m -> JSFloat m -> m (JSFloat m)
  fAdd = error "fAdd"

  -- | Performs comparison of inputs by rules of fcmpg instruction.
  fCmpg :: JSFloat m -> JSFloat m -> m (JSInt m)
  fCmpg = error "fCmpg"

  -- | Performs comparison of inputs by rules of fcmpl instruction.
  fCmpl :: JSFloat m -> JSFloat m -> m (JSInt m)
  fCmpl = error "fCmpl"

  -- | Returns single precision floating point value represented by constant.
  fConst :: Float -> m (JSFloat m)
  fConst = error "fConst"

  -- | Returns first argument divided by second according to IEEE754 arithmetic
  fDiv :: JSFloat m -> JSFloat m -> m (JSFloat m)
  fDiv = error "fDiv"

  -- | Returns product of two inputs according to IEEE754 arithemetic.
  fMul :: JSFloat m -> JSFloat m -> m (JSFloat m)
  fMul = error "fMul"

  -- | Returns negation of input.
  fNeg :: JSFloat m -> m (JSFloat m)
  fNeg = error "fNeg"

  -- | Returns IEEE754 remainder of two inputs.
  fRem :: JSFloat m -> JSFloat m -> m (JSFloat m)
  fRem = error "fRem"

  -- | Returns different of two inputs
  fSub :: JSFloat m -> JSFloat m -> m (JSFloat m)
  fSub = error "fSub"

  -- Integer functions {{{1

  -- | Returns sum of two inputs
  iAdd :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns bitwise intersection of two inputs
  iAnd :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns value representing constant.
  iConst :: Int32 -> m (JSInt m)

  -- | Returns division of inputs.
  --
  -- Can assume that second input is not zero.
  iDiv :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns whether two inputs are equal.
  iEq :: JSInt m -> JSInt m -> m (JSBool m)

  -- | Returns whether first input is less than or equal to second.
  iLeq :: JSInt m -> JSInt m -> m (JSBool m)

  -- | Returns whether first input is less than second.
  iLt :: JSInt m -> JSInt m -> m (JSBool m)
  x `iLt` y = bNot =<< y `iLeq` x

  -- | Returns product of two inputs.
  iMul :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns negation of input.
  iNeg :: JSInt m -> m (JSInt m)

  -- | Returns bitwise disjunction of two inputs.
  iOr :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns remainder of first input divided by second.
  --
  -- Can assume that second input is not zero.
  iRem :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns first input shifted left by second input mod 32.
  iShl :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns first input sign shifted right by second input mod 32.
  iShr :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns difference of two inputs.
  iSub :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns first input unsign shifted right by second input mod 32.
  iUshr :: JSInt m -> JSInt m -> m (JSInt m)

  -- | Returns bitwise exclusive or of two inputs.
  iXor :: JSInt m -> JSInt m -> m (JSInt m)

  -- Long functions {{{1

  -- | Returns sum of two inputs.
  lAdd :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Returns bitwise intersection of two inputs.
  lAnd :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Compares two values and x and y, returning 1 if x > y,
  -- 0 if x == y, and -1 if x < y.
  lCmp :: JSLong m -> JSLong m -> m (JSInt m)

  -- | Returns value representing constant.
  lConst :: Int64 -> m (JSLong m)

  -- | Returns division of inputs.
  --
  -- Requires that second input is not zero.
  lDiv :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Returns whether two inputs are equal.
  lEq :: JSLong m -> JSLong m -> m (JSBool m)

  -- | Returns product of two inputs.
  lMul :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Returns negation of input.
  lNeg :: JSLong m -> m (JSLong m)

  -- | Returns bitwise disjunction of two inputs.
  lOr :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Returns remainder of first input divided by second.
  --
  -- Requires that second input is not zero.
  lRem :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Returns first input shifted left by second input mod 64; tight
  -- adherence to the JVM spec says that this should be :: JSLong m -> int
  -- -> m (JSLong m), but the step function promotes both operands to longs.
  lShl :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Returns first input sign shifted right by second input mod 64;
  -- tight adherence to the JVM spec says this should be :: JSLong m -> int
  -- -> m (JSLong m), but the step function promotes both operands to longs.
  lShr :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Returns difference of two inputs.
  lSub :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Returns first input unsign shifted right by second input mod 64.
  lUshr :: JSLong m -> JSLong m -> m (JSLong m)

  -- | Returns bitwise exclusive or of two inputs.
  lXor :: JSLong m -> JSLong m -> m (JSLong m)

  -- Reference functions {{{1

  -- | Returns length of array at ref.
  arrayLength :: JSRef m -> m (JSInt m)

  -- (mkMultiNewArray tp len) returns a reference to a multidimentional array with
  -- type tp and len = [len1, len2, ...] where len1 equals length of first dimention
  -- len2 equals length of second dimension and so on.
  -- NOTE: Integer values are required to be non-negative.
  newMultiArray :: Type -> [JSInt m] -> m (JSRef m)

  -- | @newObject className@ returns a reference to a new instance of @className@.
  -- NOTE: Will initialize class if it has not already been initialized.
  --       Fields are initialized to have default value.
  newObject :: String -> m (JSRef m)

  -- | @isValidEltOfArray elt array@ returns true if @elt@ can be stored in
  -- arrayRef.
  -- NOTE: @array@ must be a non-null reference array, but @elt@ may be null.
  isValidEltOfArray :: JSRef m -> JSRef m -> m (JSBool m)

  -- Returns true if ref has given type.
  -- Note: Requires ref is non-null
  hasType :: JSRef m -> Type -> m (JSBool m)

  -- Returns the type of ref; Nothing if the given ref is null
  typeOf :: JSRef m -> m (Maybe Type)

  -- @coerceRef r ty@ coerces the reference @r@ to type @ty@.  Note that
  -- it is the responsibility of the caller to ensure that the coercion
  -- is safe and correct.
  coerceRef :: JSRef m -> Type -> m (JSRef m)

  -- | @superHasType ref typeName@ returns true if super class of @ref@ has
  -- type @typeName@.
  -- Note: Requires @ref@ points to a class type.
  superHasType :: JSRef m -> String -> m (JSBool m)

  -- | @rEq x y@ returns true if x == y.
  rEq :: JSRef m -> JSRef m -> m (JSBool m)

  -- rNull returns node representing null pointer.
  rNull :: m (JSRef m)

  -- Returns reference for given string constant.
  -- NOTE: Requires string comes from constant pool of an initialized class.
  refFromString :: String -> m (JSRef m)

  -- Returns the @Class@ instance corresponding to the given class name.
  getClassObject :: String -> m (JSRef m)

  -- Heap related functions {{{1

  -- Pushes value of field onto stack.
  -- NOTE: Requires ref is non null.
  pushInstanceFieldValue :: JSRef m -> FieldId -> m ()

  -- Pushes value of field onto stack
  pushStaticFieldValue :: FieldId -> m ()

  -- (pushArrayValue ref index) pushes the value of the array at index to the stack.
  -- NOTE: Requires ref is a valid array and index is a valid index in array.
  pushArrayValue :: JSRef m -> JSInt m -> m ()

  setArrayValue :: JSRef m -> JSInt m -> JSValue m -> m ()

  setInstanceFieldValue :: JSRef m -> FieldId -> JSValue m -> m ()

  setStaticFieldValue :: FieldId -> JSValue m -> m ()

  -- Pop value at top of stack.
  popValue :: m (JSValue m)

  -- Push value at top of stack.
  pushValue :: JSValue m -> m ()

  -- Pop address of top of stack.
  dPop :: m (JSDouble m)
  dPop = popValue >>= ret
    where ret (DValue v) = return v
          ret av = error $ "dPop: unexpected AtomicValue type: " ++ show av

  dPush :: JSDouble m -> m ()
  dPush = pushValue . DValue

  fPop :: m (JSFloat m)
  fPop = popValue >>= ret
    where ret (FValue v) = return v
          ret av = error $ "fPop: unexpected AtomicValue type: " ++ show av

  fPush :: JSFloat m -> m ()
  fPush = pushValue . FValue

  iPop :: m (JSInt m)
  iPop = popValue >>= ret
    where ret (IValue v) = return v
          ret av = error $ "iPop: unexpected AtomicValue type: " ++ show av

  iPush :: JSInt m -> m ()
  iPush = pushValue . IValue

  lPop :: m (JSLong m)
  lPop = popValue >>= ret
    where ret (LValue l) = return l
          ret av = error $ "lPop: unexpected AtomicValue type: " ++ show av

  lPush :: JSLong m -> m ()
  lPush = pushValue . LValue

  rPop :: m (JSRef m)
  rPop = popValue >>= ret
    where ret (RValue v) = return v
          ret av = error $ "rPop: unexpected AtomicValue type: " ++ show av

  -- Local variable functions {{{1
  -- Get local variable at index.
  getLocal :: LocalVariableIndex -> m (JSValue m)
  -- Set local variable at index.
  setLocal :: LocalVariableIndex -> JSValue m -> m ()
  --}}}

  -- | @printStream nl binary vals@ prints the given JSValue, followed by
  -- a newline when @nl@ is True, and in binary when @binary@ is true.
  printStream :: Bool -> Bool -> [JSValue m] -> m ()

  die :: String -> m ()

  doStep :: m ()

--------------------------------------------------------------------------------
-- Control flow primitives and helpers

data Case m v = Case (m (JSBool m)) (m v)
(|->) :: m (JSBool m) -> m v -> Case m v
cond |-> fn = Case cond fn
infix 1 |->

-- | Returns true if reference is null.
isNull :: JavaSemantics m => JSRef m -> m (JSBool m)
isNull ref = rEq ref =<< rNull

arrayGuards :: JavaSemantics m => JSRef m -> JSInt m -> [Case m ()]
arrayGuards arrayref index
   = [ isNull arrayref
        |-> throwNullPtrExc
     , do zero <- iConst 0
          arrayLen <- arrayLength arrayref
          index `iLt` zero ||| arrayLen `iLeq` index
        |-> createAndThrow "java/lang/ArrayIndexOutOfBoundsException"
     ]

choice :: JavaSemantics m => [Case m ()] -> m () -> m ()
choice (Case cond fn : rest) dflt = forkM cond fn (choice rest dflt)
choice [] dflt                    = dflt >> return ()

forkM :: JavaSemantics m => m (JSBool m) -> m () -> m () -> m ()
forkM bM trueM falseM = do
  cond <- bM
  fork cond trueM falseM

-- | Creates an exception of the given class (which is assumed to have a no
-- argument constructor) and throws it.
createAndThrow :: JavaSemantics m => String -> m ()
createAndThrow = (`createInstance` Nothing) >=> throw

throwNullPtrExc :: JavaSemantics m => m a
throwNullPtrExc = createAndThrow "java/lang/NullPointerException" >> error "unreachable"

--------------------------------------------------------------------------------
-- Instance creation, query, and method dispatch

createInstance :: JavaSemantics m => String -> Maybe [(Type, JSValue m)] -> m (JSRef m)
createInstance clNm margs = do
  cb <- getCodebase
  cl <- liftIO $ lookupClass cb clNm
  case cl `lookupMethod` ctorKey of
    Just method -> do
      ref <- newObject clNm
      runInstanceMethodCall clNm method ref (maybe [] (map snd) margs)
      return ref
    Nothing ->
      error $ "Unable to find method " ++ clNm ++ " (signature mismatch?)"
  where
    ctorKey = MethodKey "<init>" (maybe [] (map fst) margs) Nothing

-- | (dynBind cln meth r act) provides to 'act' the class name that defines r's
-- implementation of 'meth'
dynBind :: JavaSemantics m
        => String           -- ^ Name of 'this''s class
        -> MethodKey        -- ^ Key of method to invoke
        -> JSRef m          -- ^ 'this'
        -> (String -> m ()) -- ^ e.g., an invokeInstanceMethod invocation.
         -> m ()
dynBind clName key objectRef act =
  dynBind' clName key objectRef $ \cl ->
    objectRef `hasType` ClassType cl |-> act cl

-- Note: Assume linker errors can not be thrown
dynBind' :: JavaSemantics m
         => String                -- ^ Name of 'this''s class
         -> MethodKey             -- ^ Key of method to invoke
         -> JSRef m               -- ^ 'this'
         -> (String -> Case m ()) -- ^ case generator for method dispatch
         -> m ()
dynBind' clName key objectRef cgen = do
  mty <- typeOf objectRef
  cb <- getCodebase
  cls <- case mty of
           Nothing     -> return []
           Just (ClassType instTy) -> liftIO $ findVirtualMethodsByRef cb clName key instTy
           Just _ -> error "dynBind' type parameter not ClassType-constructed"


  let cases = (isNull objectRef |-> throwNullPtrExc) : map cgen cls
  -- In theory, this error should be unreachable.
  choice cases (error $ "Uncaught linker error: " ++ clName ++ ":" ++ show key)

-- | Invokes an instance method in a particular class.
invokeInstanceMethod :: JavaSemantics m
                     => String      -- ^ Name of class
                     -> MethodKey   -- ^ Key of method to invoke
                     -> JSRef m     -- ^ Reference to use for this parameter.
                     -> [JSValue m] -- ^ Operands to pass method
                     -> m ()
invokeInstanceMethod cName key objectRef args = do
  cb <- getCodebase
  cl <- liftIO $ lookupClass cb cName
  case cl `lookupMethod` key of
     Just method -> pushInstanceMethodCall cName method objectRef args
     Nothing -> error $
       "Could not find instance method " ++ show key ++ " in " ++ cName
         ++ "\n  objectRef = " ++ show objectRef ++ ", args = " ++ show args

-- | Invokes a static method in a particular class.
invokeStaticMethod :: JavaSemantics m
                   => String      -- ^ Name of class
                   -> MethodKey   -- ^ Key of method to invoke
                   -> [JSValue m] -- ^ Operands to pass to method
                   -> m ()
invokeStaticMethod cName key args = do
  cb <- getCodebase
  sups <- liftIO (supers cb =<< lookupClass cb cName)
  case mapMaybe (\cl -> (,) cl `fmap` (cl `lookupMethod` key)) sups of
    ((cl,method):_) -> do
      when (not $ methodIsStatic method) $
        fatal $ "Attempted static invocation on a non-static method ("
              ++ className cl ++ "." ++ methodName method ++ ")"
      initializeClass (className cl)
      pushStaticMethodCall (className cl) method args
    [] -> error $ "Could not find static method " ++ show key ++ " in " ++ cName

--------------------------------------------------------------------------------
-- Instruction/stack manip

-- | Advances PC to next instruction.
gotoNextInstruction :: JavaSemantics m => m ()
gotoNextInstruction = setPc =<< getNextPc

-- | Returns next program counter value for execution.
getNextPc :: JavaSemantics m => m PC
getNextPc = do
  finished <- isFinished
  if finished then error "Is finished early" else return ()
  liftM2 nextPc getCurrentMethod getPc

-- | Pops a single value from stack.
popType1 :: JavaSemantics m => m (JSValue m)
popType1 = popValue

-- | Pops a single Type 2 value or 2 type 1 values from stack.
popType2 :: JavaSemantics m => m [JSValue m]
popType2 = do
  value <- popValue
  case value of
    DValue _v -> return [value]
    LValue _v -> return [value]
    _ -> do
      value2 <- popValue
      return [value2, value]

pushValues :: JavaSemantics m => [JSValue m] -> m ()
pushValues list = mapM_ pushValue list

--------------------------------------------------------------------------------
-- Instances

instance ( Show double
         , Show float
         , Show int
         , Show long
         , Show ref)
         => Show (AtomicValue double float int long ref) where
  show (AValue x) = "A:" ++ show x
  show (DValue x) = "D:" ++ show x
  show (FValue x) = "F:" ++ show x
  show (IValue x) = "I:" ++ show x
  show (LValue x) = "L:" ++ show x
  show (RValue x) = "R:" ++ show x

instance ( Eq double
         , Eq float
         , Eq int
         , Eq long
         , Eq ref)
         => Eq (AtomicValue double float int long ref)where
  DValue x == DValue y = (x == y)
  FValue x == FValue y = (x == y)
  IValue x == IValue y = (x == y)
  LValue x == LValue y = (x == y)
  RValue x == RValue y = (x == y)
  AValue x == AValue y = (x == y)
  _x == _y = False

instance ( Ord double
         , Ord float
         , Ord int
         , Ord long
         , Ord ref)
         => Ord (AtomicValue double float int long ref) where
  DValue x <= DValue y = (x <= y)
  DValue _ <= _ = True
  _ <= DValue _ = False

  FValue x <= FValue y = (x <= y)
  FValue _ <= _ = True
  _ <= FValue _ = False

  IValue x <= IValue y = (x <= y)
  IValue _ <= _ = True
  _ <= IValue _ = False

  LValue x <= LValue y = (x <= y)
  LValue _ <= _ = True
  _ <= LValue _ = False

  RValue x <= RValue y = (x <= y)
  RValue _ <= _ = True
  _ <= RValue _ = False

  AValue x <= AValue y = (x <= y)
