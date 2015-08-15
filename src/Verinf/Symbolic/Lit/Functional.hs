{-# LANGUAGE ImplicitParams #-}
{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : jhendrix

Provides a functional interface to the bitblasting backend.
-}
module Verinf.Symbolic.Lit.Functional
  ( -- | Lit operations
    BitEngine
  , lTrue
  , lFalse
  , lNeg
  , lAnd
  , lEqLit
  , lMkInput
  , lFromBool
  , lOr
  , lImplies
  , lEq
  , lXor
  , lIte
  , lEvalAig
  , lWriteAiger
  , lLazyMux
    -- | Lit Vector operations
  , lVectorFromInt
  , lGetUnsigned
  , lGetSigned
  , lEqVector
  , lIteVector
  , lMuxInteger
    -- Shift operation
  , lShlC
  , lShrC
  , lShl
  , lUnsignedShr
  , lSignedShr
     -- Arithmetic operations
  , lNegate
  , lFullAdd
  , lFullAddConstant
  , lFullSub
  , lFullMul
  , lMul
  , lQuotRem, lQuot, lRem
  , lUnsignedQuotRem, lUnsignedQuot, lUnsignedRem
  -- Comparison relations
  , lSignedLt
  , lSignedLeq
  , lSignedGt
  , lSignedGeq
  , lUnsignedLt
  , lUnsignedLeq
  , lUnsignedGt
  , lUnsignedGeq
  ) where

import Control.Exception (assert)
import System.IO.Unsafe (unsafePerformIO)
import Verinf.Symbolic.Lit.DataTypes
import Data.Bits (setBit, testBit)
import qualified Data.Vector.Storable as LV

-- Utility functions {{{1

-- | @iFoldlN' fn init n@ returns result of applying fn @n@ times to n.
iFoldlN' :: (a -> Int -> a) -> a -> Int -> a
iFoldlN' fn initValue n = impl initValue 0
  where impl v i | i == n = v
                 | otherwise = (impl $! (fn v i)) $! (i+1)

-- | @iterateScanN n f v@ returns vector with @n@ elements where the ith value
-- is obtained by applying @f@ @i@-times to @v@.
iterateScanN :: LV.Storable v => Int -> (v -> Int -> v) -> v -> LV.Vector v
iterateScanN n f v = LV.scanl' f v (LV.enumFromN 0 n)

-- Core definitions {{{1

lTrue :: (?be :: BitEngine l) => l
lTrue = beTrue ?be

lFalse :: (?be :: BitEngine l) => l
lFalse = beFalse ?be

lNeg :: (?be :: BitEngine l) => l -> l
lNeg = beNeg ?be

{-# NOINLINE lAnd #-}
lAnd :: (?be :: BitEngine l) => l -> l -> l
lAnd x y
  | x `lEqLit` lFalse = lFalse
  | x `lEqLit` lTrue = y
  | otherwise = unsafePerformIO $ beAnd ?be x y

{-# NOINLINE lXor #-}
lXor :: (?be :: BitEngine l) => l -> l -> l
lXor x y = unsafePerformIO $ beXor ?be x y

{-# NOINLINE lIte #-}
lIte :: (?be :: BitEngine l) => l -> l -> l -> l
lIte c t f
  | c `lEqLit` lTrue = t
  | c `lEqLit` lFalse = f
  | otherwise = unsafePerformIO $ beMux ?be c t f

lEqLit :: (?be :: BitEngine l) => l -> l -> Bool
lEqLit = beEqLit ?be

lMkInput :: (?be :: BitEngine l) => IO l
lMkInput = beMakeInputLit ?be

{-# NOINLINE lEvalAig #-}
-- | Evaluate AIG on vector of arguments.
lEvalAig :: (?be :: BitEngine l, LV.Storable l)
          => LV.Vector Bool -> LV.Vector l -> IO (LV.Vector Bool)
lEvalAig = beEvalAigV ?be

lWriteAiger :: (?be :: BitEngine l, LV.Storable l)
            => FilePath -> LV.Vector l -> IO ()
lWriteAiger path outputs = do
  inputs <- beInputLits ?be
  beWriteAigerV ?be path inputs outputs

-- Bit level operations {{{1

lFromBool :: (?be :: BitEngine l) => Bool -> l
lFromBool True = lTrue
lFromBool False = lFalse

lOr :: (?be :: BitEngine l) => l -> l -> l
lOr x y = lNeg (lNeg x `lAnd` lNeg y)

-- Internal helper methods {{{2

infixr 3  &&&
infixr 2  |||

(&&&) :: (?be :: BitEngine l) => l -> l -> l
(&&&) = lAnd

(|||) :: (?be :: BitEngine l) => l -> l -> l
(|||) = lOr

-- }}}2

lImplies :: (?be :: BitEngine l) => l -> l -> l
lImplies x y = lNeg (x &&& lNeg y)

lEq :: (?be :: BitEngine l) => l -> l -> l
lEq x y = lNeg (lXor x y)

-- | Creates a mux that only evaluates arguments if needed.
lLazyMux :: (?be :: BitEngine l) => (l -> a -> a -> a) -> l -> a -> a -> a
lLazyMux fn c t f | c `lEqLit` lTrue = t
                  | c `lEqLit` lFalse = f
                  | otherwise = fn c t f

-- Vector operations {{{1

-- | Returns a lit vector with the given bitwidth from the integer.
lVectorFromInt :: (?be :: BitEngine l, LV.Storable l)
               => Int -> Integer -> LitVector l
lVectorFromInt w v = LV.generate w (lFromBool . testBit v)

-- | Returns a constant integer if the given vector corresponds to
-- a constant value, and Nothing if the lit vector is a symbolic value.
lGetUnsigned :: (?be :: BitEngine l, LV.Storable l)
             => LitVector l -> Maybe Integer
lGetUnsigned v = impl 0 0
  where impl :: Int -> Integer -> Maybe Integer
        impl i r
          | i == LV.length v = Just r
          | (v LV.! i) `lEqLit` lTrue  = impl (i+1) (r `setBit` i)
          | (v LV.! i) `lEqLit` lFalse = impl (i+1) r
          | otherwise = Nothing

-- | Returns a constant integer if the given vector corresponds to
-- a constant value, and Nothing if the lit vector is a symbolic value.
lGetSigned :: (?be :: BitEngine l, LV.Storable l)
           => LitVector l -> Maybe Integer
lGetSigned v
    | n == 0 = Just 0
    | otherwise = impl 0 0
  where n = LV.length v
        impl :: Int -> Integer -> Maybe Integer
        impl i r
          | (v LV.! i) `lEqLit` lTrue  =
            if i + 1 == n then Just (r - 2 ^ i) else impl (i+1) (r `setBit` i)
          | (v LV.! i) `lEqLit` lFalse =
            if i + 1 == n then Just r else impl (i+1) r
          | otherwise = Nothing

-- | Returns predicate that holds if vectors are equal.
lEqVector :: (?be :: BitEngine l, LV.Storable l)
          => LitVector l -> LitVector l -> l
lEqVector vx vy = assert (LV.length vx == LV.length vy)
                $ LV.foldl' lAnd lTrue (LV.zipWith lEq vx vy)

lIteVector :: (?be :: BitEngine l, LV.Storable l)
           => l -> LitVector l -> LitVector l -> LitVector l
lIteVector = lLazyMux (\c -> LV.zipWith (lIte c))

-- | @lMuxInteger mergeFn maxValue lv valueFn@ returns a circuit
-- whose result is @valueFn v@ when @lv@ has value @v@.
lMuxInteger :: (Integral i, LV.Storable l)
            => (l -> a -> a -> a)
            -> i -- ^ Maximum value input vector is allowed to take.
            -> LV.Vector l -- ^ Input vector
            -> (i -> a)
            -> a
lMuxInteger mergeFn maxValue vx valueFn =
  let impl _ y | y >= toInteger maxValue = valueFn maxValue
      impl 0 y = valueFn (fromInteger y)
      impl i y = mergeFn (vx LV.! j) (impl j (y `setBit` j)) (impl j y)
       where j = i - 1
   in impl (LV.length vx) 0


-- Shift operations {{{1

-- | @lShlC lits fill shift@ shifts @lits@ to the left by @shift@ bits.
-- New bits are given the value @fill@.
lShlC :: LV.Storable l => LitVector l -> l -> Int -> LitVector l
lShlC x fill shft
  | shft >= n = LV.replicate n fill
  | otherwise = LV.replicate shft fill LV.++ LV.take (n - shft) x
 where n = LV.length x

-- | @beShiftRightC lits fill shift@ shifts "lits" to the right by "shift" bits.
-- New bits are given the value "fill".
lShrC :: LV.Storable l => LitVector l -> l -> Int -> LitVector l
lShrC x fill shft
  | shft >= n = LV.replicate n fill
  | otherwise = LV.drop shft x LV.++ LV.replicate shft fill
  where n = LV.length x

-- | Shift bit vector to left.
lShl :: (?be :: BitEngine l, LV.Storable l)
     => LitVector l -> LitVector l -> LitVector l
lShl x y = lMuxInteger lIteVector (LV.length x) y (lShlC x lFalse)

-- | Shift bit vector to left.
lSignedShr :: (?be :: BitEngine l, LV.Storable l)
           => LitVector l -> LitVector l -> LitVector l
lSignedShr x y = lMuxInteger lIteVector (LV.length x) y (lShrC x (LV.last x))

-- | Shift bit vector to left.
lUnsignedShr :: (?be :: BitEngine l, LV.Storable l)
              => LitVector l -> LitVector l -> LitVector l
lUnsignedShr x y = lMuxInteger lIteVector (LV.length x) y (lShrC x lFalse)

-- Arithmetic {{{1

--- | Returns twos complement of literal.
lNegate :: (?be :: BitEngine l, LV.Storable l) => LitVector l -> LitVector l
lNegate vx = LV.generate (LV.length vx) $ \i ->
               (vc LV.! (i+1)) &&& lNeg ((vx LV.! i) &&& (vc LV.! i))
  where vc = LV.scanl lOr lFalse vx

-- | Add two bit vectors together, returning vector and overflow.
lFullAddFn :: (?be :: BitEngine l, LV.Storable l)
                => Int -> (Int -> l) -> (Int -> l) -> (l, LitVector l)
lFullAddFn n fx fy = (LV.last vc, vr)
  where nextCarry c i = (x ||| y) &&& (c ||| x &&& y)
          where x = fx i; y = fy i
        vc = iterateScanN n nextCarry lFalse
        vr = LV.generate n $ \i -> (fx i `lEq` fy i) `lEq` (vc LV.! i)

-- | Add two bit vectors together, returning vector and overflow.
lFullAdd :: (?be :: BitEngine l, LV.Storable l)
         => LitVector l -> LitVector l -> (l, LitVector l)
lFullAdd vx vy = assert (LV.length vx == LV.length vy)
               $ lFullAddFn (LV.length vx) (vx LV.!) (vy LV.!)

-- | Add two bit vectors together, returning vector and overflow.
lFullAddConstant :: (?be :: BitEngine l, LV.Storable l)
                 => LitVector l -> Integer -> (l, LitVector l)
lFullAddConstant vx y =
  lFullAddFn (LV.length vx) (vx LV.!) (lFromBool . testBit y)

-- | Add two bit vectors together, returning borrow bit and result.
lFullSub :: (?be :: BitEngine l, LV.Storable l)
         => LitVector l -> LitVector l -> (l, LitVector l)
lFullSub vx vy = assert (LV.length vx == LV.length vy)
               $ (LV.last vb, vx `lvEq` (vy `lvEq` vb))
  where lvEq = LV.zipWith lEq
        nextBorrow b i = y &&& b ||| lNeg x &&& (y ||| b)
          where x = vx LV.! i; y = vy LV.! i
        vb = iterateScanN (LV.length vx) nextBorrow lFalse

-- | Returns the product of a vector and integer.  This is a full multiplier
-- that does not require the inputs have the same length.
lMul :: (?be :: BitEngine l, LV.Storable l)
     => LitVector l -> LitVector l -> LitVector l
lMul vx vy = iFoldlN' mulStep vZero (LV.length vy)
 where lvx = LV.length vx
       vZero = LV.replicate lvx lFalse
       lAdd x y = snd (x `lFullAdd` y)
       lShift x i = LV.take lvx (LV.replicate i lFalse LV.++ x)
       mulStep r i =
         lIteVector (vy LV.! i)
                    (r `lAdd` (vx `lShift` i))
                    r

-- | Returns the product of a vector and integer.  This is a full multiplier
-- that does not require the inputs have the same length.
lFullMul :: (?be :: BitEngine l, LV.Storable l)
         => LitVector l -> LitVector l -> LitVector l
lFullMul vx vy = iFoldlN' mulStep vZero (LV.length vy)
 where vZero = LV.replicate (LV.length vx) lFalse
       lAddExt x y = (\(c,r) -> LV.snoc r c) (x `lFullAdd` y)
       lShiftExt x i = LV.replicate i lFalse LV.++ x
       mulStep r i =
         lIteVector (vy LV.! i)
                    (r `lAddExt` (vx `lShiftExt` i))
                    (r `LV.snoc` lFalse)

-- Perform quotRem on the absolute value of the operands.  Then, negate the
-- quotient if the signs of the operands differ and make the sign of a nonzero
-- remainder to match that of the dividend.
lQuotRem :: (?be :: BitEngine l, LV.Storable l)
         => LitVector l -> LitVector l -> (LitVector l,LitVector l)
lQuotRem dividend' divisor' = do
    assert (n > 0 && n == LV.length divisor') $ (q', r')
  where
    lShiftL1 v f = f `LV.cons` LV.init v
    lShiftR1 v = LV.tail v `LV.snoc` lFalse
    n = LV.length dividend'
    signLitLsbf        = LV.last
    dsign              = signLitLsbf dividend'
    il `negWhenM` l    = lIteVector l (lNegate il) il
    absLsbf x          = lIteVector (signLitLsbf x) (lNegate x) x
    dividend = absLsbf dividend'
    divisor = absLsbf divisor'
    divStep rrOrig _i =
      lIteVector (signLitLsbf s)
                 (lShiftL1 rrOrig lFalse) -- rem < 0, orig rr's quot lsl'd w/ 0
                 (lShiftL1 (q LV.++ s) lTrue)   -- rem >= 0, new rr's quot lsl'd w/ 1
     where (q,r) = LV.splitAt n rrOrig
           -- Subtract the divisor from the left half of the "remainder register"
           s = snd (lFullSub r divisor)
    -- Given an n-bit dividend and divisor, 'initial' is the starting value of
    -- the 2n-bit "remainder register" that carries both the quotient and remainder;
    initial = dividend LV.++ LV.replicate n lFalse
    (qr,rr) = LV.splitAt n
          $ LV.foldl divStep (lShiftL1 initial lFalse) (LV.enumFromN (0::Int) n)
    q' = qr `negWhenM` lXor dsign (signLitLsbf divisor')
    r' = lShiftR1 rr `negWhenM` dsign


lQuot :: (?be :: BitEngine l, LV.Storable l)
      => LitVector l -> LitVector l -> LitVector l
lQuot x y = fst (lQuotRem x y)

lRem :: (?be :: BitEngine l, LV.Storable l)
     => LitVector l -> LitVector l -> LitVector l
lRem x y = snd (lQuotRem x y)

-- | Bitblast version of unsigned @quotRem@.
lUnsignedQuotRem :: (?be :: BitEngine l, LV.Storable l)
                 => LitVector l -> LitVector l -> (LitVector l, LitVector l)
lUnsignedQuotRem dividend divisor =
    assert (n == LV.length divisor) $ divStep 0 lFalse initial
  where
    n = LV.length dividend
    lShiftL1 v f = f `LV.cons` LV.init v
    divStep i p rr
      | i == n = (lShiftL1 (LV.take n rr) p, LV.drop n rr)
      | otherwise = divStep (i+1) (lNeg b) (lIteVector b rs (q LV.++ s))
     where rs = lShiftL1 rr p
           (q,r) = LV.splitAt n rs
           -- Subtract the divisor from the left half of the "remainder register"
           (b,s) = lFullSub r divisor
    -- Given an n-bit dividend and divisor, 'initial' is the starting value of
    -- the 2n-bit "remainder register" that carries both the quotient and remainder;
    initial = (dividend LV.++ LV.replicate n lFalse)

lUnsignedQuot :: (?be :: BitEngine l, LV.Storable l)
              => LitVector l -> LitVector l -> LitVector l
lUnsignedQuot x y = fst (lUnsignedQuotRem x y)

lUnsignedRem :: (?be :: BitEngine l, LV.Storable l)
             => LitVector l -> LitVector l -> LitVector l
lUnsignedRem x y = snd (lUnsignedQuotRem x y)

-- Comparison operators {{{1


-- | @lSignedLt x y@ returns predicate indicating if x is less than y.
lSignedLt :: (?be :: BitEngine l, LV.Storable l) => LitVector l -> LitVector l -> l
lSignedLt vx vy =
  assert (LV.length vx > 0 && LV.length vx == LV.length vy) $
   lOr (x &&& lNeg y) -- Return true if x is negative and y is non-negative.
       (lAnd (lNeg (lNeg x &&& y)) -- Return false if y is negative and x is non-negative.
             (fst (lFullSub (LV.init vx) (LV.init vy)))) -- Otherwise check for borrow overflow.
  where { x = LV.last vx; y = LV.last vy }

-- | @lSignedLeq x y@ returns predicate indicating if x is less than
-- or equal to y.
lSignedLeq :: (?be :: BitEngine l, LV.Storable l)
           => LitVector l -> LitVector l -> l
lSignedLeq x y = lNeg (lSignedLt y x)

-- | @lSignedGt x y@ returns predicate indicating if x is greater than y.
lSignedGt :: (?be :: BitEngine l, LV.Storable l)
          => LitVector l -> LitVector l -> l
lSignedGt = flip lSignedLt

-- | @lSignedGeq x y@ returns predicate indicating if x is greater than
-- or equal to y.
lSignedGeq :: (?be :: BitEngine l, LV.Storable l)
           => LitVector l -> LitVector l -> l
lSignedGeq = flip lSignedLeq

-- | @lUnsignedLt x y@ returns predicate indicating if x is less than y.
-- N.B. This is the case if x - y sets the  borrow bit.
lUnsignedLt :: (?be :: BitEngine l, LV.Storable l)
             => LitVector l -> LitVector l -> l
lUnsignedLt x y = fst (lFullSub x y)

-- | @lUnsignedLeq x y@ returns predicate indicating if x is less than
-- or equal to y.
lUnsignedLeq :: (?be :: BitEngine l, LV.Storable l)
             => LitVector l -> LitVector l -> l
lUnsignedLeq x y = lNeg (lUnsignedLt y x)

-- | @lUnsignedGt x y@ returns predicate indicating if x is greater than y.
lUnsignedGt :: (?be :: BitEngine l, LV.Storable l)
             => LitVector l -> LitVector l -> l
lUnsignedGt = flip lUnsignedLt

-- | @lUnsignedGeq x y@ returns predicate indicating if x is greater than
-- or equal to y.
lUnsignedGeq :: (?be :: BitEngine l, LV.Storable l)
             => LitVector l -> LitVector l -> l
lUnsignedGeq = flip lUnsignedLeq
