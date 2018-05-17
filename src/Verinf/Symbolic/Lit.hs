{- |
Module           : Verinf.Symbolic.Lit
Description      :
License          : BSD3
Stability        : stable
Point-of-contact : jhendrix
-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE CPP #-}
-- This module provides operations on literals and data structures for word level operations on them.
module Verinf.Symbolic.Lit
  ( module Verinf.Symbolic.Lit.DataTypes
  -- | Bit level operations.
  , beAndM
  , beImplies
  , beEq
  , beIte
  , beLitFromBool
  , beOr
  , beOrM
  , beLazyMux
  -- | Lit vector operations
  , beFullAddInt
  , beAddInt
  , beAddIntConstant
  , beAndInt
  , beEqVector
  , beIteVector
  , beMuxGeneral
  , beMulInt
  , beFullMulIntConstant
  , beNegInt
  , beOrInt
  , beQuot
  , beQuotRem
  , beQuotRemUnsigned
  , beQuotUnsigned
  , beRem
  , beRemUnsigned
  , beSext
  , beShiftLeftC
  , beShiftRightC
  , beShl
  , beSignedLeq
  , beSignedLt
  , beSignedShr
  , beSubInt
  , beTrunc
  , beUnsignedLeq
  , beUnsignedLt
  , beUnsignedShr
  , beVectorFromInt
  , beVectorFromLit
  , beVectorToMaybeInt
  , beXorInt
  , beZext
  ) where

-- Imports {{{1
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import qualified Control.Exception as CE
import Data.Bits (shiftL, testBit)
import qualified Data.Vector.Storable as LV
import qualified Data.Vector.Storable.Mutable as LV (new,write)

import Verinf.Symbolic.Lit.DataTypes
import Verinf.Symbolic.Lit.Functional

-- Bit-level operations {{{1

-- | Returns literal associated with Boolean value.
beLitFromBool :: BitEngine l -> Bool -> l
beLitFromBool be = let ?be = be in lFromBool

-- | Short-circuit monadic implementation of beAnd.
beAndM :: BitEngine l -> IO l -> IO l -> IO l
beAndM be mx my = do
  lx <- mx
  case () of
    _ | beEqLit be lx (beFalse be) -> return (beFalse be)
      | beEqLit be lx (beTrue be) -> my
      | otherwise -> beAnd be lx =<< my

beImplies :: BitEngine l -> l -> l -> IO l
beImplies be x y = return $ let ?be = be in lNeg (x `lAnd` lNeg y)

beOr :: BitEngine l -> l -> l -> IO l
beOr be x y = return $ let ?be = be in lOr x y

beOrM :: BitEngine l -> IO l -> IO l -> IO l
beOrM be x y = beNeg be <$> beAndM be (beNeg be <$> x) (beNeg be <$> y)

-- | @beIte c t f@ returns @c ? t : f@
beIte :: BitEngine l -> l -> l -> l -> IO l
beIte = beMux

beEq :: BitEngine l -> l -> l -> IO l
beEq be x y = beNeg be <$> beXor be x y

-- | Lifts a strict mux operation to a lazy mux
beLazyMux :: BitEngine l -> (l -> a -> a -> IO a) -> l -> IO a -> IO a -> IO a
beLazyMux be muxFn c tm fm
  | beEqLit be c (beTrue be) = tm
  | beEqLit be c (beFalse be) = fm
  | otherwise = do
      t <- tm
      f <- fm
      muxFn c t f

-- LitVector operations {{{1

-- | Returns a lit vector with the given bitwidth from the integer.
beVectorFromInt :: LV.Storable l => BitEngine l -> Int -> Integer -> LitVector l
beVectorFromInt be w v = LV.map (beLitFromBool be . testBit v) $ LV.enumFromN 0 w

beVectorFromLit :: LV.Storable l => BitEngine l -> l -> LitVector l
beVectorFromLit _be = LV.replicate 1

-- | Returns an (w, constant integer) tuple if the given BitTerm corresponds to
-- a constant value, and Nothing if the lit vector is a symbolic value.  The
-- lower w bits of the constant integer represent the bit vector.
beVectorToMaybeInt :: (Eq l, LV.Storable l) => BitEngine l -> LitVector l -> Maybe (Int, Integer)
beVectorToMaybeInt be v = (\val -> (LV.length v, val)) <$> lGetUnsigned v
  where ?be = be

-- Bitwise operations {{{2

-- | Compares two lit vectors and returns lit if they are bitwise equal.
beEqVector :: (LV.Storable l)
           => BitEngine l -> LV.Vector l -> LV.Vector l -> IO l
beEqVector be vx vy =
  CE.assert (LV.length vx == LV.length vy) $ do
    lits <- LV.mapM (\i -> beEq be (vx LV.! i) (vy LV.! i)) $
            LV.enumFromN 0 (LV.length vx)
    LV.foldM (beAnd be) (beTrue be) lits

beAndInt :: (LV.Storable l)
         => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beAndInt = LV.zipWithM . beAnd

beOrInt :: (LV.Storable l)
         => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beOrInt = LV.zipWithM . beOr

beXorInt :: (LV.Storable l)
         => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beXorInt = LV.zipWithM . beXor

beIteVector :: (LV.Storable l)
            => BitEngine l
            -> l
            -> IO (LitVector l)
            -> IO (LitVector l)
            -> IO (LitVector l)
beIteVector be = beLazyMux be (LV.zipWithM . beMux be)

{-# DEPRECATED beMuxGeneral "Use lMuxInteger" #-}
-- | @beMuxGeneral mergeFn valueFn maxValue lv@ processes the bits in @lv@ and
-- returns a circuit whose result is @valueFn v@ when @lv@ has value @v@.
beMuxGeneral :: LV.Storable l
             => (l -> a -> a -> a)
             -> Integer -- ^ Maximum value vector may take (value is undefined for larger vectors)
             -> LV.Vector l
             -> (Integer -> a)
             -> a
beMuxGeneral = lMuxInteger

{-# DEPRECATED beShiftLeftC "Use lShlC" #-}
beShiftLeftC :: LV.Storable l => LitVector l -> l -> Int -> LitVector l
beShiftLeftC = lShlC

{-# DEPRECATED beShiftRightC "Use lShrC" #-}
beShiftRightC :: LV.Storable l => LitVector l -> l -> Int -> LitVector l
beShiftRightC = lShrC

-- | Shift bit vector to left.
beShl :: (LV.Storable l)
     => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beShl be x y = return $ let ?be = be in lShl x y

-- | Shift bit vector to left.
beSignedShr :: (LV.Storable l)
     => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beSignedShr be x y = return $ let ?be = be in lSignedShr x y

-- | Shift bit vector to left.
beUnsignedShr :: (LV.Storable l)
     => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beUnsignedShr be x y = return $ let ?be = be in lUnsignedShr x y

-- Arithmetic operations {{{2

-- | Add to bit vector to integer.
beAddIntConstant :: (LV.Storable l)
      => BitEngine l -> LitVector l -> Integer -> IO (LitVector l)
beAddIntConstant be vx vy = do
  let n = LV.length vx
  m <- LV.new n
  let adderStepM c i
        | i == n = return ()
        | otherwise = do
          let a = vx `LV.unsafeIndex` i
          let b = vy `testBit` i
          ac <- beAnd be a c
          negAnegC <- beAnd be (beNeg be a) (beNeg be c)
          aEqC <- beOr be ac negAnegC
          if b
            then do
              LV.write m i aEqC
              adderStepM (beNeg be negAnegC) (i+1)
            else do
              LV.write m i (beNeg be aEqC)
              adderStepM ac (i+1)
  adderStepM (beFalse be) 0
  LV.freeze m

-- | Truncate the given bit vector to the specified length
beTrunc :: (LV.Storable l)
     => BitEngine l -> Int -> LitVector l -> LitVector l
beTrunc _be w vx = CE.assert (LV.length vx >= w) $ LV.take w vx

-- | Perform unsigned extension to given number of bits.
beZext :: (LV.Storable l)
       => BitEngine l -> Int -> LitVector l -> LitVector l
beZext be w vx = CE.assert (LV.length vx <= w) $
  vx LV.++ LV.replicate (w - LV.length vx) (beFalse be)

beSext :: (LV.Storable l)
     => BitEngine l -> Int -> LitVector l -> LitVector l
beSext _be w vx = CE.assert (LV.length vx <= w) $
  vx LV.++ LV.replicate (w - LV.length vx) (LV.last vx)

-- | Add two bit vectors together, returnning vector and overflow.
beFullAddInt :: (LV.Storable l)
             => BitEngine l -> LitVector l -> LitVector l -> IO (l,LitVector l)
beFullAddInt be vx vy =
  CE.assert (LV.length vx == LV.length vy) $ do
    let n = LV.length vx
    m <- LV.new n
    let adderStepM c i
          | i == n = return c
          | otherwise = do
            let a = vx `LV.unsafeIndex` i
            let b = vy `LV.unsafeIndex` i
            ab <- beAnd be a b
            abN <- beAnd be (beNeg be a) (beNeg be b)
            cp <- beAnd be c (beNeg be abN)
            c' <- beOr be ab cp
            abEq <- beOr be ab abN
            r  <- beEq be c abEq
            LV.write m i r
            adderStepM c' (i+1)
    c <- adderStepM (beFalse be) 0
    r <- LV.freeze m
    return (c,r)

beAddInt :: (LV.Storable l)
         => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beAddInt be vx vy = return (snd (let ?be = be in lFullAdd vx vy))

beNegInt :: (LV.Storable l) => BitEngine l -> LitVector l -> IO (LitVector l)
beNegInt be vx = return $ let ?be = be in lNegate vx

-- | Subtract one bitvector from another
beSubInt :: (LV.Storable l)
         => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beSubInt be vx vy = return (snd (let ?be = be in lFullSub vx vy))

-- | Returns the product of two lit vectors.
beMulInt :: (LV.Storable l)
         => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beMulInt be vx vy = CE.assert (LV.length vx == LV.length vy)
                  $ mulStepM 0 (LV.replicate n lf)
  where lf = beFalse be
        n = LV.length vx
        mulStepM i r
          | i == n = return r
          | otherwise = mulStepM (i+1) =<< beIteVector be (vy LV.! i) nextIntM (return r)
          where nextIntM = beAddInt be r (beShiftLeftC vx lf i)

-- | Returns the product of a vector and integer.  This is a full multiplier
-- that does not require the inputs have the same length.
beFullMulIntConstant :: (LV.Storable l)
                     => BitEngine l
                     -> LitVector l
                     -> (Int, Integer)
                     -> IO (LitVector l)
beFullMulIntConstant be vx (ly, y) = mulStepM 0 (LV.replicate ly lf)
 where lf = beFalse be
       mulStepM i r
         | i == LV.length vx = return r
         | otherwise = mulStepM (i+1) =<< beIteVector be (vx LV.! i) nextIntM (return rext)
         where rext = LV.snoc r lf
               nextIntM = beAddIntConstant be rext (y `shiftL` i)

-- | (a `beQuotRem` b) bit-blasts (a `quotRem` b).
--
-- TODO: This isn't the most efficient implementation, because it
-- essentially undoes things that beQuotRem does, but it works.
beQuotRemUnsigned :: (LV.Storable l) =>
                     BitEngine l -> LitVector l -> LitVector l
                  -> IO (LitVector l, LitVector l)
beQuotRemUnsigned be dividend divisor = do
  (q, r) <- beQuotRem be dividend' divisor'
  return (LV.take n q, LV.take n r)
  where dividend' = LV.snoc dividend (beFalse be)
        divisor' = LV.snoc divisor (beFalse be)
        n = LV.length dividend

beQuotUnsigned :: (LV.Storable l)
               => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beQuotUnsigned be x y = fst <$> beQuotRemUnsigned be x y

beRemUnsigned :: (LV.Storable l)
              => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beRemUnsigned be x y = snd <$> beQuotRemUnsigned be x y

-- | (a `beQuotRem` b) bit-blasts (a `quotRem` b).
-- TODO: Check that semantics matches both LLVM and Java expected semantics with signed numbers.
beQuotRem :: (LV.Storable l)
           => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l, LitVector l)
beQuotRem be dividend' divisor' = do
  CE.assert (LV.length dividend' == LV.length divisor') $ do
    let lf = beFalse be
    let n = LV.length dividend'
    -- Perform quotRem on the absolute value of the operands.  Then, negate the
    -- quotient if the signs of the operands differ and make the sign of a nonzero
    -- remainder to match that of the dividend.
    dividend <- absLsbf dividend'
    divisor <- absLsbf divisor'
    let divStep rrOrig _i = do
          let q = LV.take n rrOrig
          let r = LV.drop n rrOrig
          -- Subtract the divisor from the left half of the "remainder register"
          s <- beSubInt be r divisor
          let negRem = beShiftLeftC rrOrig (beFalse be) 1 -- rem < 0, orig rr's quot lsl'd w/ 0
              posRem = beShiftLeftC (q LV.++ s) (beTrue be) 1   -- rem >= 0, new rr's quot lsl'd w/ 1
          beIteVector be (signLitLsbf s) (return negRem) (return posRem)
    -- Given an n-bit dividend and divisor, 'initial' is the starting value of
    -- the 2n-bit "remainder register" that carries both the quotient and remainder;
    let initial = dividend LV.++ LV.replicate n (beFalse be)
    res <- LV.foldM divStep (beShiftLeftC initial lf 1) (LV.enumFromN (0::Int) n)
    let q = LV.take n res
    let r = LV.drop n res
    q' <- q `negWhenM` beXor be dsign (signLitLsbf divisor')
    r' <- beShiftRightC r lf 1 `negWhenM` (return dsign)
    CE.assert (LV.length r' == n) $
      CE.assert (LV.length q' == n) $
        return (q', r')
  where
    dsign              = signLitLsbf dividend'
    il `negWhenM` lM   = lM >>= \l -> beIteVector be l (beNegInt be il) (return il)
    absLsbf x          = beIteVector be (signLitLsbf x) (beNegInt be x) (return x)
    signLitLsbf        = LV.last

beQuot :: (LV.Storable l)
       => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beQuot be x y = fst <$> beQuotRem be x y

beRem :: (LV.Storable l)
      => BitEngine l -> LitVector l -> LitVector l -> IO (LitVector l)
beRem be x y = snd <$> beQuotRem be x y

-- Arithmetic comparison operations {{{2

-- | @litLtImpl ifEq lx ly ifEq@ returns true if lx < ly or lx == ly & ifEq is
-- True.
-- @lx@ and @ly@ are expected to be in least-significant bit first order.
beUnsignedLtImpl :: (LV.Storable l) => Bool -> BitEngine l -> LitVector l -> LitVector l -> IO l
beUnsignedLtImpl ifEq be lx ly = do
  let cmpStep r i = do
        let x = lx `LV.unsafeIndex` i
            y = ly `LV.unsafeIndex` i
        beAndM be
               (beOr be (beNeg be x) y)
               (beOrM be (beAnd be (beNeg be x) y) (return r))
  LV.foldM cmpStep (beLitFromBool be ifEq) $
    LV.enumFromN 0 (LV.length lx)

-- | @beSignedLt ifEq lx ly ifEq@ returns true if lx < ly or lx == ly & ifEq
-- is True.
beSignedLtImpl :: (LV.Storable l)
               => Bool -> BitEngine l -> LitVector l -> LitVector l -> IO l
beSignedLtImpl ifEq be vx vy =
  let l = LV.length vx - 1
      x = LV.last vx
      y = LV.last vy
   in beOrM be
            -- Return true if x is negative and y is positive
            (beAnd be x (beNeg be y))
            -- Return false if x is negative and y is positive.
            (beAndM be
                    (beNeg be <$> beAnd be (beNeg be x) y)
                    (beUnsignedLtImpl ifEq be (LV.slice 0 l vx) (LV.slice 0 l vy)))

-- | Performed signed less than or equal comparison.
beSignedLeq :: (LV.Storable l)
            => BitEngine l -> LitVector l -> LitVector l -> IO l
beSignedLeq = beSignedLtImpl True

-- | Performed signed less than comparison.
beSignedLt :: (LV.Storable l)
           => BitEngine l -> LitVector l -> LitVector l -> IO l
beSignedLt = beSignedLtImpl False

-- | Performed unsigned less than or equal comparison.
beUnsignedLeq :: (LV.Storable l)
              => BitEngine l -> LitVector l -> LitVector l -> IO l
beUnsignedLeq = beUnsignedLtImpl True

-- | Performed unsigned less than comparison.
beUnsignedLt :: (LV.Storable l)
             => BitEngine l -> LitVector l -> LitVector l -> IO l
beUnsignedLt = beUnsignedLtImpl False
