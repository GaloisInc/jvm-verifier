{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix
-}
{-# LANGUAGE ViewPatterns #-}
module Verifier.Java.Utils where

import Data.Bits
import Data.Char
import Data.Int
import Data.List (foldl')
import Data.Word
import Numeric

import Verinf.Symbolic

boolSeqToValue :: (Bits a, Num a) => [Bool] -> a
boolSeqToValue bs = foldl' (.|.) 0  $ zipWith (\b i -> if b then bit i else 0) bs [0..]

splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n l = h : splitN n r
  where (h,r) = splitAt n l

boolSeqToHex :: [Bool] -> String
boolSeqToHex bs = reverse (impl bs)
  where fn b i = if b then i else 0
        ch [x0, x1, x2, x3] = intToDigit (fn x3 8 + fn x2 4 + fn x1 2 + fn x0 1)
        ch _ = error "internal: 'char': unexpected input length"
        impl [] = []
        impl s = let (first',s1)  = splitAt 4 s
                     (second',s2) = splitAt 4 s1
                  in ch first' : ch second' : impl s2

byteSeqToHex :: [CValue] -> String
byteSeqToHex ((getSValW -> Just (32, c)) : r)
  = (intToDigit $ fromIntegral $ ((fromIntegral c :: Word32) `quot` 16) `rem` 16)
    : (intToDigit $ fromIntegral $ (fromIntegral c :: Word32) `rem`  16)
    : byteSeqToHex r
byteSeqToHex ((getSValW -> Just (w,_c)) : _r)
  = error $ "internal: byteSeqToHex unexpected width " ++ show w
byteSeqToHex (CArray _ : _) = error "internal: byteSeqToHex CArray"
byteSeqToHex (CBool _ : _) = error "internal: byteSeqToHex CBool"
byteSeqToHex (CRec{} : _) = error "internal: byteSeqToHex CRec"
byteSeqToHex [] = []
byteSeqToHex _ = error "internal: byteSeqToHex bad value"

hexToBoolSeq :: String -> [Bool]
hexToBoolSeq s =
  let ch c = map (testBit $ digitToInt c) [0..3]
      loop (x : y : rest) = ch x ++ ch y ++ loop rest
      loop [] = []
      loop _ = error "hexToBoolSeq: invalid string"
   in loop $ reverse s

hexToByteSeq :: String -> [Int32]
hexToByteSeq (x : y : r)
  = fromIntegral (16 * (digitToInt x) + (digitToInt y)) : hexToByteSeq r
hexToByteSeq [] = []
hexToByteSeq _ = error "internal: hexToByteSeq: invalid input string"

hexToNumSeq :: (Bits a, Num a) => Int -> String -> [a]
hexToNumSeq n = reverse . impl
  where impl xs | length xs >= n =
          foldr (+) 0 [ dToNum xi `shiftL` bi
                      | (xi, bi) <- xs `zip` reverse [4*i|i<-[0..n]]
                      ]
          : impl (drop n xs)
        impl [] = []
        impl _  = error "internal: hexToNumSeq invalid input string"
        dToNum  = fromIntegral . digitToInt
        
hexToIntSeq :: String -> [Int32]
hexToIntSeq = reverse . impl
  where impl (x0 : x1 : x2 : x3 : x4 : x5 : x6 : x7 : r)
         = fromIntegral (  (digitToInt x0) `shiftL` 28
                         + (digitToInt x1) `shiftL` 24
                         + (digitToInt x2) `shiftL` 20
                         + (digitToInt x3) `shiftL` 16
                         + (digitToInt x4) `shiftL` 12
                         + (digitToInt x5) `shiftL`  8
                         + (digitToInt x6) `shiftL`  4
                         + (digitToInt x7))
           : impl r
        impl [] = []
        impl _ = error "internal: hexToIntSeq invalid input string"
        
intToHex :: CValue -> String
intToHex (getSValW -> Just (32, c)) =
  let r = showHex (fromIntegral c :: Word32) ""
   in replicate (8 - length r) '0' ++ r
intToHex (getSValW -> Just (64, c)) =
  let r = showHex (fromIntegral c :: Word64) ""
   in replicate (16 - length r) '0' ++ r
intToHex _ = error $ "internal: Undefined intToHex for type"

intSeqToHex :: [CValue] -> String
intSeqToHex = foldl (\s c -> intToHex c ++ s) []

intSeqToBoolSeq :: [CValue] -> [Bool]
intSeqToBoolSeq = hexToBoolSeq . intSeqToHex