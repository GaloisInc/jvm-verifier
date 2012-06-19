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
import Data.Word
import Numeric

import Execution
import Verifier.Java.Parser
import Verinf.Symbolic

-- | Returns true if type is an integer value.
isIValue :: Type -> Bool
isIValue BooleanType = True
isIValue ByteType    = True
isIValue CharType    = True
isIValue IntType     = True
isIValue ShortType   = True
isIValue _           = False

-- | Returns true if type is a reference value.
isRValue :: Type -> Bool
isRValue (ArrayType _) = True
isRValue (ClassType _) = True
isRValue _             = False

headf :: [a] -> (a -> a) -> [a]
headf [] _     = error "headf: empty list"
headf (x:xs) f = f x : xs

runStaticMethod_ :: JavaSemantics m => String -> String -> String -> [JSValue m] -> m ()
runStaticMethod_ cName methName methodType args =
  runStaticMethod cName methName methodType args >> return ()

runStaticMethod :: JavaSemantics m => String -> String -> String -> [JSValue m] -> m (JSRslt m)
runStaticMethod cName methName methodType args = do
  invokeStaticMethod
    cName
    (makeMethodKey methName methodType)
    args
  run

runMain :: JavaSemantics m => String -> [JSValue m] -> m (JSRslt m)
runMain cName args =
  runStaticMethod cName "main" "([Ljava/lang/String;)V" args

floatRem :: (RealFrac a) => a -> a -> a
floatRem x y = fromIntegral z
  where z :: Integer
        z = truncate x `rem` truncate y

boolSeqToInt8 :: [Bool] -> Int32
boolSeqToInt8 bs = toEnum . fromEnum . head $ ns
  where ns :: [Int8]
        ns = hexToNumSeq 2 . boolSeqToHex $ bs

boolSeqToInt32 :: [Bool] -> Int32
boolSeqToInt32 = head . hexToIntSeq . boolSeqToHex

boolSeqToInt64 :: [Bool] -> Int64
boolSeqToInt64 = head . hexToLongSeq . boolSeqToHex

boolSeqToHex :: [Bool] -> String
boolSeqToHex bs = reverse (impl bs)
  where fn b i = if b then i else 0
        ch [x0, x1, x2, x3] = intToDigit (fn x3 8 + fn x2 4 + fn x1 2 + fn x0 1)
        ch _ = error "internal: 'char': unexpected input length"
        impl [] = []
        impl s = let (first',s1)  = splitAt 4 s
                     (second',s2) = splitAt 4 s1
                  in ch first' : ch second' : impl s2

hexToByteSeq :: String -> [Int32]
hexToByteSeq (x : y : r)
  = fromIntegral (16 * (digitToInt x) + (digitToInt y)) : hexToByteSeq r
hexToByteSeq [] = []
hexToByteSeq _ = error "internal: hexToByteSeq: invalid input string"

outsToInts8 :: Int -> [Bool] -> [Int32]
outsToInts8 n outs  = [ boolSeqToInt8 $ take 8 (drop (8*k) outs) | k <- [0..(n-1)] ]

outsToInts32 :: Int -> [Bool] -> [Int32]
outsToInts32 n outs  = [ boolSeqToInt32 $ take 32 (drop (32*k) outs) | k <- [0..(n-1)] ]

outsToInts64 :: Int -> [Bool] -> [Int64]
outsToInts64 n outs  = [ boolSeqToInt64 $ take 64 (drop (64*k) outs) | k <- [0..(n-1)] ]

hexToBoolSeq :: String -> [Bool]
hexToBoolSeq s =
  let ch c = map (testBit $ digitToInt c) [0..3]
      loop (x : y : rest) = ch x ++ ch y ++ loop rest
      loop [] = []
      loop _ = error "hexToBoolSeq: invalid string"
   in loop $ reverse s

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

hexToLongSeq :: String -> [Int64]
hexToLongSeq = reverse . impl
  where impl xs@(_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:r) =
          foldr (+) 0 [ dTo64 xi `shiftL` bi
                      | (xi, bi) <- xs `zip` reverse [4*i|i<-[0..15]]
                      ]
          : impl r
        impl [] = []
        impl _  = error "internal: hexToLongSeq invalid input string"
        dTo64   = fromIntegral . digitToInt

hexToNumSeq :: (Bits a) => Int -> String -> [a]
hexToNumSeq n = reverse . impl
  where impl xs | length xs >= n =
          foldr (+) 0 [ dToNum xi `shiftL` bi
                      | (xi, bi) <- xs `zip` reverse [4*i|i<-[0..n]]
                      ]
          : impl (drop n xs)
        impl [] = []
        impl _  = error "internal: hexToNumSeq invalid input string"
        dToNum  = fromIntegral . digitToInt
        
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