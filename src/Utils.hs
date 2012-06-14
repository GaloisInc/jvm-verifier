{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}
{-# LANGUAGE ViewPatterns #-}
module Utils where

import Verinf.Symbolic

import Execution
import JavaParser

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