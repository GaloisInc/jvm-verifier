{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : lerkok
-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import System.Console.CmdArgs
import System.Exit
import Text.PrettyPrint.HughesPJ

import Language.JVM.Parser

data JVMGraphOpts =
  JVMGraphOpts {
    jvmClassFile :: FilePath
  , jvmMethodName :: String
  } deriving (Show, Data, Typeable)


defaultOpts :: JVMGraphOpts
defaultOpts = JVMGraphOpts
  { jvmClassFile = def &= argPos 0 &= typ "<class file>"
  , jvmMethodName = def &= argPos 1 &= typ "<method name>"
  } &= program "jvmgraph"
    &= summary "Utility for printing out control-flow-graph of a JVM method"

main :: IO ()
main = do
  opts <- cmdArgs defaultOpts
  cl <- loadClass (jvmClassFile opts)
  let mn = jvmMethodName opts
  m <- case filter (\m -> methodName m == mn) (classMethods cl) of
         [] -> do
           putStrLn $ render $ 
             text "Could not find method" <+> quotes (text mn) <> text ".\n" $$
             text "Methods in" <+> text (className cl) <+> text "include:" $$
               vcat [nest 2 (text (methodName m)) | m <- classMethods cl]
           exitFailure
         _:_:_ -> do
           putStrLn $ "Found multiple methods named " ++ show mn ++ "."
           exitFailure
         [m] -> return m
  case methodBody m of
    AbstractMethod -> do
      putStrLn $ "Method " ++ show mn ++ " is abstract."
      exitFailure
    NativeMethod -> do
      putStrLn $ "Method " ++ show mn ++ " is native."
      exitFailure
    Code _ _ cfg et _ _ _ -> putStrLn (cfgToDot et cfg mn)
