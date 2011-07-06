{- |
Module           : $Header$
Description      : The command line driver for the Java Symbolic Simulator
Stability        : stable
Point-of-contact : jstanley
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import Control.Applicative hiding (many)
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Char
import System.Console.CmdArgs.Implicit hiding (verbosity)
import System.Directory
import System.Environment (getArgs)
import System.Environment.Executable (getExecutablePath)
import System.Exit
import System.FilePath
import Text.ParserCombinators.Parsec

import Execution
import JavaParser.Common
import Simulation
import Utils
import Utils.Simulation

import Verinf.Symbolic
import Verinf.Utils.CatchMIO

data JSS = JSS
  { classpath :: String
  , jars      :: String
  , opts      :: String
  , blast     :: Bool
  , dbug      :: Int
  , mcname    :: Maybe String
  } deriving (Show, Data, Typeable)

main :: IO ()
main = do
  numArgs <- length <$> getArgs
  when (numArgs == 0) $ do
    putStrLn $ "jss: No command line options given.  Try --help for more information."
    exitFailure
  args' <- cmdArgs $
    JSS { classpath = def
                   &= typ "CLASSPATH"
#ifdef mingw32_HOST_OS
                   &= help "semicolon-delimited list of auxiliary class file directories"
#else
                   &= help "colon-delimited list of auxiliary class file directories"
#endif
        , jars = def
              &= typ "JARS"
#ifdef mingw32_HOST_OS
              &= help "semicolon-delimited list of paths to jar files (e.g. --jars=rt.jar;foo.jar)"
#else
              &= help "colon-delimited list of paths to jar files (e.g. --jars=jdk1.6/classes.jar:foo.jar)"
#endif
        , opts = def
              &= typ "\"ARGS\""
              &= help ("space-delimited arguments to be passed to main()"
                       ++ " (use --help for more info)")

        , blast  = def &= help "Always bitblast symbolic condition terms at branches (may force symbolic termination)"
        , dbug   = def &= opt "0" &= help "Debug verbosity level (0-5)"
        , mcname = def &= typ "MAIN CLASS NAME" &= args
        }
    &= summary ("Java Symbolic Simulator (jss) 0.1c Jan 2011. "
                ++ "Copyright 2010-2011 Galois, Inc. All rights reserved.")

  let
    cname  = case mcname args' of
               Nothing -> error "Please provide the name of a class containing main()"
               Just x  -> x
#ifdef mingw32_HOST_OS
    cpaths = case runParser (delimited ';') () "classpath" (classpath args') of
               Left _e -> error "Unable to parse semicolon-delimited CLASSPATH."
               Right x -> x
    jpaths = case runParser (delimited ';') () "jars" (jars args') of
               Left _e -> error "Unable to parse semicolon-delimited list of jar files"
               Right x -> x
#else
    cpaths = case runParser (delimited ':') () "classpath" (classpath args') of
               Left _e -> error "Unable to parse colon-delimited CLASSPATH."
               Right x -> x
    jpaths = case runParser (delimited ':') () "jars" (jars args') of
               Left _e -> error "Unable to parse colon-delimited list of jar files"
               Right x -> x
#endif
    jopts  = case runParser (many $ eatWS $ many1 $ satisfy $ not . isSpace)
               () "java args" (opts args')
             of
               Left _e -> error "Unable to parse Java command line arguments"
               Right x -> x
    eatWS        = between spaces spaces
    delimited ch = many1 (noneOf [ch]) `sepBy` char ch


  jpaths' <-
    if elem "galois.jar" $ map takeFileName jpaths
      then return jpaths
      else do
        exeDir   <- takeDirectory <$> getExecutablePath
        contents <- getDirectoryContents exeDir
        unless (elem "galois.jar" contents) $ do
          putStrLn $ "Error: Unable to locate the file 'galois.jar'"
          putStrLn $ "Expected to find it in " ++ exeDir ++ "."
          putStrLn $ "You may specify an alternate location via the -j switch."
          exitFailure
        return $ (exeDir </> "galois.jar") : jpaths

  cb <- loadCodebase jpaths' cpaths

  runSymbolic $
    let fl = defaultSimFlags{ alwaysBitBlastBranchTerms = blast args' }
        go = runSimulator' fl cb $ do
               Simulation.setVerbosity (dbug args')
               rs <- runMain cname =<< do
                       jargs <- newMultiArray (ArrayType (ClassType "java/lang/String"))
                                              [mkCInt (Wx 32) $ fromIntegral $ length jopts]
                       forM_ ([0..length jopts - 1] `zip` jopts) $ \(i,s) ->
                         setArrayValue jargs (mkCInt (Wx 32) . fromIntegral $ i)
                           =<< RValue <$> refFromString s
                       return [RValue jargs]

               when (length (withoutExceptions rs) > 1) $
                 -- As long as we filter out exception paths and our merging is working,
                 -- this warning shouldn't fire.
                 liftIO $ putStrLn $ "Warning: Simulator could not merge all paths."
    in
      go `catchMIO` \e -> do
        case fromException e of
          Just ExitSuccess     -> liftIO $ exitSuccess
          Just c@ExitFailure{} -> liftIO $ exitWith c
          _ -> simExcHndlr' False "jss" e >> liftIO (exitWith $ ExitFailure 1)
