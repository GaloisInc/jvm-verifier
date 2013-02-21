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
import Control.Monad.Error
import Control.Monad.Trans
import Data.Char
import System.Console.CmdArgs.Implicit hiding (verbosity, setVerbosity)
import System.Directory
import System.Environment (getArgs)
import System.Environment.Executable (getExecutablePath)
import System.Exit
import System.IO
import System.FilePath
import Text.ParserCombinators.Parsec
import Prelude hiding (catch)

import Text.PrettyPrint hiding (char)

import Language.JVM.CFG

import Execution
import Data.JVM.Symbolic.Translation
import Verifier.Java.Codebase
import Verifier.Java.Simulator
import Verifier.Java.WordBackend
import Overrides

import Verinf.Utils.LogMonad

#ifdef UseSAW
import Verifier.Java.SAWBackend
#else
import Verifier.Java.WordBackend
#endif

simExcHndlr' :: Backend sbe -> Bool -> Doc -> InternalExc sbe m -> Simulator sbe m ()
simExcHndlr' sbe suppressOutput failMsg exc =
  case exc of
    epe@ErrorPathExc{} -> liftIO $
      unless suppressOutput . hPutStr stderr . render 
        $ failMsg <> colon <+> ppInternalExc epe
    unk -> error . render $ ppInternalExc unk

dumpSymASTs :: Codebase -> String -> IO ()
dumpSymASTs cb cname = do
  mc <- tryLookupClass cb cname
  case mc of
    Just c -> mapM_ (dumpMethod c) $ classMethods c
    Nothing -> putStrLn $ "Main class " ++ cname ++ " not found."
  where ppInst' (pc, i) = show pc ++ ": " ++ ppInst i
        ppSymInst' (mpc, i) =
          maybe "" (\pc -> show pc ++ ": ") mpc ++ render (ppSymInsn i)
        dumpMethod c m =
          case methodBody m of
            Code _ _ cfg _ _ _ _ -> do
              putStrLn ""
              putStrLn . className $ c
              putStrLn . show . methodKey $ m
              putStrLn ""
              mapM_ (putStrLn . ppInst') . concatMap bbInsts $ allBBs cfg
              putStrLn ""
              mapM_ dumpBlock . fst $ liftCFG cfg
            _ -> return ()
        dumpBlock b = do
          putStrLn . render . ppBlockId . sbId $ b
          mapM_ (\i -> putStrLn $ "  " ++ ppSymInst' i) $ sbInsns b

data JSS = JSS
  { classpath :: String
  , jars      :: String
  , opts      :: String
  , blast     :: Bool
  , xlate     :: Bool
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
        , xlate  = def &= help "Print the symbolic AST translation stdout and terminate"
        , mcname = def &= typ "MAIN CLASS NAME" &= args
        }
    &= summary ("Java Symbolic Simulator (jss) 0.1d Jan 2012. "
                ++ "Copyright 2010-2012 Galois, Inc. All rights reserved.")

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
  when (xlate args') $ do
    dumpSymASTs cb cname
--    dumpSymASTs cb "java/lang/String"
--    dumpSymASTs cb "java/lang/String$CaseInsensitiveComparator"
    
    exitSuccess
  withFreshBackend $ \sbe -> do
   let fl = defaultSimFlags{ alwaysBitBlastBranchTerms = blast args' }
   let go = do tl <- liftIO $ termInt sbe (fromIntegral (length jopts))
               jssOverrides
               setVerbosity (dbug args')
               rs <- runMain cname =<< do
                 jargs <- newMultiArray (ArrayType (ClassType "java/lang/String")) [tl]
                 forM_ ([0..length jopts - 1] `zip` jopts) $ \(i,s) -> do
                   r <- RValue <$> refFromString s
                   ti <- liftIO $ termInt sbe (fromIntegral i)
                   setArrayValue jargs ti r
                 return [RValue jargs]
               when (length rs > 1) $
                 -- As long as we filter out exception paths and our merging is working,
                 -- this warning shouldn't fire.
                 liftIO $ putStrLn $ "Warning: Simulator could not merge all paths."
   runSimulator cb sbe defaultSEH (Just fl) $ 
     go `catchError` \e -> do
                       simExcHndlr' sbe False (text "jss") e
                       liftIO . exitWith $ ExitFailure 1
