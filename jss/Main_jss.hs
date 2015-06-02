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

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative hiding (many)
#endif
import Control.Lens ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Function
import Data.List
import System.Console.CmdArgs.Implicit hiding (verbosity, setVerbosity)
import System.Directory
import System.Environment (getArgs)
import System.Environment.Executable (getExecutablePath)
import System.Exit
import System.IO
import System.FilePath (searchPathSeparator, splitSearchPath, takeDirectory, takeFileName, (</>))
import Text.ParserCombinators.Parsec

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import Text.PrettyPrint hiding (char)

import Language.JVM.CFG
import Language.JVM.Parser

import Execution.JavaSemantics
import Data.JVM.Symbolic.Translation
import Verifier.Java.Codebase
import Verifier.Java.Debugger
import Verifier.Java.Simulator
import qualified Verifier.Java.WordBackend as W
import qualified Verifier.Java.SAWBackend as S
import Overrides

simExcHndlr' :: Bool -> Doc -> InternalExc sbe m -> Simulator sbe m ()
simExcHndlr' suppressOutput failMsg exc =
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
              mapM_ dumpBlock . sortBy (compare `on` sbId) . fst $ liftCFG cfg
            _ -> return ()
        dumpBlock b = do
          putStrLn . render . ppBlockId . sbId $ b
          mapM_ (\i -> putStrLn $ "  " ++ ppSymInst' i) $ sbInsns b

data JSS = JSS
  { classpath     :: String
  , jars          :: String
  , opts          :: String
  , blast         :: Bool
  , sat           :: Bool
  , xlate         :: Bool
  , errPaths      :: Bool
  , dbug          :: Int
  , mcname        :: Maybe String
  , startDebugger :: Bool
  , useSaw        :: Bool
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

                   &= help ("A " ++ [searchPathSeparator] ++
                      "-delimited list of auxiliary class file directories")
        , jars = def
              &= typ "JARS"
              &= help ("A " ++ [searchPathSeparator] ++
                 "-delimited list of jar files (e.g. '--jars=rt.jar" ++
                 [searchPathSeparator] ++ "foo.jar')")
        , opts = def
              &= typ "\"ARGS\""
              &= help ("The space-delimited arguments to be passed to the Java main()"
                       ++ " (use '--opts=--help' for more info)")

        , blast  = def &= help "Always bitblast symbolic condition terms at branches (may force symbolic termination)"
        , sat = def &= help "Always check satisfiability of symbolic path assertions at branches (subsumes bitblasting)"
        , errPaths = def &= help "Print details of symbolic execution paths that end in errors"
        , dbug   = def &= opt "0" &= help "Debug verbosity level (0-6)"
        , xlate  = def &= help "Print the symbolic AST translation stdout and terminate"
        , mcname = def &= typ "MAIN CLASS NAME" &= args
        , startDebugger = def &= help "Break and enter the JSS debugger when running main method"
        , useSaw = def &= help "Use SAWCore backend instead of default word backend"
        }
    &= summary ("Java Symbolic Simulator (jss) 0.4 September 2013. "
                ++ "Copyright 2010-2013 Galois, Inc. All rights reserved.")

  let
    cname  = case mcname args' of
               Nothing -> error "Please provide the /-separated name of a class containing main(). E.g. 'com/example/Class' for class 'Class' in package 'com.example'."
               Just x  -> x
    cpaths = splitSearchPath (classpath args')
    jpaths = splitSearchPath (jars args')
    jopts  = case runParser (many $ eatWS $ many1 $ satisfy $ not . isSpace)
               () "java args" (opts args')
             of
               Left _e -> error "Unable to parse Java command line arguments"
               Right x -> x
    eatWS        = between spaces spaces

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
    exitSuccess

  let withFreshBackend
          :: forall a. (forall b. (AigOps b) => Backend b -> IO a)
          -> IO a
      withFreshBackend k =
        if useSaw args' then S.withFreshBackend k else W.withFreshBackend k

  withFreshBackend $ \sbe -> do
   let fl  = defaultSimFlags { alwaysBitBlastBranchTerms = blast args'
                             , satAtBranches             = sat args'
                             }
       seh = defaultSEH { onPreStep = runAtBreakpoints debuggerREPL }
   let go = do tl <- liftIO $ termInt sbe (fromIntegral (length jopts))
               jssOverrides
               setVerbosity (dbug args')
               printErrPaths .= errPaths args'
               when (startDebugger args') $ do
                 breakOnMain cname
                 evHandlers .= seh
               rs <- runStaticMethod cname "main" "([Ljava/lang/String;)V" =<< do
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
     go `catchSM` \e -> do
                       simExcHndlr' False (text "jss") e
                       liftIO . exitWith $ ExitFailure 1
