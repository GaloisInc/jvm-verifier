{- |
Module           : $Header$
Description      : Debugger implementation for JSS
Stability        : provisional
Point-of-contact : acfoltzer

Debugger for the JVM Symbolic Simulator. This module provides
implementations of the 'SEH' event handlers.

-}

{-# LANGUAGE OverloadedStrings #-}
module Verifier.Java.Debugger where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as S

import System.Console.Haskeline
import System.Exit

import Text.PrettyPrint

import Data.JVM.Symbolic.AST

import Verifier.Java.Common
import Verifier.Java.Simulator hiding (getCurrentClassName, getCurrentMethod)

-- | Add a breakpoint to the @main@ method of the given class
breakOnMain :: String -> Simulator sbe m ()
breakOnMain clName = addBreakpoint clName mainKey BreakEntry

-- | Given a step handler, return a new step handler that runs it when
-- breakpoints are encountered
runAtBreakpoints :: (Functor m, Monad m)
                 => (Maybe PC -> SymInsn -> Simulator sbe m ())
                 -> Maybe PC
                 -> SymInsn
                 -> Simulator sbe m ()
runAtBreakpoints sh (Just pc) insn = do
  atTransient <- handleTransientBreakpoints pc insn
  if atTransient
    then sh (Just pc) insn
    else do
      mp <- getPathMaybe
      case currentCallFrame =<< mp of
        Nothing -> return ()
        Just cf -> do
          let clName = cf^.cfClass
              method = cf^.cfMethod
          mbps <- M.lookup (clName, method) <$> use breakpoints
          case S.member pc <$> mbps of
            Nothing -> return ()
            Just False -> return ()
            Just True -> sh (Just pc) insn
runAtBreakpoints _  _         _    = return ()

-- | Check whether we're at a transient breakpoint, and if so,
-- deactivate it and return 'True'
handleTransientBreakpoints :: PC -> SymInsn -> Simulator sbe m Bool
handleTransientBreakpoints pc insn = do
  method <- getCurrentMethod
  mLineNum <- getCurrentLineNumber pc
  let rember bp = do
        tbps <- use trBreakpoints
        if S.member bp tbps
          then do trBreakpoints %= S.delete bp
                  return True
          else return False
      handleStep = rember BreakNextInsn
      handleRet = case insn of
                    ReturnVal  -> rember (BreakReturnFrom method)
                    ReturnVoid -> rember (BreakReturnFrom method)
                    _          -> return False
      handleLineChange = do
          tbps <- use trBreakpoints
          or <$> forM (S.elems tbps) (\bp ->
            case bp of
              BreakLineChange mLineNum' | mLineNum /= mLineNum' -> do
                rember (BreakLineChange mLineNum')
              _ -> return False)
  or <$> sequence [handleStep, handleRet, handleLineChange]

breakpointLogger :: (Functor m, Monad m)
                 => Maybe PC
                 -> SymInsn
                 -> Simulator sbe m ()
breakpointLogger = runAtBreakpoints (printLoc "hit breakpoint:")

printLoc :: (Functor m, Monad m)
         => String
         -> Maybe PC
         -> SymInsn
         -> Simulator sbe m ()
printLoc msg mpc insn = do
  clName <- getCurrentClassName
  method <- getCurrentMethod
  let lineNum = maybe "unknown" (integer . fromIntegral) $
                sourceLineNumberOrPrev method =<< mpc
      pc = maybe "unknown" (integer . fromIntegral) mpc
  dbugM . render $
    text msg <+> text clName <> "." <>
    ppMethod method <> colon <> lineNum <> "%" <> pc <> colon <>
    ppSymInsn insn

debuggerREPL :: (Functor m, Monad m)
             => Maybe PC
             -> SymInsn
             -> Simulator sbe m ()
debuggerREPL mpc insn = do
    printLoc "at" mpc insn
    runInputT defaultSettings loop
  where loop = do
          mline <- getInputLine "jss% "
          case mline of
            Nothing -> return ()
            Just "" -> loop
            Just input -> do
              let (cmd:args) = words input
              case M.lookup cmd commandMap of
                Just cmd -> do
                  continue <- lift $ cmdAction cmd args
                  unless continue loop
                Nothing -> do
                  outputStrLn $ "unknown command '" ++ cmd ++ "'"
                  outputStrLn $ "type 'help' for more info"
                  loop

data Command m = Cmd { cmdNames :: [String]
                     , cmdDesc :: String
                     , cmdCompletion :: CompletionFunc m
                     , cmdAction :: [String] -> m Bool
                     }

commandMap :: (Functor m, Monad m) => M.Map String (Command (Simulator sbe m))
commandMap = M.fromList . concatMap expandNames $ cmds
  where expandNames cmd = do
          name <- cmdNames cmd
          return (name, cmd)

cmds :: (Functor m, Monad m) => [Command (Simulator sbe m)]
cmds = [
    contCmd
  , exitCmd
  , helpCmd
  , stepiCmd
  ]

contCmd :: Monad m => Command m
contCmd = Cmd {
    cmdNames = ["cont", "c"]
  , cmdDesc = "continue execution"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> return True
  }

stepiCmd :: (Functor m, Monad m) => Command (Simulator sbe m)
stepiCmd = Cmd {
    cmdNames = ["stepi"]
  , cmdDesc = "execute current symbolic instruction"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> do
      breakNextInsn
      return True
  }

exitCmd :: MonadIO m => Command m
exitCmd = Cmd {
    cmdNames = ["exit", "quit"]
  , cmdDesc = "exit JSS"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> liftIO $ exitWith ExitSuccess
  }

helpCmd :: MonadIO m => Command m
helpCmd = Cmd {
    cmdNames = ["help", "?"]
  , cmdDesc = "show this help"
  , cmdCompletion = noCompletion
  , cmdAction = \_ -> do liftIO $ putStrLn helpString
                         return False
  }

helpString :: String
helpString = render . vcat $
  [ (hsep . punctuate "," $ map text names) <> colon $$ nest 2 (text desc)
  | Cmd { cmdNames = names, cmdDesc = desc }
      <- (cmds :: [Command (Simulator sbe IO)])
  ]

breakNextInsn :: Simulator sbe m ()
breakNextInsn = trBreakpoints %= S.insert BreakNextInsn

breakCurrentMethodReturn :: Simulator sbe m ()
breakCurrentMethodReturn = do
  method <- getCurrentMethod
  trBreakpoints %= S.insert (BreakReturnFrom method)

breakLineChange :: PC -> Simulator sbe m ()
breakLineChange pc = do
  mline <- getCurrentLineNumber pc
  trBreakpoints %= S.insert (BreakLineChange mline)
