{- |
Module           : $Header$
Description      : Debugger implementation for JSS
Stability        : provisional
Point-of-contact : acfoltzer

Debugger for the JVM Symbolic Simulator. This module provides
implementations of the 'SEH' event handlers.

-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Verifier.Java.Debugger where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens hiding (createInstance)

import qualified Data.Map as M
import qualified Data.Set as S

import System.Console.Haskeline
import System.Console.Haskeline.History
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

debuggerREPL :: (MonadSim sbe m)
             => Maybe PC
             -> SymInsn
             -> Simulator sbe m ()
debuggerREPL mpc insn = do
    printLoc "at" mpc insn
    runInputT (defaultSettings { historyFile = Just ".jssdb" }) loop
  where loop = do
          mline <- getInputLine "jss% "
          case mline of
            Nothing -> return ()
            Just "" -> do
              -- repeat last command if nothing entered
              hist <- getHistory
              case historyLines hist of
                (l:_) -> doCmd (words l)
                _ -> loop
            Just input -> doCmd (words input)
        doCmd (cmd:args) = do
          case M.lookup cmd commandMap of
            Just cmd -> do
              continue <- lift $ cmdAction cmd mpc insn args
              unless continue loop
            Nothing -> do
              outputStrLn $ "unknown command '" ++ cmd ++ "'"
              outputStrLn $ "type 'help' for more info"
              loop
        doCmd [] = error "unreachable"

data Command m = Cmd { cmdNames :: [String]
                     , cmdArgs :: [String]
                     , cmdDesc :: String
                     , cmdCompletion :: CompletionFunc m
                     , cmdAction :: Maybe PC -> SymInsn -> [String] -> m Bool
                     }

commandMap :: (MonadSim sbe m) => M.Map String (Command (Simulator sbe m))
commandMap = M.fromList . concatMap expandNames $ cmds
  where expandNames cmd = do
          name <- cmdNames cmd
          return (name, cmd)

cmds :: (MonadSim sbe m) => [Command (Simulator sbe m)]
cmds = [
    helpCmd
  , whereCmd
  , contCmd
  , killCmd
  , exitCmd
  , stepCmd
  , stepupCmd
  , stepiCmd
  ]

killCmd :: (MonadSim sbe m) => Command (Simulator sbe m)
killCmd = Cmd {
    cmdNames = ["kill"]
  , cmdArgs = ["[<msg>]"]
  , cmdDesc = "throw an exception on the current execution path"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ args -> do
      let rte = "java/lang/RuntimeException"
      when (null args) $ createAndThrow rte

      msgStr@(Ref _ ty) <- refFromString (unwords args)
      let params = Just [(ty, RValue msgStr)]
      excRef <- createInstance rte params
      throw excRef
      return True
  }

whereCmd :: (Functor m, Monad m) => Command (Simulator sbe m)
whereCmd = Cmd {
    cmdNames = ["where", "w"]
  , cmdArgs = []
  , cmdDesc = "print call stack"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ _ -> do
      mp <- getPathMaybe
      case mp of
        Nothing -> dbugM "no active execution path"
        Just p -> dbugM . render . ppStackTrace $ p^.pathStack
      return False
  }

contCmd :: Monad m => Command m
contCmd = Cmd {
    cmdNames = ["cont", "c"]
  , cmdArgs = []
  , cmdDesc = "continue execution"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ _-> return True
  }

stepCmd :: (Functor m, Monad m) => Command (Simulator sbe m)
stepCmd = Cmd {
    cmdNames = ["step", "s"]
  , cmdArgs = []
  , cmdDesc = "execute current line"
  , cmdCompletion = noCompletion
  , cmdAction = \mpc _ _ -> do
      case mpc of
        Just pc -> breakLineChange pc >> return True
        Nothing -> dbugM "unknown current line (try 'stepi')" >> return False
  }

stepupCmd :: (Functor m, Monad m) => Command (Simulator sbe m)
stepupCmd = Cmd {
    cmdNames = ["stepup"]
  , cmdArgs = []
  , cmdDesc = "execute until current method returns to its caller"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ _ -> do
      breakCurrentMethodReturn
      return True
  }

stepiCmd :: (Functor m, Monad m) => Command (Simulator sbe m)
stepiCmd = Cmd {
    cmdNames = ["stepi"]
  , cmdArgs = []
  , cmdDesc = "execute current symbolic instruction"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ _ -> do
      breakNextInsn
      return True
  }

exitCmd :: MonadIO m => Command m
exitCmd = Cmd {
    cmdNames = ["exit", "quit"]
  , cmdArgs = []
  , cmdDesc = "exit JSS"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ _ -> liftIO $ exitWith ExitSuccess
  }

helpCmd :: forall sbe m . MonadSim sbe m => Command (Simulator sbe m)
helpCmd = Cmd {
    cmdNames = ["help", "?"]
  , cmdArgs = []
  , cmdDesc = "show this help"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ _ -> do
      dbugM $ helpString (cmds :: [Command (Simulator sbe m)])
      return False
  }

helpString :: [Command m] -> String
helpString cmds = render . vcat $
  [ invs <> colon $$ nest 2 (text $ cmdDesc cmd)
  | cmd <- cmds
  , let invs = hsep . map text $ (cmdNames cmd ++ cmdArgs cmd)
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
