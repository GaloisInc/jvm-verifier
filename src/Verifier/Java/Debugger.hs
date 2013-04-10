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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Verifier.Java.Debugger
  (
    breakOnMain
  , breakpointLogger
  , debuggerREPL
  , runAtBreakpoints
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Lens hiding (createInstance)

import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple.Curry
import Data.Word (Word16)

import System.Console.Haskeline
import System.Console.Haskeline.History
import System.Exit

import Text.PrettyPrint

import Data.JVM.Symbolic.AST

import Verifier.Java.Common
import Verifier.Java.Simulator hiding (getCurrentClassName, getCurrentMethod)

#if __GLASGOW_HASKELL__ < 706
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.Read as R
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- R.readPrec_to_S read' R.minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- R.readPrec
       R.lift P.skipSpaces
       return x

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a
#else
import Text.Read (readMaybe)
#endif

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
              let go = cmdAction cmd mpc insn args
                  handleErr epe@(ErrorPathExc _ _) = throwError epe
                  handleErr (UnknownExc (Just (FailRsn rsn))) = do
                    dbugM $ "error: " ++ rsn
                    return False
              continue <- lift $ catchError go handleErr
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
  , localsCmd
  , dumpCmd
  , contCmd
  , killCmd
  , exitCmd
  , clearCmd
  , stopinCmd
  , clearinCmd
  , stopatCmd
  , clearatCmd
  , stoppcCmd
  , clearpcCmd
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

localsCmd :: MonadSim sbe m => Command (Simulator sbe m)
localsCmd = Cmd {
    cmdNames = ["locals"]
  , cmdArgs = []
  , cmdDesc = "print local variables in current stack frame"
  , cmdCompletion = noCompletion
  , cmdAction = \mpc _ _ -> do
      locals <- modifyCallFrameM "debugger" $ \cf ->
                  return (cf^.cfLocals, cf)
      sbe <- use backend
      case mpc of
        Nothing -> dbugM . render $ ppLocals sbe locals
        Just pc -> do
          method <- getCurrentMethod
          dbugM . render =<< ppNamedLocals method pc locals
      return False
  }

contCmd :: Monad m => Command m
contCmd = Cmd {
    cmdNames = ["cont", "c"]
  , cmdArgs = []
  , cmdDesc = "continue execution"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ _ -> return True
  }

stopinCmd :: MonadSim sbe m => Command (Simulator sbe m)
stopinCmd = Cmd {
    cmdNames = ["stopin"]
  , cmdArgs = ["<class id>.<method>[<type_descriptor>]"]
  , cmdDesc = "set a breakpoint in a method; type descriptor is optional"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ args ->
      case args of
        [arg] -> do
          bps <- entriesForArg arg
          forM_ bps $ uncurryN addBreakpoint
          return False
        _ -> failHelp
  }

clearinCmd :: MonadSim sbe m => Command (Simulator sbe m)
clearinCmd = Cmd {
    cmdNames = ["clearin"]
  , cmdArgs = ["<class id>.<method>[<type_descriptor>]"]
  , cmdDesc = "clear a breakpoint in a method; type descriptor is optional"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ args ->
      case args of
        [arg] -> do
          bps <- entriesForArg arg
          forM_ bps $ uncurryN removeBreakpoint
          return False
        _ -> failHelp
  }

entriesForArg :: String -> Simulator sbe m [(String, MethodKey, Breakpoint)]
entriesForArg arg = do
  (clName, keys) <- keysForArg arg
  return . map (clName, , BreakEntry) $ keys

-- | Given a string expected to be a method signature of the form
-- @com.Foo.bar@ or @com.Foo.bar(I)V@, return the class name and a
-- list of method keys that correspond to that method
keysForArg :: String -> Simulator sbe m (String, [MethodKey])
keysForArg arg = do
  let (sig, desc) = break (== '(') arg
      ids = reverse (splitOn "." sig)
  clName <- case ids of
              (_methStr:rest) -> return . intercalate "/" . reverse $ rest
              _ -> fail $ "invalid method signature " ++ arg
  methName <- case ids of
                (methStr:_) -> return methStr
                _ -> fail $ "invalid method signature " ++ arg
  cl <- lookupClass clName
  let keys | not (null desc) = [makeMethodKey methName desc]
           | otherwise = concatMap (makeKeys . methodKey) (classMethods cl)
      makeKeys key = if thisName == methName then [key] else []
        where thisName = methodKeyName key
  return (clName, keys)

stopatCmd :: MonadSim sbe m => Command (Simulator sbe m)
stopatCmd = Cmd {
    cmdNames = ["stopat"]
  , cmdArgs = ["<class id>:<line>"]
  , cmdDesc = "set a breakpoint at a line"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ args ->
      case args of
        [arg] -> do
          uncurryN removeBreakpoint =<< lineNumForArg arg
          return False
        _ -> failHelp
  }

clearatCmd :: MonadSim sbe m => Command (Simulator sbe m)
clearatCmd = Cmd {
    cmdNames = ["clearat"]
  , cmdArgs = ["<class id>:<line>"]
  , cmdDesc = "clear a breakpoint at a line"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ args ->
      case args of
        [arg] -> do
          uncurryN removeBreakpoint =<< lineNumForArg arg
          return False
        _ -> failHelp
  }

stoppcCmd :: MonadSim sbe m => Command (Simulator sbe m)
stoppcCmd = Cmd {
    cmdNames = ["stoppc"]
  , cmdArgs = ["<class id>.<method>[<type_descriptor>]%pc"]
  , cmdDesc = "set a breakpoint at a program counter"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ args ->
      case args of
        [arg] -> do
          bps <- pcsForArg arg
          forM_ bps $ uncurryN addBreakpoint
          return False
        _ -> failHelp
  }

clearpcCmd :: MonadSim sbe m => Command (Simulator sbe m)
clearpcCmd = Cmd {
    cmdNames = ["clearpc"]
  , cmdArgs = ["<class id>.<method>[<type_descriptor>]%pc"]
  , cmdDesc = "clear a breakpoint at a program counter"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ args ->
      case args of
        [arg] -> do
          bps <- pcsForArg arg
          forM_ bps $ uncurryN removeBreakpoint
          return False
        _ -> failHelp
  }

clearCmd :: Command (Simulator sbe m)
clearCmd = Cmd {
    cmdNames = ["clear"]
  , cmdArgs = []
  , cmdDesc = "list breakpoints"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ _ -> dumpBPs >> return False
  }

dumpBPs :: Simulator sbe m ()
dumpBPs = do
  bps <- use breakpoints
  if all S.null (M.elems bps)
     then dbugM "no breakpoints set"
     else dbugM . render . ppBreakpoints $ bps

pcsForArg :: String -> Simulator sbe m [(String, MethodKey, Breakpoint)]
pcsForArg arg = do
  let (method, pcStr) = break (== '%') arg
  pc <- case readMaybe (drop 1 pcStr) of
          Just (n :: Word16) -> return n
          Nothing -> fail $ "invalid program counter " ++ pcStr
  (clName, keys) <- keysForArg method
  return . map (clName, , BreakPC pc) $ keys

lineNumForArg :: String -> Simulator sbe m (String, MethodKey, Breakpoint)
lineNumForArg arg = do
  let (clName, lnStr) = break (== ':') arg
  lineNum <- case readMaybe (drop 1 lnStr) of
               Just (n :: Word16) -> return n
               Nothing -> fail $ "invalid line number " ++ lnStr
  cl <- lookupClass clName
  case lookupLineMethodStartPC cl lineNum of
    Just (method, pc) ->
      return (clName, (methodKey method), (BreakLineNum lineNum))
    Nothing -> fail . render $
      "line number" <+> integer (fromIntegral lineNum) <+> "not found in"
      <+> text clName $$ "were class files compiled with debugging symbols?"

dumpCmd :: MonadSim sbe m => Command (Simulator sbe m)
dumpCmd = Cmd {
    cmdNames = ["dump", "d"]
  , cmdArgs = ["[ctrlstk | memory | method | path]"]
  , cmdDesc = "dump an object in the simulator"
  , cmdCompletion = noCompletion
  , cmdAction = \_ _ args -> do
      case args of
        ["ctrlstk"] -> dumpCtrlStk
        ["memory"] -> dumpMemory "debugger"
        ["method"] -> dumpCurrentMethod
        ["path"] -> dumpCurrentPath
        _ -> dbugM $ "dump: unsupported object " ++ unwords args
      return False
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

failHelp :: Monad m => m a
failHelp = fail "invalid arguments; type 'help' for details"

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
