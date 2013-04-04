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
import Control.Lens

import qualified Data.Map as M
import qualified Data.Set as S

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

breakpointLogger :: (Functor m, Monad m)
                 => Maybe PC
                 -> SymInsn
                 -> Simulator sbe m ()
breakpointLogger = runAtBreakpoints f
  where f mpc insn = do
          clName <- getCurrentClassName
          method <- getCurrentMethod
          let lineNum = maybe "unknown" (integer . fromIntegral) $
                          sourceLineNumberOrPrev method =<< mpc
              pc = maybe "unknown" (integer . fromIntegral) mpc
          dbugM . render $
            "hit breakpoint at" <+> text clName <> "." <>
            ppMethod method <> colon <> lineNum <> "%" <> pc

debuggerREPL :: Maybe PC -> SymInsn -> Simulator sbe m ()
debuggerREPL mpc insn = do
  undefined
