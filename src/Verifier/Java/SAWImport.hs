{-# LANGUAGE TemplateHaskell #-}

{- |
Module           : $Header$
Description      :
License          : BSD3
Stability        : provisional
Point-of-contact : atomb
-}
module Verifier.Java.SAWImport where

import Verifier.SAW.ParserUtils

$(defineModuleFromFileWithFns
  "javaModule" "scLoadJavaModule" "saw/Java.sawcore")
