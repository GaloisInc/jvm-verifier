{-# LANGUAGE TemplateHaskell #-}

{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : atomb
-}
module Verifier.Java.SAWImport where

import Verifier.SAW
import Verifier.SAW.ParserUtils

$(runDecWriter $ do
    prelude <- defineImport [|preludeModule|] preludeModule
    java <- defineModuleFromFile [prelude] "javaModule" "saw/Java.sawcore"
    declareDefTermF prelude "ite"
    declareSharedModuleFns "Java" (decVal java)
 )