{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix, jstanley
-}

module Execution
  ( AtomicValue(..)
  , Codebase
  , JavaSemantics(..)
  , JSDouble
  , JSFloat
  , JSInt
  , JSLong
  , JSRef
  , JSBool
  , JSRslt
  , JSValue
  , createAndThrow
  , dynBind
  , isSubtype
  , loadClass
  , loadCodebase
  , lookupClass
  , run
  , step
  , throwNullPtrExc
  , runMain
  , runStaticMethod
  )
where

import Execution.JavaSemantics
import Execution.Stepper
import Verifier.Java.Codebase
import Verifier.Java.Simulator

runMain cName args =
  runStaticMethod cName "main" "([Ljava/lang/String;)V" args