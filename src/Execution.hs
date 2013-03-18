{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix, jstanley
-}

{-# LANGUAGE ConstraintKinds #-}

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

-- | Run the static @main@ method of a class, returning a list of all
-- successful paths and their return values (if present).
runMain :: MonadSim sbe m 
        => String
        -- ^ The name of the class containing @main@
        -> [Value (SBETerm sbe)]
        -- ^ Arguments to @main@
        -> Simulator sbe m [(Path sbe, Maybe (Value (SBETerm sbe)))]
runMain cName args =
  runStaticMethod cName "main" "([Ljava/lang/String;)V" args