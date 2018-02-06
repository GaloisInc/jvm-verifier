{- |
Module           : Execution
Description      : A basic subset of the JSS API for basic execution
License          : BSD3
Stability        : provisional
Point-of-contact : jhendrix, acfoltzer
-}

{-# LANGUAGE ConstraintKinds #-}

module Execution
{-# DEPRECATED "use Execution.JavaSemantics with Verifier.Java.Simulator" #-}
  ( Codebase
  , AtomicValue(..)
  , JSDouble
  , JSFloat
  , JSInt
  , JSLong
  , JSRef
  , JSBool
  , JSRslt
  , JSValue
  , JavaSemantics(..)
  , dynBind
  , isSubtype
  , loadClass
  , loadCodebase
  , runMain
  , runStaticMethod
  , createAndThrow
  , throwNullPtrExc
  )
where

import Execution.JavaSemantics
import Verifier.Java.Codebase
import Verifier.Java.Simulator

-- | Run the static @main@ method of a class, returning a list of all
-- successful paths and their return values (if present).
runMain :: MonadSim sbe m
        => ClassName
        -- ^ The name of the class containing @main@
        -> [Value (SBETerm sbe)]
        -- ^ Arguments to @main@
        -> Simulator sbe m [(Path sbe, Maybe (Value (SBETerm sbe)))]
runMain cName args =
  runStaticMethod cName "main" "([Ljava/lang/String;)V" args
