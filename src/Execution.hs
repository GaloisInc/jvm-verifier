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
  , getNextPc
  , isSubtype
  , loadClass
  , loadCodebase
  , lookupClass
  , run
  , run_
  , step
  , throwNullPtrExc
  , runMain
  , runStaticMethod
  , runStaticMethod_
  )
where

import Execution.JavaSemantics
import Execution.Stepper
import Verifier.Java.Codebase

-- | Executes the JavaSemantics doStep function until execution is complete.
run :: JavaSemantics m => m (JSRslt m)
run = isFinished >>= \b -> if b then getResult else doStep >> run
      --
      -- NB: Pushing the entire 'run' function into the JavaSemantics typeclass,
      -- as we'd originally tried to do, somehow trips the following ghc bug:
      -- http://hackage.haskell.org/trac/ghc/ticket/3963 when using ghc 6.12.1.
      -- This workaround is probably fine to leave as-is, but there's really no
      -- good reason to have it outside the typeclass.

run_ :: JavaSemantics m => m ()
run_ = run >> return ()

runStaticMethod_ :: JavaSemantics m => String -> String -> String -> [JSValue m] -> m ()
runStaticMethod_ cName methName methodType args =
  runStaticMethod cName methName methodType args >> return ()

runStaticMethod :: JavaSemantics m => String -> String -> String -> [JSValue m] -> m (JSRslt m)
runStaticMethod cName methName methodType args = do
  invokeStaticMethod
    cName
    (makeMethodKey methName methodType)
    args
  run

runMain :: JavaSemantics m => String -> [JSValue m] -> m (JSRslt m)
runMain cName args =
  runStaticMethod cName "main" "([Ljava/lang/String;)V" args