{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : acfoltzer

A minimal set of internal exports that are used in the test suite.
Not to be used for client code.
-}

module Verifier.Java.TestInterface
( CValue(..)
, getSValW
, flattenLitResult
) where

import Verinf.Symbolic
