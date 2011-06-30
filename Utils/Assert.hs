{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : matthews
-}

{-# LANGUAGE DeriveDataTypeable #-}
module Utils.Assert
  where

import Control.Exception
import Control.Monad.Trans

import Data.Typeable

import Utils.CatchMIO


data AssertIOFailed = AssertIOFailed String
  deriving (Show, Typeable)

instance Exception AssertIOFailed

assertIO :: MonadIO m => Bool -> String -> m ()
assertIO True  _   = return ()
assertIO False msg = liftIO (throwIO (AssertIOFailed msg))

-- | REVISIT: this is likely harder to catch cleanly
-- than @assertIO@.
assertMsg :: Monad m => Bool -> String -> m ()
assertMsg True _    = return ()
assertMsg False msg = failWith msg

-- | Raises an @AssertIOFailed@ exception in an IO monad, intended
-- to be caught by CatchMIO monads. See the documentation for
-- throw and throwIO in Control.Exceptions on when to use
-- failIOWith vs. failWith. (Basically, use failWithIO whenever
-- you can).
failWithIO :: MonadIO m => String -> m a
failWithIO msg = throwMIO (AssertIOFailed msg)

-- | Raises an @AssertIOFailed@ exception, intended
-- to be caught by CatchMIO monads.
failWith :: String -> a
failWith msg = throw (AssertIOFailed msg)

-- | Upon catching an @AssertIOFailed@ exception,
-- appends a new string on to the end of the message
-- and then re-throws the exception. Basically a
-- crude way to reconstruct an exception stack trace.
withFailMsg :: CatchMIO m => m a -> String -> m a
withFailMsg m msg =
  m `catchMIO` \(AssertIOFailed msg') ->
    failWithIO (msg ++ " || " ++ msg')