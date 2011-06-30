{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : matthews
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tests.ExampleExceptionSession (exampleExceptionSessionTests)
  where

import IO.Session

import Control.Exception as Exc
import Control.Monad.State

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC

import Data.Typeable (Typeable)

_assertIO :: Bool -> IO ()
_assertIO True = return ()
_assertIO False = error "Tests.Session: assertion failed"

data SymbolicExc
  = SymExtErr
    {
      symExtErrMsg       :: String
    , _symExtErrVerbosity :: Int
    }
  | SymExtTbd
  deriving (Show, Typeable)

-- | Allow SymbolicMonad errors to also be IO exceptions. Session.toIO
-- will convert an uncaught error into an IO exception.
instance Exception SymbolicExc

newtype Mock a = M (StateT Int IO a)
  deriving (Functor, Monad, MonadIO, MonadState Int)

-- | Throw an error intended to be exposed to the user
throwErr :: String -> Mock a
throwErr msg = liftIO $ throwIO (SymExtErr msg 1)

mockSession :: IO (Session Mock)
mockSession = do
  sess <- makeSessionStateT pureSession noFinishIO 0
  return $ liftSession (\(M m) -> m) sess

bug :: Mock Int
bug = throwErr "Bzz"

testSession :: PropertyM IO ()
testSession = do
  sess <- run $ mockSession
  let m = toIO sess
  res <- run ((m $ bug >> return "everything's fine")
                `Exc.catch` (return . symExtErrMsg))
  QC.assert (res == "Bzz")
  run $ finishIO sess

exampleExceptionSessionTests :: [(Args, Property)]
exampleExceptionSessionTests =
  [ ( stdArgs{ maxSuccess = 1 }
    , label "exampleExceptionSessionTests" $ monadicIO testSession
    )
  ]
