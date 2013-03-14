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

import Control.Exception as Exc
import Control.Monad.State
import Data.Typeable (Typeable)

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Verinf.IO.Session

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

testSession :: Assertion
testSession = do
  sess <- mockSession
  let m = toIO sess
  res <- ((m $ bug >> return "everything's fine")
            `Exc.catch` (return . symExtErrMsg))
  res @?= "Bzz"
  finishIO sess

exampleExceptionSessionTests :: Test
exampleExceptionSessionTests = testGroup "ExampleExceptionSession" $ 
  [ testCase "testSession" testSession ]
