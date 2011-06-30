{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : matthews
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tests.ExampleErrorSession (exampleErrorSessionTests)
  where

import IO.Session

import Control.Exception as Exc
import Control.Monad.State
import Control.Monad.Error

import Test.QuickCheck
import Test.QuickCheck.Monadic as QC

import Data.Typeable (Typeable)

_assertIO :: Bool -> IO ()
_assertIO True = return ()
_assertIO False = error "Tests.Session: assertion failed"


data SymbolicExc
  = SymExtErr
    {
      symExtErrMsg        :: String
    , _symExtErrVerbosity :: Int
    }
  | SymExtTbd
  deriving (Show, Typeable)

-- | Allow SymbolicMonad errors to also be IO exceptions. Session.toIO
-- will convert an uncaught error into an IO exception.
instance Exception SymbolicExc

instance Error SymbolicExc where
  strMsg msg = SymExtErr ("Unhandled exception raised: (" ++ msg ++ ")") 1
  noMsg      = SymExtErr "Unhandled exception raised" 1

newtype Mock a = EI (ErrorT SymbolicExc (StateT Int IO) a)
  deriving (Functor, Monad, MonadIO, MonadError SymbolicExc, MonadState Int)

-- | Throw an error intended to be exposed to the user
throwErr :: String -> Mock a
throwErr msg = throwError (SymExtErr msg 1)

mockSession :: IO (Session Mock)
mockSession = do
  stateSess <- makeSessionStateT pureSession noFinishIO 0
  let errorSess = sessionErrorT stateSess toException
  return $ liftSession (\(EI m) -> m) errorSess

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

exampleErrorSessionTests :: [(Args, Property)]
exampleErrorSessionTests =
  [ ( stdArgs{ maxSuccess = 1 }
    , label "exampleErrorSessionTests" $ monadicIO testSession
    )
  ]
