{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : matthews
-}

{-# LANGUAGE Rank2Types #-}
module IO.Session
  ( Session(..)
  , pureSession
  , liftSession
  , makeSessionStateT
  , sessionErrorT
  , sessionReaderT
  , sessionExcT
  , noFinishIO
  , SessionMonad(..)
  , SessionState(..)
  ) where

import Control.Monad.Error
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Control.Exception

{----

  Given a monad parameterized on an internal state value, a Session
  allows you to create an IO session where the internal state is initialized,
  but then kept hidden. An IOSession has accessor "run" to perform a
  computation using the hidden state within the IO monad. You can perform
  more than one computation during a given session, and the state is
  appropriately threaded through them. There is also an accessor to deallocate
  the hidden state once you are finished with the session.

Items to revisit:
  * Generalize liftSessionT to work on more that just StateT
  For example, replace StateT with variable t, make t an instance
  of MonadTrans, MonadWriter, then use MonadWriter.listen and
  MonadWriter.tell to read and set the state. However, I think
  I may need to declare a new RunT class something like this:

    class RunT t s m where
      runT :: t a -> s -> m (a,s)

----}

data Session m
  = Session { toIO    :: forall a. m a -> IO a
            , finishIO :: IO ()
            }

-- | The "identity" session containing no internal state
pureSession :: Session IO
pureSession = Session { toIO     = id
                      , finishIO = return ()
                      }

-- | Lifts a session from monad t to monad m.
liftSession :: (forall a. m a -> t a) -> Session t -> Session m
liftSession f (Session { toIO = run, finishIO = finish }) =
  Session { toIO = run . f
          , finishIO = finish
          }

-- | Lifts a state session and a state-finalizer to a state-transformer session.
makeSessionStateT :: (Monad m)
                  => Session m
                  -> (s -> IO ())
                  -> s
                  -> IO (Session (StateT s m))
makeSessionStateT ios finish initState = do
  r <- newIORef initState
  return $ Session { toIO = \t -> do
                       s <- readIORef r
                       (a,s') <- (toIO ios $ runStateT t s)
                       writeIORef r s'
                       return a
                   , finishIO = do
                       s <- readIORef r
                       finish s `finally` finishIO ios
                   }

sessionErrorT :: (Monad m)
              => Session m
              -> (e -> SomeException)
              -> Session (ErrorT e m)
sessionErrorT ios errFn = do
  Session { toIO     = runErr
          , finishIO = return ()
          };
  where
    runErr m = do
      res <- toIO ios $ runErrorT m
      case res of
        Left err -> throwIO (errFn err)
        Right x  -> return x

sessionExcT :: (Monad m)
            => Session m
            -> Session (ExcT m)
sessionExcT ios = do
  Session { toIO     = runExc
          , finishIO = return ()
          };
  where
    runExc m = do
      res <- toIO ios $ runExcT m
      case res of
        Left exc -> throwIO exc
        Right x  -> return x

sessionReaderT :: Monad m =>
                  Session m
               -> r
               -> Session (ReaderT r m)
sessionReaderT ios r = do
  Session { toIO     = toIO ios . (`runReaderT` r)
          , finishIO = return ()
          }

-- | An IOSession state-finalizer that does nothing.
noFinishIO :: s -> IO ()
noFinishIO _ = return ()

-- SessionMonad and SessionState {{{1

-- | Monad with the ability to create a session.
class SessionMonad m where
  makeSession :: IO (Session m)

-- | Defines constant for initial state to a state monad used to create
-- sessions.
class SessionState s where
  initialState :: s

instance SessionMonad IO where
  makeSession = return Session { toIO = id, finishIO = return () }
