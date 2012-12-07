{- |
Module           : $Header$
Description      : operators for manipulating merge frames in the simulator
Stability        : stable
Point-of-contact : acfoltzer
-}

module Verifier.Java.MergeFrame where

import Control.Error hiding (catch)
import Control.Lens hiding (Path)

import Verifier.Java.Common

pushMF :: MergeFrame sym -> Simulator sym ()
pushMF mf = ctrlStk.mergeFrames %= (mf :)

popMF :: Simulator sym (MergeFrame sym)
popMF = liftEitherT $ try =<< use (ctrlStk.mergeFrames)
  where try = tryHead . errRsn $ "popMF: empty stack"

{-# DEPRECATED bottomMF "is there ever a good use of this?" #-}
bottomMF :: Simulator sym (Maybe (MergeFrame sym))
bottomMF = liftEitherT $ uses (ctrlStk.mergeFrames) lastMay

modifyMF :: (MergeFrame sym -> MergeFrame sym) -> Simulator sym ()
modifyMF f = pushMF . f =<< popMF

-- | Run a computation with the top 'MergeFrame'. If 'fst' of the
-- result is 'Just mf', then 'mf' will be pushed back on the 'CtrlStk'
-- after running.
withTopMF :: (MergeFrame sym -> Simulator sym (Maybe (MergeFrame sym), a)) 
          -> Simulator sym a
withTopMF f = do 
  (mmf', a) <- f =<< popMF
  case mmf' of
    Just mf' -> pushMF mf'
    _        -> return ()
  return a

pushPending :: SymPath sym -> Simulator sym ()
pushPending p = modifyMF $ \mf -> mf & pending %~ (p :)

popPending :: Simulator sym (SymPath sym)
popPending = do mf <- popMF
                liftEitherT . try $ mf^.pending
  where try = tryHead . errRsn $ "popPending: empty stack"

modifyPending :: (SymPath sym -> SymPath sym) -> Simulator sym ()
modifyPending f = pushPending . f =<< popPending

-- | Run a computation with the top 'SymPath' If 'fst' of the
-- result is 'Just p', then 'p' will be pushed back on the top 'MergeFrame'
-- after running.
withTopPending :: (SymPath sym -> Simulator sym (Maybe (SymPath sym), a))
               -> Simulator sym a
withTopPending f = do
  (mp', a) <- f =<< popPending
  case mp' of
    Just p' -> pushPending p'
    _       -> return ()
  return a

hasPending :: Simulator sym Bool
hasPending = (popPending >>= pushPending >> return True) `catch` const (return False)