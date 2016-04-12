{- |
Module           : $Header$
Description      : operators for manipulating merge frames in the simulator
License          : BSD3
Stability        : stable
Point-of-contact : acfoltzer
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Verifier.Java.MergeFrame where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad.Error
import Control.Lens hiding (Path)

import Verifier.Java.Common

pushMF :: MergeFrame sym -> Simulator sym ()
pushMF mf = ctrlStk.mergeFrames %= (mf :)

popMF :: String -> Simulator sym (MergeFrame sym)
popMF ctx = do 
  stk <- use (ctrlStk.mergeFrames)
  case stk of
    []       -> fail $ ctx ++ ": empty CtrlStk"
    (mf:mfs) -> ctrlStk.mergeFrames .= mfs >> return mf
                         

peekMF :: String -> Simulator sym (MergeFrame sym)
peekMF ctx = SM $ try =<< use (ctrlStk.mergeFrames)
  where try = tryHead $ ctx ++ ": empty CtrlStk"

{-# DEPRECATED bottomMF "is there ever a good use of this?" #-}
bottomMF :: Simulator sym (Maybe (MergeFrame sym))
bottomMF = SM $ uses (ctrlStk.mergeFrames) lastMay

modifyMF :: String
         -> (MergeFrame sym -> MergeFrame sym) 
         -> Simulator sym ()
modifyMF ctx f = pushMF . f =<< popMF ctx

withTopMF :: String -> (MergeFrame sym -> a) -> Simulator sym a
withTopMF ctx f = f <$> peekMF ctx

-- | Run a computation with the top 'MergeFrame'. If 'fst' of the
-- result is 'Just mf', then 'mf' will be pushed back on the 'CtrlStk'
-- after running.
withPoppedMF :: String
             -> (MergeFrame sym -> Simulator sym (Maybe (MergeFrame sym), a)) 
             -> Simulator sym a
withPoppedMF ctx f = do 
  (mmf', a) <- f =<< popMF ctx
  case mmf' of
    Just mf' -> pushMF mf'
    _        -> return ()
  return a

pushPending :: String -> SymPath sym -> Simulator sym ()
pushPending ctx p = modifyMF ctx $ \mf -> mf & pending %~ (p :)

popPending :: String -> Simulator sym (SymPath sym)
popPending ctx = withPoppedMF ctx $ \mf ->
  case mf^.pending of
    []     -> fail $ ctx ++ ": no pending paths"
    (p:ps) -> return (Just $ mf & pending .~ ps, p)

peekPending :: String -> Simulator sym (SymPath sym)
peekPending ctx = do 
    mf <- popMF ctx
    SM . try $ mf^.pending
  where try = tryHead $ ctx ++ ": no pending paths"

modifyPending :: String
              -> (SymPath sym -> SymPath sym) 
              -> Simulator sym ()
modifyPending ctx f = pushPending ctx . f =<< popPending ctx

-- | Run a computation with the top 'SymPath' If 'fst' of the
-- result is 'Just p', then 'p' will be pushed back on the top 'MergeFrame'
-- after running.
withPoppedPending :: String
                  -> (SymPath sym -> Simulator sym (Maybe (SymPath sym), a))
                  -> Simulator sym a
withPoppedPending ctx f = do
  (mp', a) <- f =<< popPending ctx
  case mp' of
    Just p' -> pushPending ctx p'
    _       -> return ()
  return a

withPoppedPending_ :: String
                   -> (SymPath sym -> Simulator sym (Maybe (SymPath sym)))
                   -> Simulator sym ()
withPoppedPending_ ctx f = withPoppedPending ctx (\p -> (,()) <$> f p)

hasPending :: Simulator sym Bool
hasPending = (popPending "" >>= pushPending "" >> return True) 
             `catchError`
             const (return False)

--------------------------------------------------------------------------------
-- Utilities

tryHead :: (Monad m, Error e) => String -> [a] -> ErrorT e m a
tryHead msg []    = fail msg
tryHead _   (x:_) = return x

lastMay [] = Nothing
lastMay xs = Just (last xs)
