{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies #-}
module Symbolic.Lit.Pure
  ( Lit
  , neg
  , AigComputation
  , module Symbolic.Lit.SatResult
  , runAigComputation
  ) where

-- Import {{{1

import Control.Applicative
import Control.Monad
import Data.Aiger hiding (Lit, writeAiger)
import qualified Data.Aiger as Aiger (Lit, writeAiger)
import Data.Bits
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Storable as SV
import Foreign.Storable

import IO.Session

import Symbolic.Lit.SatResult
import Utils.CatchMIO
import Utils.IOStateT

-- AigComputation for AIG computations {{{1

newtype Lit = Lit { unLit :: Aiger.Lit }
  deriving (Eq, Ord, Storable)

neg :: Lit -> Lit
neg = Lit . xor 1 . unLit

data AigState = AigState {
    andMap     :: !(Map (Lit,Lit) Lit)
  , inputLits  :: ![Lit]
  , inputCount :: !Int
  , nextLit    :: !Aiger.Lit
  }

instance SessionState AigState where
  initialState =
    AigState {
      andMap = Map.empty
    , inputLits = []
    , inputCount = 0
    , nextLit = 2
    }

newtype AigComputation m a = AM { unAM :: StateT AigState m a }
  deriving ( Monad
           , MonadIO
           , CatchMIO
           , MonadState AigState
           , MonadTrans
           , SessionMonad)

instance Monad m => Applicative (AigComputation m) where
  pure      = return
  af <*> aa = af >>= flip (<$>) aa

instance Monad m => Functor (AigComputation m) where
  fmap fn m = m >>= \r -> return (fn r)

-- | Runs AIG computation.
runAigComputation :: MonadIO m => AigComputation m a -> m a
runAigComputation (AM m) = evalStateT m initialState

-- | Internal function for creating AIGER
makeAiger :: MonadIO m
          => [Lit] -- ^ Output lits
          -> AigComputation m AIGER
makeAiger outputLits = do
  state <- get
  let aMap :: Map Lit (Lit,Lit)
      aMap = Map.fromList
           $ map (\(x,y) -> (y,x))
           $ Map.toList
           $ andMap state
      -- @impl tm l@ returns the new literal bound to l.
      impl :: MonadIO m => Map Lit Lit -> Lit -> AigComputation m (Lit,Map Lit Lit)
      impl tm (Lit 0) = return (Lit 0, tm)
      impl tm (Lit 1) = return (Lit 1, tm)
      impl tm (Lit l) = do
        let toLit (Lit v) = Lit (v .|. (l .&. 0x1))
            p = Lit (clearBit l 0)
        case Map.lookup p tm of
          Just p' -> return (toLit p', tm)
          Nothing -> do
            let Just (u,v) = Map.lookup p aMap
            (u', tm1) <- impl tm u
            (v', tm2) <- impl tm1 v
            p' <- litAnd u' v'
            return (toLit p', Map.insert p p' tm2)
      implList :: MonadIO m => Map Lit Lit -> [Lit] -> AigComputation m [Lit]
      implList _tm [] = return []
      implList tm (e : l) = do
        (e',tm1) <- impl tm e
        l' <- implList tm1 l
        return $ e' : l'
  let inputs = reverse $ inputLits state
      cnt = length inputs
  (outLits, newState) <- lift $ flip runStateT initialState $ unAM $ do
    newInputLits <- replicateM cnt makeInputLit
    let tm = Map.fromList $ inputs `zip` newInputLits
    implList tm outputLits
  return $ newAiger cnt
                    -- Latches
                    []
                    -- Output lits
                    (map unLit  outLits)
                    -- And gates
                    (map ((\(x,y) -> (unLit x, unLit y)) . fst)
                        $ sortBy (\x y -> compare (snd x) (snd y))
                        $ Map.toList (andMap newState))
                    []

type instance MonadLit (AigComputation m) = Lit

instance MonadIO m => LitMonad (AigComputation m) where

  litTrue = return (Lit 1)
  litFalse = return (Lit 0)
  litAnd (Lit 0)  _ = return (Lit 0)
  litAnd (Lit 1) ly = return ly
  litAnd  _  (Lit 0) = return (Lit 0)
  litAnd lx  (Lit 1) = return lx
  litAnd lx ly | lx == ly     = return lx
  litAnd lx ly | lx == neg ly = return (Lit 0)
  litAnd lx ly | lx >  ly     = litAnd ly lx
  litAnd lx ly = do
      s <- get
      case Map.insertLookupWithKey (\_ _ x -> x)
                                   (lx,ly)
                                   (Lit $ nextLit s)
                                   (andMap s) of
        (Just l,  _) -> return l
        (Nothing, newMap) -> do
           put s { andMap = newMap, nextLit = nextLit s + 2 }
           return $ Lit (nextLit s)

  getInputLitCount = gets inputCount
  makeInputLit = do
    s <- get
    let l = nextLit s
    put s { nextLit = l + 2
          , inputLits = (Lit l) : inputLits s
          }
    return (Lit l)

  writeAigerV filepath outputLits =
    liftIO . Aiger.writeAiger filepath =<< makeAiger (SV.toList outputLits)
  evalAigV inputs outputLits = do
    aiger <- makeAiger (SV.toList outputLits)
    return $ SV.fromList $ fst (evalAiger aiger (SV.toList inputs) [])

  canCheckSat = return False
  checkSat _ = error "Symbolic.Lit.Pure.checkSat: calling ABC as an external process is not yet implemented."
