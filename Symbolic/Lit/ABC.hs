{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Symbolic.Lit.ABC (
    Lit
  , neg
  , AigComputation
  , module Symbolic.Lit.SatResult
  , runAigComputation
  -- * Aig functions
  , writeDot
  ) where

import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad.Reader
import qualified Data.Vector.Storable as LV
import qualified Data.Vector.Storable.Mutable as LV (new,write)
import Foreign hiding (new)
import Foreign.C

import Data.ABC.Internal.ABC
import Data.ABC.Internal.FRAIG
import Data.ABC.Internal.IO

import IO.Session
import Symbolic.Lit.SatResult
import Utils.CatchMIO

newtype AigComputationState = AigComputationState {
   snmNetwork :: Ptr Abc_Ntk_t
  }

newtype AigComputation m a = AM (ReaderT AigComputationState m a)
  deriving (Monad, MonadIO, MonadTrans, CatchMIO)

instance Monad m => Applicative (AigComputation m) where
  pure      = return
  af <*> aa = af >>= flip (<$>) aa

instance Monad m => Functor (AigComputation m) where
  fmap fn m = return . fn =<< m

-- | Initializes an AIG network Session.
instance SessionMonad m => SessionMonad (AigComputation m) where
  makeSession = do
    Session { toIO = mToIO, finishIO = mFinishIO } <- makeSession
    networkPtr <- abcNtkAlloc AbcNtkStrash AbcFuncAig True
    fptr <- mallocForeignPtr
    withForeignPtr fptr $ \ptr -> poke ptr networkPtr
    addForeignPtrFinalizer p_abcBridgeNtkPtrDelete fptr
    return Session
      { toIO = \(AM m) ->
          withForeignPtr fptr $ \ptr ->
            mToIO $ runReaderT m AigComputationState { snmNetwork = ptr }
      , finishIO = do
          finalizeForeignPtr fptr
          mFinishIO
      }

-- Use unsafe version of Abc_AigAnd to boost performance.
foreign import ccall unsafe "Abc_AigAnd"
  abcAigAnd2 :: Abc_Aig_t -> Abc_Obj_t -> Abc_Obj_t -> IO Abc_Obj_t

-- | Run a state transformer monad.
runAigComputation :: CatchMIO m => AigComputation m a -> m a
runAigComputation (AM m) = do
  ptr <- liftIO malloc
  flip finallyMIO (liftIO $ free ptr) $ do
    networkPtr <- liftIO $ abcNtkAlloc AbcNtkStrash AbcFuncAig True
    liftIO $ poke ptr networkPtr
    finallyMIO
      (runReaderT m AigComputationState{ snmNetwork = ptr })
      (liftIO $ abcNtkDelete networkPtr)

newtype Lit = Lit Abc_Obj_t
  deriving (Eq, Storable)

neg :: Lit -> Lit
neg (Lit o) = Lit
            $ wordPtrToPtr
            $ ptrToWordPtr o `xor` 0x1

getNetworkPtr :: MonadIO m => AigComputation m Abc_Ntk_t
getNetworkPtr = liftIO . peek =<< AM (asks snmNetwork)

-- | Internal method that adds the given outputs to the network,
-- executes the IO action, and removes the outputs.
withOutputs :: MonadIO m => LV.Vector Lit -> (Abc_Ntk_t -> IO a) -> AigComputation m a
withOutputs outputs mFn = do
  networkPtr <- getNetworkPtr
  liftIO $ do
    outputNodes <- LV.forM outputs $ \(Lit outNodePtr) -> do
      po <- abcNtkCreateObj networkPtr AbcObjPo
      abcObjAddFanin po (CoAbc_Obj_t outNodePtr)
      return po
    v <- mFn networkPtr
    LV.forM_ outputNodes abcNtkDeleteObj
    return v

-- | Translates model to Boolean result.
translateBoolArray :: Int -> Ptr CInt -> IO (LV.Vector Bool)
translateBoolArray len pModel = do
  let convertToBool 0 = False
      convertToBool 1 = True
      convertToBool _ = error "unrecognized value during convertToBool"
  mv <- LV.new len
  LV.forM_ (LV.enumFromN 0 len) $ \i ->
    LV.write mv i . convertToBool =<< peekElemOff pModel i
  LV.freeze mv

type instance MonadLit (AigComputation m) = Lit

instance MonadIO m => LitMonad (AigComputation m) where

  litTrue = do
    networkPtr <- getNetworkPtr
    c <- liftIO $ abcAigConst1 networkPtr
    return $ Lit c

  litFalse = fmap neg litTrue

  litAnd (Lit pa) (Lit pb) = do
    networkPtr <- getNetworkPtr
    liftIO $ do
      manFunc <- fmap castPtr $ abcNtkManFunc networkPtr
      fmap Lit $ abcAigAnd2 manFunc pa pb

  getInputLitCount = do
    networkPtr <- getNetworkPtr
    liftIO $ abcNtkPiNum networkPtr

  makeInputLit = do
    networkPtr <- getNetworkPtr
    liftIO $ fmap Lit $ abcNtkCreateObj networkPtr AbcObjPi

  canCheckSat = return True

  checkSat (Lit outNodePtr) = do
    ppNetwork <- AM $ asks snmNetwork
    liftIO $ do
      networkPtr <- peek ppNetwork
      po <- abcNtkCreateObj networkPtr AbcObjPo
      flip CE.finally (abcNtkDeleteObj po) $ do
        abcObjAddFanin po (CoAbc_Obj_t outNodePtr)
        alloca $ \ppCopy -> do
          copyPtr <- abcNtkDup networkPtr
          poke ppCopy copyPtr
          flip CE.finally (abcNtkDelete =<< peek ppCopy) $ do
            let params = proveParamsDefault { nItersMax'Prove_Params = 5 }
            result <- with params $ \paramsPtr ->
              abcNtkIvyProve ppCopy (castPtr paramsPtr)
            case result of
              -1 -> return Unknown
              0 -> liftIO $ do
                     pm <- peek ppCopy
                     ciNum <- abcNtkCiNum pm
                     m <- abcNtkModel pm
                     fmap Sat $ translateBoolArray ciNum m
              1 -> return UnSat
              _ -> fail $ "Unrecognized return code " ++ show result ++ " from abcNtkIvyProve"

  evalAigV inputs outputs =
    withOutputs outputs $ \networkPtr -> do
      -- Check number of inputs matches expectation.
      expectedInputCount <- abcNtkPiNum networkPtr
      CE.assert (LV.length inputs == expectedInputCount) $ return ()
      -- Perform simulation
      let intFromBool False = 0
          intFromBool True = 1
      foreignPtr <- LV.unsafeWith (LV.map intFromBool inputs) $ \ptr ->
                      abcNtkVerifySimulatePattern networkPtr ptr
      withForeignPtr foreignPtr $
        translateBoolArray (LV.length outputs)

  writeAigerV path outputs =
    withOutputs outputs $ \networkPtr -> do
      abcNtkShortNames networkPtr
      ioWriteAiger networkPtr path True False

writeDot :: MonadIO m => FilePath -> LV.Vector Lit -> AigComputation m ()
writeDot path outputs = do
  withOutputs outputs $ \networkPtr -> do
    ioWriteDot networkPtr path
