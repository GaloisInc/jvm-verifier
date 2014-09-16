
{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Verinf.Symbolic.Lit.ABC 
  ( module Verinf.Symbolic.Lit.DataTypes
  , Lit
  , createBitEngine
  , prettyLit
  ) where

import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad.Reader
import qualified Data.Traversable as T
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as LV
import qualified Data.Vector.Storable.Mutable as LV (new,write)
import Foreign hiding (new)
import Foreign.C

import Data.ABC (initialize)
import Data.ABC.Internal.ABC
import Data.ABC.Internal.AIG
import Data.ABC.Internal.CNF
    ( cnfVarNums
    , cnfDataWriteIntoFileWithHeader
    , withCnfDerive
    )
import Data.ABC.Internal.FRAIG ( proveParamsDefault, Prove_Params_t_(..) )
import Data.ABC.Internal.IO (ioWriteAiger)
import Data.ABC.Internal.Orphan (abcNtkToDar)
import Data.ABC.Internal.VecPtr

import Verinf.Symbolic.Lit.DataTypes

newtype Lit = Lit { unLit :: Abc_Obj_t }
  deriving (Eq, Ord, Storable, Typeable)

instance Show Lit where
  show = prettyLit

-- Aig functions

prettyLit :: Lit -> String
prettyLit (Lit o) = "(" ++ show o ++ ")"

-- | Negate a literal
neg :: Lit -> Lit
neg (Lit o) = Lit $ wordPtrToPtr $ ptrToWordPtr o `xor` 0x1

-- | Translates model to Boolean result.
translateBoolArray :: Int -> Ptr CInt -> IO (LV.Vector Bool)
translateBoolArray len pModel = do
  mv <- LV.new len
  LV.forM_ (LV.enumFromN 0 len) $ \i ->
    LV.write mv i . (/= 0) =<< peekElemOff pModel i
  LV.freeze mv

-- | Add the given outputs to the network, execute the IO action, and
-- remove the outputs.
withOutputs :: Abc_Ntk_t -> LV.Vector Lit -> IO a -> IO a
withOutputs networkPtr outputs m = do
  outputNodes <- LV.forM outputs $ \(Lit outNodePtr) -> do
    po <- abcNtkCreateObj networkPtr AbcObjPo
    abcObjAddFanin po outNodePtr
    return po
  v <- m
  LV.forM_ outputNodes abcNtkDeleteObj
  return v

-- | Returns vector where each index corresponds to a variable id, and
-- maps it to CNF variable id. If the second argument is non-empty, it
-- lists literals that should be quantified according to QDIMACS syntax.
writeCnf :: Abc_Ntk_t -> FilePath -> Quantifiers Lit -> Lit -> IO (V.Vector (Maybe Int))
writeCnf pNtk path qs l = withOutputs pNtk (LV.singleton l) $
  CE.bracket (abcNtkToDar pNtk False False) aigManStop $ \pMan -> do
  n <- aigManObjNumMax pMan
  withCnfDerive pMan 0 $ \pCnf -> do
    pVars <- cnfVarNums pCnf
    let nodeIds = V.enumFromN 0 (fromIntegral n)
    varMap <- V.mapM (peekElemOff pVars) nodeIds
    let varIdx :: Lit -> IO CInt
        varIdx (Lit o) = (varMap V.!) . fromIntegral <$> abcObjId o
    qns <- T.mapM varIdx qs
    cnfDataWriteIntoFileWithHeader pCnf path (ppQDIMACSQuantifiers qns) 0
    return $ (Just . fromIntegral) <$> varMap

-- | Create Bit engine for ABC.
createBitEngine :: IO (BitEngine Lit)
createBitEngine = do
  initialize
  networkPtr <- abcNtkAlloc AbcNtkStrash AbcFuncAig True
  flip CE.onException (abcNtkDelete networkPtr) $ do
    true <- Lit <$> abcAigConst1 networkPtr
    return
      BitEngine
        { beTrue = true
        , beFalse = neg true
        , beNeg = neg
        , beAnd = \x y -> do
            manFunc <- castPtr <$> abcNtkManFunc networkPtr
            Lit <$> abcAigAnd manFunc (unLit x) (unLit y)
        , beXor = \x y -> do
            manFunc <- castPtr <$> abcNtkManFunc networkPtr
            Lit <$> abcAigXor manFunc (unLit x) (unLit y)
        , beMux = \c t f -> do
            manFunc <- castPtr <$> abcNtkManFunc networkPtr
            Lit <$> abcAigMux manFunc (unLit c) (unLit t) (unLit f)
        , beEqLit = (==)
        , beInputLitCount = abcNtkPiNum networkPtr
        , beInputLits = do
            p <- abcNtkPis networkPtr
            sz <- vecPtrSize p
            a <- vecPtrArray p
            LV.generateM (fromIntegral sz) (fmap Lit . peekElemOff a)
        , beMakeInputLit =  Lit <$> abcNtkCreateObj networkPtr AbcObjPi
        , beCheckSat = Just $ \(Lit outNodePtr) -> do
            po <- abcNtkCreateObj networkPtr AbcObjPo
            flip CE.finally (abcNtkDeleteObj po) $ do
              abcObjAddFanin po outNodePtr
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
                           Sat <$> translateBoolArray ciNum m
                    1 -> return UnSat
                    _ -> fail $ "Unrecognized return code " ++ show result
                                 ++ " from abcNtkIvyProve"
        , beEvalAigV = \inputs outputs ->
            withOutputs networkPtr outputs $ do
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
        , beWriteAigerV = \path _ outputs ->
            withOutputs networkPtr outputs $ do
              ntkCpy  <- abcNtkDup networkPtr
              manFunc <- castPtr <$> abcNtkManFunc ntkCpy
              _ <- abcAigCleanup manFunc
              abcNtkShortNames ntkCpy
              ioWriteAiger ntkCpy path True False False
        , beWriteCNF = \path qs output -> do
            writeCnf networkPtr path qs output
        , beFree = abcNtkDelete networkPtr
        }
