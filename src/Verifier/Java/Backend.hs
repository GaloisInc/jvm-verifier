{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ImplicitParams #-}
module Verifier.Java.Backend where

import Control.Applicative
import Control.Exception (assert)
import Data.Int
import Data.IORef
import qualified Data.Map as Map
import Verinf.Symbolic.Common
import Verinf.Symbolic
import Verinf.Symbolic.Lit.Functional
import qualified Data.Vector.Storable as SV

type BackendInt  sym = MonadTerm sym
type BackendLong sym = MonadTerm sym

data Backend sym = Backend {
          -- | Allocates a fresh variable where the 8 low-order bits are fresh
          -- lits and the upper bits are zero.
         freshByte :: IO (BackendInt sym)
       , freshInt  :: IO (BackendInt sym)
       , freshLong :: IO (BackendLong sym)
       , termBool :: Bool  -> IO (MonadTerm sym)  
       , termInt  :: Int32 -> IO (MonadTerm sym)
       , termLong :: Int64 -> IO (MonadTerm sym)
         -- | @applyGetArrayValue arr i@ returns value at @arr[i]@.
       , applyGetArrayValue :: MonadTerm sym -> MonadTerm sym -> IO (MonadTerm sym)
         -- | @applySetArrayValue arr i v@ returns value at @arr[i] = v@.
       , applySetArrayValue :: MonadTerm sym -> MonadTerm sym -> MonadTerm sym -> IO (MonadTerm sym)
       , blastTerm :: MonadTerm sym -> IO (Maybe Bool)
         -- | @evalAigIntegral f ins out@ applies concrete inputs @ins@ to the 
         -- AIG at the given symbolic output term @out@, applying @f@ to the
         -- @ins@ bit sequence
       ,  evalAigIntegral :: ([Bool] -> [Bool]) -> [CValue] 
                             -> MonadTerm sym -> IO (MonadTerm sym)
          -- | @evalAigArray w ins outs@ applies concrete inputs @ins@ to the
          -- AIG at the given symbolic output terms @outs@.  Each output is
          -- assumed to be w bits.
       , evalAigArray :: BitWidth -> [CValue] -> [MonadTerm sym] -> IO [MonadTerm sym]
       }
                   
getBackend :: SymbolicMonad (Backend SymbolicMonad)
getBackend = symbolicBackend <$> getSymState

symbolicBackend :: SymbolicMonadState -> Backend SymbolicMonad
symbolicBackend sms = do
  let ?be = smsBitEngine sms
  let de = smsDagEngine sms
  let lr = smsInputLitRef sms 
  let getTermLit = smsBitBlastFn sms
  let freshTerm tp lv = do  
        n@(InputTerm i _) <- deFreshInput (smsDagEngine sms) tp
        m <- readIORef lr
        writeIORef lr $! (Map.insert i (LV lv) m)
        return n
  Backend {
      freshByte = do
        inputs <- SV.replicateM 7 lMkInput
        msb <- lMkInput
        let lv = inputs SV.++ SV.replicate 25 msb
        freshTerm int32Type lv
    , freshInt = do
        lv <- SV.replicateM 32 lMkInput
        freshTerm int32Type lv
    , freshLong = do
        lv <- SV.replicateM 64 lMkInput
        freshTerm int64Type lv
    , termBool = return . mkCBool
    , termInt  = return . mkCInt 32 . fromIntegral
    , termLong = return . mkCInt 64 . fromIntegral
    , applyGetArrayValue = \a i ->
        case (termType a, termType i) of
          (SymArray len eltType, SymInt idxType) ->
            deApplyBinary de (getArrayValueOp len idxType eltType) a i
          _ -> error $ "internal: illegal arguments to applyGetArrayValue "
                        ++ prettyTerm a ++ "\n" ++ show (termType a)
    , applySetArrayValue = \a i v ->
        case (termType a,termType i) of
          (SymArray len eltType, SymInt idxType) ->
            deApplyTernary de (setArrayValueOp len idxType eltType) a i v
          _ -> error "internal: illegal arguments to applySetArrayValue"
    , blastTerm = \v -> do
        LV lv <- getTermLit v
        let l = assert (SV.length lv == 1) $ SV.head lv
        if l == lTrue then
          return (Just True)
        else if l == lFalse then
          return (Just False)
        else
          return Nothing
    , evalAigIntegral = \f ins out -> do
        outLits <- getTermLit out
        bbits <- lEvalAig (SV.fromList (concatMap (f . intToBoolSeq) ins))
                          (toLsbfV outLits)
        if SV.length bbits <= 32 then
          return $ mkCInt 32 $ fromIntegral $ boolSeqToInt32 (SV.toList bbits)
        else if SV.length bbits <= 64 then
          return $ mkCInt 64 $ fromIntegral $ boolSeqToInt64 (SV.toList bbits)
        else
          error "internal: evalAigIntegral: no support for arbitrary-width integers"
    , evalAigArray = \(Wx n) ins outs -> do
        let bits = case n of
                     8  -> evalAigArgs8  $ map intFromConst  ins
                     32 -> evalAigArgs32 $ map intFromConst  ins
                     64 -> evalAigArgs64 $ map longFromConst ins
                     _  -> error $ "evalAigArray: input array elements have unexpected bit width"
        outLits <- mapM getTermLit outs
        rs <- lEvalAig (SV.fromList bits)
                       (SV.concat (map toLsbfV outLits))
        let n' = SV.length rs `div` n
        let rsl = SV.toList rs
        case n of
          8  -> return $ map (mkCInt 32 . fromIntegral) $ outsToInts8 n' rsl
          32 -> return $ map (mkCInt 32 . fromIntegral) $ outsToInts32 n' rsl
          64 -> return $ map (mkCInt 64 . fromIntegral) $ outsToInts64 n' rsl
          _  -> error $ "evalAigArray: input array elements have unexpected bit width"
    }