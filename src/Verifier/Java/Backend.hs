{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ViewPatterns #-}
module Verifier.Java.Backend where

import Control.Applicative
import Control.Exception (assert)
import Data.Int
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Verinf.Symbolic.Common
import Verinf.Symbolic
import Verinf.Symbolic.Lit.Functional
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

type BackendInt  sym = MonadTerm sym
type BackendLong sym = MonadTerm sym

type UnaryOp sym = MonadTerm sym -> IO (MonadTerm sym)
type BinaryOp sym = MonadTerm sym -> MonadTerm sym -> IO (MonadTerm sym)

data Backend sym = Backend {
          -- | Allocates a fresh variable where the 8 low-order bits are fresh
          -- lits and the upper bits are zero.
         freshByte :: IO (BackendInt sym)
       , freshInt  :: IO (BackendInt sym)
       , freshLong :: IO (BackendLong sym)
       , asBool :: MonadTerm sym -> Maybe Bool
       , asInt :: MonadTerm sym -> Maybe Int32
       , asLong :: MonadTerm sym -> Maybe Int64
       , termBool :: Bool  -> IO (MonadTerm sym)  
       , termInt  :: Int32 -> IO (MonadTerm sym)
       , termLong :: Int64 -> IO (MonadTerm sym)
       , termByteFromInt :: UnaryOp sym
       , termLongFromInt :: UnaryOp sym
       , termIntFromLong :: UnaryOp sym
         -- | Complement argument.
       , termNot :: UnaryOp sym
         -- | Return conjunction of two arguments. 
       , termAnd :: BinaryOp sym
         -- | Compare equality of arguments.
       , termEq :: BinaryOp sym
         -- | Form if-then-else comparing arguments.
       , termIte :: MonadTerm sym -> MonadTerm sym -> MonadTerm sym -> IO (MonadTerm sym)
         -- | Bitwise and of arguments.
       , termIAnd :: BinaryOp sym
         -- | Bitwise or of arguments.
       , termIOr  :: BinaryOp sym
         -- | Bitwise exclusive or of arguments.
       , termIXor :: BinaryOp sym
         -- | Java shift-left on int values.
       , termIShl  :: BinaryOp sym
         -- | Java signed shift-right on int values.
       , termIShr  :: BinaryOp sym
         -- | Java unsigned shift-right on int values.
       , termIUshr :: BinaryOp sym
         -- | Java shift-left on long values.
       , termLShl  :: BinaryOp sym
         -- | Java signed shift-right on long values.
       , termLShr  :: BinaryOp sym
         -- | Java unsigned shift-right on long values.
       , termLUshr :: BinaryOp sym
         -- | Less than or equal.
       , termLeq :: BinaryOp sym
         -- |Less than
       , termLt :: BinaryOp sym
         -- | Negates argument
       , termNeg :: UnaryOp sym
         -- | Adds two arguments.
       , termAdd :: BinaryOp sym
         -- | Subtracts two integer arguments.
       , termSub :: BinaryOp sym
         -- | Multiplies two arguments.
       , termMul :: BinaryOp sym
         -- | Returns signed division of two arguments.
       , termDiv :: BinaryOp sym 
         -- | Returns signed remainder of two arguments.
       , termRem :: BinaryOp sym
         -- | @termIntArray l@ returns an integer array of zeros with length
         -- @l@.  Will return @Nothing@ is operation cannot be performed because
         -- length is symbolic and symbolic lengths are unsupported. 
       , termIntArray :: MonadTerm sym -> IO (Maybe (MonadTerm sym))
         -- | @termLongArray l@ returns a long array of zeros with length @l@.
       , termLongArray :: MonadTerm sym -> IO (Maybe (MonadTerm sym))
         -- | @applyGetArrayValue arr i@ returns value at @arr[i]@.
       , applyGetArrayValue :: BinaryOp sym
         -- | @applySetArrayValue arr i v@ returns value at @arr[i] = v@.
       , applySetArrayValue :: MonadTerm sym -> MonadTerm sym -> MonadTerm sym -> IO (MonadTerm sym)
       , blastTerm :: MonadTerm sym -> IO (Maybe Bool)
         -- | @evalAigIntegral f ins out@ applies concrete inputs @ins@ to the 
         -- AIG at the given symbolic output term @out@, applying @f@ to the
         -- @ins@ bit sequence
       , evalAigIntegral :: ([Bool] -> [Bool]) -> [MonadTerm sym] 
                             -> MonadTerm sym -> IO (MonadTerm sym)
         -- | @evalAigArray w ins outs@ applies concrete inputs @ins@ to the
         -- AIG at the given symbolic output terms @outs@.  Each output is
         -- assumed to be w bits.  If @ins@ is not a constant, then this fails.
       , evalAigArray :: BitWidth -> [MonadTerm sym] -> [MonadTerm sym] -> IO [MonadTerm sym]
       , writeAigToFile :: FilePath -> SV.Vector Lit -> IO ()
         -- | Returns lit vector associated with given term, or fails
         -- if term cannot be bitblasted.
       , getVarLit :: MonadTerm sym -> IO (LitResult Lit)
       }
                   
getBackend :: SymbolicMonad (Backend SymbolicMonad)
getBackend = symbolicBackend <$> getSymState

symbolicBackend :: SymbolicMonadState -> Backend SymbolicMonad
symbolicBackend sms = do
  let ?be = smsBitEngine sms
  let de = smsDagEngine sms
  let oc = smsOpCache sms
  let lr = smsInputLitRef sms 
  let getTermLit = smsBitBlastFn sms
  let freshTerm tp lv = do  
        n@(InputTerm i _) <- deFreshInput (smsDagEngine sms) tp
        m <- readIORef lr
        writeIORef lr $! (Map.insert i (LV lv) m)
        return n
  let termTrunc resultWidth t = do
        case termType t of
          SymInt iw@(widthConstant -> Just inputWidth)
            | inputWidth >= resultWidth ->
              deApplyUnary de (truncOp oc iw resultWidth) t
          _ -> error "internal: illegal arguments to termTrunc"
  let termBinaryIntOp opFn name x y =
        case termType x of
          SymInt w -> deApplyBinary de (opFn w) x y
          _ -> error $ "internal: illegal value to " ++ name ++ ": " ++ show x
  let termShift opFn name w x y = do
        case (termType x, termType y) of
          (SymInt vw, SymInt sw@(widthConstant -> Just inputWidth))
            | inputWidth >= w -> do
                y' <- deApplyUnary de (truncOp oc sw w) y
                deApplyBinary de (opFn vw (constantWidth w)) x y'
          _ -> error $ "internal: illegal value to " ++ name ++ ": " ++ show x
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
    , asBool = getBool
    , asInt = \x -> fromInteger <$> getSVal x
    , asLong = \x -> fromInteger <$> getSVal x
    , termBool = return . mkCBool
    , termInt  = return . mkCInt 32 . toInteger
    , termLong = return . mkCInt 64 . toInteger
    , termByteFromInt = \x -> termTrunc 8 x
    , termLongFromInt = \t ->
        case termType t of
          SymInt iw@(widthConstant -> Just inputWidth)
            | inputWidth <= 64  -> 
              deApplyUnary de (signedExtOp oc iw 64) t
          _ -> error "internal: illegal value to termLongFromInt"
    , termIntFromLong = \x -> termTrunc 32 x
    , termNot  = deApplyUnary de bNotOp
    , termAnd  = deApplyBinary de bAndOp
    , termEq   = \x y -> deApplyBinary de (eqOp (termType x)) x y
    , termIte  = \b t f ->
        case getBool b of
          Just True -> return t
          Just False -> return f
          Nothing | t == f -> return t
                  | otherwise -> deApplyTernary de (iteOp (termType t)) b t f
    , termIAnd  = termBinaryIntOp iAndOp "termIAnd"
    , termIOr   = termBinaryIntOp iOrOp  "termIOr"
    , termIXor  = termBinaryIntOp iXorOp "termIXor"
    , termIShl  = termShift shlOp  "termIShl"  5 
    , termIShr  = termShift shrOp  "termIShr"  5
    , termIUshr = termShift ushrOp "termIUshr" 5 
    , termLShl  = termShift shlOp  "termLShl"  6
    , termLShr  = termShift shrOp  "termLShr"  6
    , termLUshr = termShift ushrOp "termLUshr" 6
    , termLeq = termBinaryIntOp signedLeqOp "termLeq"
    , termLt  = termBinaryIntOp signedLtOp  "termLt"
    , termNeg = \x ->
        case termType x of
          SymInt w -> deApplyUnary de (negOp w) x
          _ -> error "internal: illegal value to termNeg"
    , termAdd = termBinaryIntOp addOp "termAdd"
    , termSub = termBinaryIntOp subOp "termSub"
    , termMul = termBinaryIntOp mulOp "termMul"
    , termDiv = termBinaryIntOp signedDivOp "termDiv"
    , termRem = termBinaryIntOp signedRemOp "termRem"
    , termIntArray = \l ->
        case fromIntegral <$> getSVal l of
          Just c ->
            Just <$> deApplyOp de (mkArrayOp oc c int32Type)
                       (V.replicate c (mkCInt 32 0))  
          Nothing -> return Nothing
    , termLongArray = \l ->
        case fromIntegral <$> getSVal l of
          Just c ->
            Just <$> deApplyOp de (mkArrayOp oc c int64Type)
                       (V.replicate c (mkCInt 64 0))  
          Nothing -> return Nothing
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
        bbits <- lEvalAig (SV.fromList (concatMap (f . intToBoolSeq . fromJust . termConst) ins))
                          (toLsbfV outLits)
        if SV.length bbits <= 32 then
          return $ mkCInt 32 $ fromIntegral $ boolSeqToInt32 (SV.toList bbits)
        else if SV.length bbits <= 64 then
          return $ mkCInt 64 $ fromIntegral $ boolSeqToInt64 (SV.toList bbits)
        else
          error "internal: evalAigIntegral: no support for arbitrary-width integers"
    , evalAigArray = \(Wx n) ins outs -> do
        -- TODO: Report sensible error if ins is not constants.
        let bits = case n of
                     8  -> evalAigArgs8  $ map (fromInteger . fromJust . getSVal) ins
                     32 -> evalAigArgs32 $ map (fromInteger . fromJust . getSVal) ins
                     64 -> evalAigArgs64 $ map (fromInteger . fromJust . getSVal) ins
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
   , writeAigToFile = \fname res -> lWriteAiger fname [res]
   , getVarLit = getTermLit
   }