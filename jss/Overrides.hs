{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Overrides (jssOverrides) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Vector.Storable as SV

import Text.PrettyPrint

import Verifier.Java.Simulator
import Verifier.Java.Utils

-- | Register all predefined overrides for the com.galois.symbolic.Symbolic
-- API.
jssOverrides :: MonadSim sbe m => Simulator sbe m ()
jssOverrides = do
  sbe <- use backend
  let m `pushAs` f  = pushValue =<< f <$> liftIO (m sbe)
  let evalAigBody f chkInps dtor ctor out cvArr = do
--        abortWhenMultiPath "AIG evaluation (scalar)"
        cinps <- map dtor <$> (chkInps =<< getInputs cvArr)
        pushValue =<< liftIO (ctor <$> evalAigIntegral sbe f cinps out)
  let evalAigArrayBody w chkInps getArr dtor push [RValue outs, RValue cvArr] = do
--        abortWhenMultiPath "AIG evaluation (array)"
        ins     <- chkInps =<< getInputs cvArr
        outLits <- getArr outs
        push =<< liftIO
                 (evalAigArray sbe
                               w
                               (map dtor ins)
                               outLits)
      evalAigArrayBody _ _ _ _ _ _ = error "invalid evalAigArrayBody parameters"
  let writeCnf fnameRef out = do
        mfn <- lookupStringRef fnameRef
        case mfn of
          Nothing -> abort $ "writeCnf filename parameter does "
                       ++ "not refer to a constant string"
          Just fn -> liftIO $ do
            zero <- termInt sbe 0
            cEq <- termEq sbe out zero
            l <- getVarLit sbe cEq
            writeCnfToFile sbe fn (SV.head l)
  let writeAigerBody f fnameRef outs = do
--        abortWhenMultiPath "AIGER write"
        mfn <- lookupStringRef fnameRef
        case mfn of
          Nothing -> abort $ "writeAiger filename parameter does "
                       ++ "not refer to a constant string"
          Just fn -> liftIO $
            writeAigToFile sbe fn . SV.concat
              =<< mapM (fmap f . getVarLit sbe) outs
  mapM_ (\(cn, key, impl) -> overrideStaticMethod cn key impl)
      --------------------------------------------------------------------------------
      -- fresh vars & path info

      -- TODO: Make the overriden functions be total and handle errors if, e.g.,
      -- user passes a symbolic value where a concrete one is expected
      -- (getPathDescriptors, for instance).
    [ sym "freshByte" "(B)B"    $ \_ -> freshByte `pushAs` IValue
    , sym "freshInt" "(I)I"     $ \_ -> freshInt  `pushAs` IValue
    , sym "freshBoolean" "(Z)Z" $ \_ -> freshInt  `pushAs` IValue
    , sym "freshLong" "(J)J"    $ \_ -> freshLong `pushAs` LValue
    , sym "freshByteArray" "(I)[B" $ \[IValue (asInt sbe -> Just n)] ->
        pushByteArr =<< liftIO (replicateM (fromIntegral n) $ freshByte sbe)
    , sym "freshIntArray" "(I)[I" $ \[IValue (asInt sbe -> Just n)] ->
        pushIntArr =<< liftIO (replicateM (fromIntegral n) $ freshInt sbe)
    , sym "freshLongArray" "(I)[J" $ \[IValue (asInt sbe -> Just n)] ->
        pushLongArr =<< liftIO (replicateM (fromIntegral n) $ freshLong sbe)
    , sym "getPathDescriptors" "(Z)[I" $ \[IValue (asInt sbe -> Just _)] ->
          pushIntArr []
{- No more global map of path states, so this is kind of meaningless now
        let filterExc PathState{ finalResult = fr } =
              if includeExc /= 0
                then True
                else case fr of Exc{} -> False; _ -> True
        in pushIntArr
             =<< mapM (liftIO . termInt sbe . fromIntegral . unPSS)
               =<< (M.keys . M.filter filterExc <$> gets pathStates)
-}
      --------------------------------------------------------------------------------
      -- evalAig

      -- symbolic byte AIG output -> concrete byte
    , sym "evalAig" "(B[Lcom/galois/symbolic/CValue;)B" $ \[IValue out, RValue cvArr] ->
        evalAigBody (take 8) assertAllInts unIValue IValue out cvArr

      -- symbolic int AIG output -> concrete int
    , sym "evalAig" "(I[Lcom/galois/symbolic/CValue;)I" $ \[IValue out, RValue cvArr] ->
        evalAigBody id assertAllInts unIValue IValue out cvArr

      -- symbolic long AIG output -> concrete long
    , sym "evalAig" "(J[Lcom/galois/symbolic/CValue;)J" $ \[LValue out, RValue cvArr] ->
        evalAigBody id assertAllLongs unLValue LValue out cvArr

      -- symbolic byte array AIG output -> concrete byte array
    , sym "evalAig" "([B[Lcom/galois/symbolic/CValue;)[B" $
        evalAigArrayBody 8 assertAllInts getByteArray unIValue pushByteArr

      -- symbolic int array AIG output -> concrete int array
    , sym "evalAig" "([I[Lcom/galois/symbolic/CValue;)[I" $
        evalAigArrayBody 32 assertAllInts getIntArray unIValue pushIntArr

      -- symbolic long array AIG output -> long array
    , sym "evalAig" "([J[Lcom/galois/symbolic/CValue;)[J" $
        evalAigArrayBody 64 assertAllLongs getLongArray unLValue pushLongArr

      --------------------------------------------------------------------------------
      -- writeAiger

    , sym "writeAiger" "(Ljava/lang/String;Z)V"  $ \([RValue fnameRef, IValue out]) ->
        writeAigerBody (SV.take 1) fnameRef [out]
    , sym "writeAiger" "(Ljava/lang/String;B)V"  $ \([RValue fnameRef, IValue out]) ->
        writeAigerBody id fnameRef [out]
    , sym "writeAiger" "(Ljava/lang/String;I)V"  $ \([RValue fnameRef, IValue out]) ->
        writeAigerBody id fnameRef [out]
    , sym "writeAiger" "(Ljava/lang/String;J)V"  $ \([RValue fnameRef, LValue out]) ->
        writeAigerBody id fnameRef [out]
    , sym "writeAiger" "(Ljava/lang/String;[B)V" $ \([RValue fnameRef, RValue outs]) ->
        writeAigerBody id fnameRef =<< getByteArray outs
    , sym "writeAiger" "(Ljava/lang/String;[I)V" $ \([RValue fnameRef, RValue outs]) ->
        writeAigerBody id fnameRef =<< getIntArray outs
    , sym "writeAiger" "(Ljava/lang/String;[J)V" $ \([RValue fnameRef, RValue outs]) ->
        writeAigerBody id fnameRef =<< getLongArray outs
    , sym "writeCnf" "(Ljava/lang/String;Z)V" $ \([RValue fnameRef, IValue out]) ->
        writeCnf fnameRef out

      --------------------------------------------------------------------------------
      -- debugging

    , dbg "trace" "(I)V" $ \[v@IValue{}] -> dbugM . render $ ppValue sbe v
    , dbg "trace" "(Ljava/lang/String;I)V" $ \[RValue msgRef, v@IValue{}] -> do
        mmsg <- lookupStringRef msgRef
        case mmsg of
          Just msg -> dbugM $ "(JSS) " ++ msg ++ ": " ++ render (ppValue sbe v)
          _ -> error "Symbolic.Debug.trace expects interned message strings"
    , dbg "trace" "(J)V" $ \[v@LValue{}] -> dbugM . render $ ppValue sbe v
    , dbg "trace" "(Ljava/lang/String;J)V" $ \[RValue msgRef, v@LValue{}] -> do
        mmsg <- lookupStringRef msgRef
        case mmsg of
          Just msg -> dbugM $ "(JSS) " ++ msg ++ ": " ++ render (ppValue sbe v)
          _ -> error "Symbolic.Debug.trace expects interned message strings"

    , dbg "abort" "()V"          $ \_ -> abort "Abort explicitly triggered (via JAPI)."
    , dbg "dumpPathStates" "()V" $ \_ -> dumpCurrentPath
    , dbg "setVerbosity" "(I)V"  $ \[IValue (asInt sbe -> Just v)] ->
        setVerbosity (fromIntegral v)
    , dbg "eval" "(I[Lcom/galois/symbolic/CValue;)I" $ \[IValue _out, RValue _cvArr] -> do
        error "debug dag eval / bitblast integrity checker NYI"
    ]
  where
    sym meth md f = (symbolicCN, makeMethodKey meth md, f)
    dbg meth md f = (symbolicCN ++ "$Debug", makeMethodKey meth md, f)
    symbolicCN    = "com/galois/symbolic/Symbolic"
    pushByteArr   = pushValue . RValue <=< newIntArray byteArrayTy
    pushIntArr    = pushValue . RValue <=< newIntArray intArrayTy
    pushLongArr   = pushValue . RValue <=< newLongArray 

    assertAllInts xs = do
      when (any (\x -> case x of IValue{} -> False; _ -> True) xs) $
        abort "JAPI: expected CValue operands to represent type int "
      return xs

    assertAllLongs xs = do
      when (any (\x -> case x of LValue{} -> False; _ -> True) xs) $
        abort "JAPI: expected CValue operands to represent type long "
      return xs
    getInputs cvArr = do
      cvalRefs <- getRefArray cvArr
      forM cvalRefs $ \r -> do
        mty <- typeOf r
        case mty of
          Nothing -> abort "Null reference encountered in concrete value array"
          Just ty
            | ty == ClassType ciCN -> unboxCV ciCN r "()I"
            | ty == ClassType czCN -> unboxCV czCN r "()Z"
            | ty == ClassType cjCN -> unboxCV cjCN r "()J"
            | ty == ClassType cbCN -> unboxCV cbCN r "()B"
            | otherwise ->
                abort $ "evalAig concrete value operand type is not supported"
      where
        cbCN              = "com/galois/symbolic/CValue$CByte"
        ciCN              = "com/galois/symbolic/CValue$CInt"
        czCN              = "com/galois/symbolic/CValue$CBool"
        cjCN              = "com/galois/symbolic/CValue$CLong"
        unboxCV cn ref td = do
          mp <- execInstanceMethod cn (makeMethodKey "getValue" td) ref []
          case mp of
            Just v -> return v
            Nothing -> fail "getInputs: no return value"
