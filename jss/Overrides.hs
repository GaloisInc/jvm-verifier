{-# LANGUAGE ViewPatterns #-}
module Overrides (jssOverrides) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector.Storable as SV

import Execution.JavaSemantics
import JavaParser
import Simulation
import Utils.Common
import Verinf.Symbolic

-- | Register all predefined overrides for the com.galois.symbolic.Symbolic
-- API.
jssOverrides :: (AigOps sym) => Simulator sym ()
jssOverrides = do
  b <- gets backend
  mapM_ (\(cn, key, impl) -> overrideStaticMethod cn key impl)
      --------------------------------------------------------------------------------
      -- fresh vars & path info

      -- TODO: Make the overriden functions be total and handle errors if, e.g.,
      -- user passes a symbolic value where a concrete one is expected
      -- (getPathDescriptors, for instance).
    [ sym "freshByte" "(B)B"    $ \_ -> freshByte `pushAs` IValue
    , sym "freshInt" "(I)I"     $ \_ -> freshInt  `pushAs` IValue
    , sym "freshBoolean" "(Z)Z" $ \_ -> freshInt  `pushAs` IValue
    , sym "freshLong" "(J)J"    $ \_ -> 
       pushValue =<< LValue <$> liftIO (freshLong b) 
    , sym "freshByteArray" "(I)[B" $ \[IValue (intVal -> Just n)] ->
        pushByteArr =<< replicateM n (liftSymbolic freshByte)

    , sym "freshIntArray" "(I)[I" $ \[IValue (intVal -> Just n)] ->
        pushIntArr =<< replicateM n (liftSymbolic freshInt)

    , sym "freshLongArray" "(I)[J" $ \[IValue (intVal -> Just n)] ->
        pushLongArr =<< liftIO (replicateM n (freshLong b))

    , sym "getPathDescriptors" "(Z)[I" $ \[IValue (getSVal -> Just includeExc)] ->
        let filterExc PathState{ finalResult = fr } =
              if includeExc /= 0
                then True
                else case fr of Exc{} -> False; _ -> True
        in pushIntArr
             =<< mapM (liftSymbolic . termInt . fromIntegral . unPSS)
               =<< (M.keys . M.filter filterExc <$> gets pathStates)

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
        writeAigerBody id fnameRef =<< (`getByteArray` outs) =<< getPSS
    , sym "writeAiger" "(Ljava/lang/String;[I)V" $ \([RValue fnameRef, RValue outs]) ->
        writeAigerBody id fnameRef =<< (`getIntArray` outs) =<< getPSS
    , sym "writeAiger" "(Ljava/lang/String;[J)V" $ \([RValue fnameRef, RValue outs]) ->
        writeAigerBody id fnameRef =<< (`getLongArray` outs) =<< getPSS

      --------------------------------------------------------------------------------
      -- debugging

    , dbg "trace" "(I)V" $ \[v@IValue{}] -> dbugM $ ppValue v
    , dbg "trace" "(Ljava/lang/String;I)V" $ \[RValue msgRef, v@IValue{}] -> do
        mmsg <- lookupStringRef msgRef
        case mmsg of
          Just msg -> dbugM $ "(JSS) " ++ msg ++ ": " ++ ppValue v
          _ -> error "Symbolic.Debug.trace expects interned message strings"
    , dbg "trace" "(J)V" $ \[v@LValue{}] -> dbugM $ ppValue v
    , dbg "trace" "(Ljava/lang/String;J)V" $ \[RValue msgRef, v@LValue{}] -> do
        mmsg <- lookupStringRef msgRef
        case mmsg of
          Just msg -> dbugM $ "(JSS) " ++ msg ++ ": " ++ ppValue v
          _ -> error "Symbolic.Debug.trace expects interned message strings"

    , dbg "abort" "()V"          $ \_ -> abort "Abort explicitly triggered (via JAPI)."
    , dbg "dumpPathStates" "()V" $ \_ -> dumpPathStates
    , dbg "setVerbosity" "(I)V"  $ \[IValue (intVal -> Just v)] ->
        setVerbosity v
    , dbg "eval" "(I[Lcom/galois/symbolic/CValue;)I" $ \[IValue _out, RValue _cvArr] -> do
        error "debug dag eval / bitblast integrity checker NYI"
    ]
  where
    intVal = fmap fromInteger  . getSVal
    abortWhenMultiPath msg = do
      finished <- snd <$> splitFinishedPaths
      when (length finished > 1) $ do
        dbugM $ "Error: " ++ msg ++ ": There are multiple non-exception paths.  Displaying all path states:"
        dumpPathStates
        abort $ msg ++ ": only permitted when there's one non-exception path state."

    sym meth md f = (symbolicCN, makeMethodKey meth md, f)
    dbg meth md f = (symbolicCN ++ "$Debug", makeMethodKey meth md, f)
    symbolicCN    = "com/galois/symbolic/Symbolic"
    m `pushAs` f  = pushValue =<< f <$> liftSymbolic m
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

    evalAigBody f chkInps dtor ctor out cvArr = do
      abortWhenMultiPath "AIG evaluation (scalar)"
      cinps <- map (fromJust . termConst . dtor)
               <$> (chkInps =<< getInputs cvArr)
      evalAigIntegral f cinps out `pushAs` ctor

    evalAigArrayBody w chkInps getArr dtor push [RValue outs, RValue cvArr] = do
      abortWhenMultiPath "AIG evaluation (array)"
      pd      <- getPSS
      ins     <- chkInps =<< getInputs cvArr
      outLits <- getArr pd outs
      push =<< liftSymbolic
                 (evalAigArray w
                               (map (fromJust . termConst . dtor) ins)
                               outLits
                 )
    evalAigArrayBody _ _ _ _ _ _ = error "invalid evalAigArrayBody parameters"

    writeAigerBody f fnameRef outs = do
      abortWhenMultiPath "AIGER write"
      mfn <- lookupStringRef fnameRef
      case mfn of
        Nothing -> abort $ "writeAiger filename parameter does "
                         ++ "not refer to a constant string"
        Just fn -> liftSymbolic
                   $ writeAigToFile fn . SV.concat
                       =<< mapM (fmap (f . toLsbfV) . getVarLit) outs

    getInputs cvArr = do
      cvalRefs <- getPSS >>= (`getRefArray` cvArr)
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
          invokeInstanceMethod cn (makeMethodKey "getValue" td) ref []
          runFrame
          popValue