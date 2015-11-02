{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

{- |
Module           : $Header$
Description      :
License          : Free for non-commercial use. See LICENSE.
Stability        : provisional
Point-of-contact : acfoltzer
-}
module Overrides (jssOverrides) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Lens
import Control.Monad
import Control.Monad.State

import Text.PrettyPrint

import Verifier.Java.Simulator

-- | Register all predefined overrides for the com.galois.symbolic.Symbolic
-- API.
jssOverrides :: forall sbe m . MonadSim sbe m => Simulator sbe m ()
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
            -- Elsewhere in JSS SAWCore booleans have been translated
            -- into ints, by the translation
            --
            --   b |-> if b then 1 else 0
            --
            -- However, those ints are 32 bit, and for CNF we want a
            -- single bit of output. So, here we recover a single bit,
            -- by going back to a SAWCore equality:
            --
            --   e |-> not (e == 0)
            --
            -- We use this translation because 'javac' compiles
            -- boolean false to zero in JVM, similar to treatment of
            -- booleans in C. However, if all the booleans are coming
            -- from the above if-then-else translation, we could just
            -- as well translate
            --
            --   e |-> e == 1
            --
            -- and better yet just undo the if-then-else translation,
            -- i.e.
            --
            --   (if b then 1 else 0 ) |-> b
            --
            -- when possible. Experiments show this is not always
            -- possible though: while equality comparisons translate
            -- via if-then-else, symbolic booleans do not, and boolean
            -- literals pass through untouched, presumably due to
            -- "smart" constructors which do constant propagation.
            zero <- termInt sbe 0
            cEq <- termEq sbe out zero
            cNeq <- termNot sbe cEq
            writeCnfToFile sbe fn cNeq
  let writeAigerBody :: Ref   -- file name
                     -> Ref   -- array of input values
                     -> [SBETerm sbe]  -- output values
                     -> Simulator sbe m ()
      writeAigerBody fnameRef inRef outs = do
--        abortWhenMultiPath "AIGER write"
        ins <- mapM unwrapVal =<< getInputs inRef
        mfn <- lookupStringRef fnameRef
        case mfn of
          Nothing -> abort $ "writeAiger filename parameter does "
                       ++ "not refer to a constant string"
          Just fn -> liftIO $
            writeAigToFile sbe fn ins outs
  mapM_ (\(cn, key, impl) -> overrideStaticMethod cn key impl)
      --------------------------------------------------------------------------------
      -- fresh vars

      -- TODO: Make the overriden functions be total and handle errors if, e.g.,
      -- user passes a symbolic value where a concrete one is expected
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

    , sym "writeAiger" "(Ljava/lang/String;Z[Ljava/lang/Object;)V"  $ \([RValue fnameRef, IValue out, RValue ins]) -> do
        neq <- liftIO $ termNot sbe =<< termEq sbe out =<< termInt sbe 0
        writeAigerBody fnameRef ins [neq]
    , sym "writeAiger" "(Ljava/lang/String;B[Ljava/lang/Object;)V"  $ \([RValue fnameRef, IValue out, RValue ins]) ->
        writeAigerBody fnameRef ins [out]
    , sym "writeAiger" "(Ljava/lang/String;I[Ljava/lang/Object;)V"  $ \([RValue fnameRef, IValue out, RValue ins]) ->
        writeAigerBody fnameRef ins [out]
    , sym "writeAiger" "(Ljava/lang/String;J[Ljava/lang/Object;)V"  $ \([RValue fnameRef, LValue out, RValue ins]) ->
        writeAigerBody fnameRef ins [out]
    , sym "writeAiger" "(Ljava/lang/String;[B[Ljava/lang/Object;)V" $ \([RValue fnameRef, RValue outs, RValue ins]) ->
        writeAigerBody fnameRef ins =<< getByteArray outs
    , sym "writeAiger" "(Ljava/lang/String;[I[Ljava/lang/Object;)V" $ \([RValue fnameRef, RValue outs, RValue ins]) ->
        writeAigerBody fnameRef ins =<< getIntArray outs
    , sym "writeAiger" "(Ljava/lang/String;[J[Ljava/lang/Object;)V" $ \([RValue fnameRef, RValue outs, RValue ins]) ->
        writeAigerBody fnameRef ins =<< getLongArray outs

      --------------------------------------------------------------------------------
      -- writeCnf
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
    , dbg "dumpPathState" "()V" $ \_ -> dumpCurrentPath
    , dbg "dumpMemState"  "()V" $ \_ -> dumpMemory (text "dumpMemState")
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

    unwrapVal (IValue x) = return x
    unwrapVal (LValue x) = return x
    unwrapVal _ = fail "expected integer or long value"

    assertAllInts xs = do
      when (any (\x -> case x of IValue{} -> False; _ -> True) xs) $
        abort "JAPI: expected CValue operands to represent type int "
      return xs

    assertAllLongs xs = do
      when (any (\x -> case x of LValue{} -> False; _ -> True) xs) $
        abort "JAPI: expected CValue operands to represent type long "
      return xs

    -- Given an array of references, unbox the contained values, which are assumed
    -- to be boxed primitives.  The boxes may either be the built-in Java
    -- box types (Boolean, Byte Integer, Long) or our custom box types
    -- (CValue.CBool, CValue.CByte, CValue.CInt, CValue.CLong).
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
            | ty == ClassType  iCN -> unbox    iCN r "intValue" "()I"
            | ty == ClassType  zCN -> unbox    zCN r "booleanValue" "()Z"
            | ty == ClassType  jCN -> unbox    jCN r "longValue" "()J"
            | ty == ClassType  bCN -> unbox    bCN r "byteValue" "()B"
            | otherwise ->
                abort $ "evalAig concrete value operand type is not supported"
      where
        cbCN              = "com/galois/symbolic/CValue$CByte"
        ciCN              = "com/galois/symbolic/CValue$CInt"
        czCN              = "com/galois/symbolic/CValue$CBool"
        cjCN              = "com/galois/symbolic/CValue$CLong"
        bCN               = "java/lang/Byte"
        iCN               = "java/lang/Integer"
        zCN               = "java/lang/Boolean"
        jCN               = "java/lang/Long"

        unboxCV cn ref td = unbox cn ref "getValue" td

        unbox cn ref methName td = do
          mp <- execInstanceMethod cn (makeMethodKey methName td) ref []
          case mp of
            Just v -> return v
            Nothing -> fail "getInputs: no return value"
