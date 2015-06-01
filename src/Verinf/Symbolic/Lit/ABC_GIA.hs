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
{-# LANGUAGE CPP #-}
module Verinf.Symbolic.Lit.ABC_GIA
   ( Lit
   , prettyLit
   , View(..)
     -- * Network functions.
   , Network(..)
   , createEmptyNetwork
   , readAiger
   , freeNetwork
   , appendNetworkInput
   , networkInputCount
   , networkInputs
   , networkOutputs
   , bitEngineForNetwork
   , litEvaluator
     -- * BitEngine function.
   , createBitEngine
   ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception
import Control.Monad
import qualified Data.Foldable as Fold
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Traversable as T
import qualified Data.Vector.Storable as LV
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import Foreign hiding (new, void)
import Foreign.C

import Data.ABC (initialize)
import Data.ABC.Internal.AIG
import Data.ABC.Internal.CEC
import Data.ABC.Internal.CNF
import Data.ABC.Internal.GIA
import Data.ABC.Internal.GiaAig
import Data.ABC.Internal.VecInt

import Verinf.Symbolic.Lit.DataTypes

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb act = mb >>= \b -> when b act 

bracketForeignPtr :: IO (ForeignPtr a)
                  -> (ForeignPtr a -> IO b)
                  -> IO b
bracketForeignPtr ctor = bracket ctor finalizeForeignPtr

type Lit = GiaLit

prettyLit :: Lit -> String
prettyLit o = "(" ++ show (unGiaLit o) ++ ")"

giaObjValueIsAssigned :: Gia_Obj_t -> IO Bool
giaObjValueIsAssigned obj = (/= complement 0) <$> getGiaObjValue obj

-- | Assign lit to value field of GIA object.
assignGiaObjValue :: Gia_Obj_t -> GiaLit -> IO ()
assignGiaObjValue obj l = do
  whenM (giaObjValueIsAssigned obj) $ do
    v <- getGiaObjValue obj
    fail $ "internal: Attempt to copy GIA node multiple times.  " ++ show v
  setGiaObjValue obj $ fromIntegral $ unGiaLit l

-- | Create a fresh literal in new GIA that is associated to lit in
-- original GIA.
sliceGiaLit :: Gia_Man_t -- ^ GIA to add new input to.
            -> Gia_Man_t -- ^ Original git
            -> GiaLit -- ^ Literal in old git.
            -> IO ()
sliceGiaLit m' m l = do
  input <- giaManAppendCi m'
  let input' = giaLitNotCond input (giaLitIsCompl l)
  obj <- giaManObj m (giaLitVar l)
  assignGiaObjValue obj input'

withGiaCopy :: Gia_Man_t -> (Gia_Man_t -> IO a) -> IO a
withGiaCopy m act = do
  -- Create new GIA.
  objCnt <- giaManObjNum m
  bracket (giaManStart objCnt) giaManStop $ \m' -> do
    -- Initialize all object values to -1.
    giaManFillValue m
    -- Initialize mapping between constants.
    flip setGiaObjValue 0 =<< giaManConst0 m
    -- Perform action.
    act m'

aigMapIsAssigned :: Gia_AigMap_t -> Int -> IO Bool
aigMapIsAssigned m i = (/= nullPtr) <$> peekElemOff m i

-- | Assign lit to value field of GIA object.
assignGiaAigMap:: Gia_AigMap_t -> GiaLit -> Aig_Obj_t -> IO ()
assignGiaAigMap aigMap l o = do
  let v = fromIntegral $ unGiaVar $ giaLitVar l
  whenM (aigMapIsAssigned aigMap v) $
    fail $ "internal: Attempt to copy GIA node multiple times."
  let o' = o `aigNotCond` (giaLitIsCompl l)
  pokeElemOff aigMap v o'

withAigCopyOfGia :: Gia_Man_t -> (Aig_Man_t -> Gia_AigMap_t -> IO a) -> IO a
withAigCopyOfGia gia act = do
  -- Create new GIA.
  objCnt <- giaManObjNum gia
  bracket (aigManStart objCnt) aigManStop $ \aig ->
    withGiaAigMap gia $ \aigMap -> do
      -- Map between constant values.
      pokeElemOff aigMap 0 =<< aigManConst0 aig
      -- Perform computation
      act aig aigMap

-- TODO: use internal ABC simulation code here (this is derived from abcBridge)
run :: Gia_Man_t -> LV.Vector Lit -> LV.Vector Bool -> IO (LV.Vector Bool)
run n outs ins = do
    vecSize <- giaManObjNum n
    inCount <- giaManCiNum n
    let maxOut = fromIntegral . LV.maximum . LV.map unGiaLit $ outs
    () <- assert (LV.length ins < fromIntegral inCount) $ return ()
    () <- assert (maxOut < vecSize) $ return ()
    vec <- VUM.replicate (fromIntegral vecSize) False
    -- initialize inputs
    forM_ (zip [0..] (LV.toList ins)) $ \(i, b) -> do
        cid <- giaObjId n =<< giaManCi n i
        VUM.write vec (unpackVar cid) b
    -- run the network
    giaManForEachObj1_ n $ \po i -> do
        isAnd <- giaObjIsAndOrConst0 po
        isCo <- giaObjIsCo po
        let simulateAnd = do
                i0 <- giaObjFaninId0 po i
                i1 <- giaObjFaninId1 po i
                c0 <- giaObjFaninC0 po
                c1 <- giaObjFaninC1 po
                b0 <- VUM.read vec (unpackVar i0)
                b1 <- VUM.read vec (unpackVar i1)
                VUM.write vec (unpackVar i) $ case (c0, c1) of
                    (False, False) -> b0 && b1
                    (False, True)  -> b0 && Prelude.not b1
                    (True, False)  -> Prelude.not b0 && b1
                    (True, True)   -> Prelude.not (b0 || b1)
            simulateCo = do
                i0 <- giaObjFaninId0 po i
                b0 <- VUM.read vec (unpackVar i0)
                c0 <- giaObjFaninC0 po
                VUM.write vec (unpackVar i) $ case c0 of
                    False -> b0
                    True  -> Prelude.not b0
        when isAnd simulateAnd
        when isCo simulateCo
    -- return the outputs
    r <- LV.forM outs (VUM.read vec . fromIntegral . unGiaLit)
    return r
    where unpackVar = fromIntegral . unGiaVar

{-
getSatResult :: Gia_Man_t -> Int -> IO SatResult
getSatResult _ (-1) = return Unknown
getSatResult _   0  = return UnSat
getSatResult m'  1  = do
  cex <- do
    pCex <- giaManCexComb m'
    when (pCex == nullPtr) $
      error "verifyMiter: Generated counter-example was invalid"
    peekAbcCex pCex
  case pData'inputs'Abc_Cex cex of
    [] -> error "verifyMiter: Generated counter-example had no inputs"
    [bs] -> return (Sat (LV.fromList bs))
    r -> error $ "Invalid counter-example from cecManVerify: " ++ show r
getSatResult _ r = error $
                   "Invalid result value from cecManVerify: " ++
                   show r
-}

-- | An interface to a GIA network.
newtype Network = Network { giaMan :: Gia_Man_t }

-- | Create a new empty GIA network
createEmptyNetwork :: IO Network
createEmptyNetwork = do
  m <- giaManStart 5000
  Network m <$ giaManHashAlloc m

-- | Attempt to read GIA from AIGER file.
readAiger :: FilePath -> IO (Maybe Network)
readAiger path = resolve <$> giaAigerRead path False False
  where resolve m | m == nullPtr = Nothing
                  | otherwise    = Just (Network m)

-- | Free the GIA network.
freeNetwork :: Network -> IO ()
freeNetwork = giaManStop . giaMan

-- | Create a new network input and append it to the list of inputs.
appendNetworkInput :: Network -> IO Lit
appendNetworkInput = giaManAppendCi . giaMan

-- | Returns list of inputs in GIA network.
networkInputCount :: Network -> IO Int
networkInputCount (Network m) = do
  cis <- giaManCis m
  fromIntegral <$> vecIntSize cis

readVec_Int :: Vec_Int_t -> IO (LV.Vector Lit)
readVec_Int v = do
  cnt <- vecIntSize v
  arr <- vecIntArray v
  let peekCiLit i = giaVarLit . GiaVar <$> peekElemOff arr i
  LV.generateM (fromIntegral cnt) peekCiLit


-- | Returns list of inputs in GIA network.
networkInputs :: Network -> IO (LV.Vector Lit)
networkInputs (Network m) = readVec_Int =<< giaManCis m

networkOutputs :: Network -> IO (LV.Vector Lit)
networkOutputs (Network m) = readVec_Int =<< giaManCos m

data View l
  = AndLit l l
  | NotLit l
  | InputLit Int
  | FalseLit

litEvaluator :: Network -> (View a -> IO a) -> IO (Lit -> IO a)
litEvaluator (Network ntk) viewFn = do
  let memo r o t = do
        m <- readIORef r
        writeIORef r $! Map.insert o t m
        return t
  r <- newIORef Map.empty
  let objTerm o = do
        --putStrLn $ "objTerm " ++ show o
        m0 <- readIORef r
        case Map.lookup o m0 of
          Just t -> return t
          _ | giaIsComplement o -> do
            --putStrLn "Complement"
            memo r o =<< viewFn . NotLit =<< objTerm (giaRegular o)
          _ | otherwise -> do
            isTerm <- giaObjIsTerm o
            d0 <- giaObjDiff0 o
            case () of
              _ | not isTerm && d0 /= gia_none -> do -- And gate
                    --putStrLn $ "And " ++ show
                    --    (showHex d0 "" , showHex d1 "" , gia_none)
                    x <- objTerm =<< giaObjChild0 o
                    y <- objTerm =<< giaObjChild1 o
                    memo r o =<< viewFn (AndLit x y)
                | isTerm && d0 /= gia_none -> do -- Primary output
                    --putStrLn "Output"
                    -- This is a primary output, so we just get the lit
                    -- for the gate that it is attached to.
                    objTerm =<< giaObjChild0 o
                | not isTerm -> do -- Constant false
                    memo r o =<< viewFn FalseLit
                | otherwise -> do -- Primary input
                    memo r o =<< viewFn . InputLit . fromIntegral
                           =<< giaObjDiff1 o
  return $ objTerm <=< giaObjFromLit ntk

-- | Writes CNF to file, and returns vector that maps each GiaVar to the variable number
-- in the CNF file, or Nothing if GIA variable was not mapped.
writeCnf :: Network -> FilePath -> Quantifiers Lit -> Lit -> IO (V.Vector (Maybe Int))
writeCnf (Network gia) path qs l = do
  withAigCopyOfGia gia $ \aig aigMap -> do
    -- Map each element of inputs to new input.
    let sliceGiaLitIntoAig l' =
          assignGiaAigMap aigMap l' =<< aigObjCreateCi aig
    Fold.mapM_ sliceGiaLitIntoAig qs
    -- Create output for l
    void $ aigObjCreateCo aig =<< aigDupGiaLit aig aigMap gia l
    -- Create CNF manager
    bracketForeignPtr cnfManStart $ \cnfMan -> do
      bracket (cnfDeriveWithMan cnfMan aig 0) cnfDataFree $ \pCnf -> do
        objCount <- giaManObjNum gia
        pVars <- cnfVarNums pCnf :: IO (Ptr CInt)
        let varIdx :: CInt -> IO (Maybe CInt)
            varIdx v = do
              unless (0 <= v && v < objCount) $
                fail "Illegal var given to writeCnf"
              obj <- peekElemOff aigMap (fromIntegral v)
              if obj == nullPtr then
                return Nothing
              else do
                objId <- aigObjId (aigRegular obj)      
                Just <$> peekElemOff pVars (fromIntegral objId)
        -- Get mapping from GIA vars to CNF Vars
        varMap <- V.generateM (fromIntegral objCount) (varIdx . fromIntegral)
        let resolveLit :: GiaLit -> Maybe CInt
            resolveLit l' = varMap V.! fromIntegral (unGiaVar (giaLitVar l'))
        case T.mapM resolveLit qs of
          Nothing -> fail "Could not translate all inputs"
          Just qs' -> do
             -- Write CNF data to file.
            cnfDataWriteIntoFileWithHeader pCnf path (ppQDIMACSQuantifiers qs') 1
        return $ (fmap . fmap) fromIntegral varMap

bitEngineForNetwork :: Network -> BitEngine Lit
bitEngineForNetwork ntk@(Network m) =
    BitEngine
        { beTrue = giaManConst1Lit
        , beFalse = giaManConst0Lit
        , beNeg = giaLitNot
        , beAnd = giaManHashAnd m
        , beXor = giaManHashXor m
        , beMux = giaManHashMux m
        , beEqLit = (==)
        , beInputLitCount = fromIntegral <$> giaManCiNum m
        , beInputLits = networkInputs ntk
        , beMakeInputLit = appendNetworkInput ntk
        , beCheckSat = Just $ \lit -> do
            withGiaCopy m $ \m' -> do
              --TODO: Fix this
              lit' <- giaDupLit m' m lit
              _ <- giaManAppendCo m' lit'
              giaManStop =<< cecManSatSolving m' cecManSatDefaultParams
              return Unknown          
        , beEvalAigV = flip (run m)
        , beWriteAigerV = \path inputs outputs -> do
            withGiaCopy m $ \m' -> do
              -- Map each element of inputs to new input.
              LV.mapM_ (sliceGiaLit m' m) inputs
              -- Map each primary output.
              let appendCo = giaManAppendCo m' <=< giaDupLit m' m
              LV.mapM_ appendCo outputs
              -- Write GIA to file.
              giaAigerWrite m' path False False
        , beWriteCNF = writeCnf ntk
        , beFree = freeNetwork ntk
        }

-- | Create Bit engine for ABC.
createBitEngine :: IO (BitEngine Lit)
createBitEngine = do
  initialize
  bitEngineForNetwork <$> createEmptyNetwork
