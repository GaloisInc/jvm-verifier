{- |
Module           : $Header$
Description      : The (default) symbolic backend.  Defines operations over a term DAG.
Stability        : provisional
Point-of-contact : jhendrix, jstanley
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Symbolic (
   -- * Lit operations
    module Symbolic.Lit
  -- * Lit vector operations
  , BitWidth (..)
  , maxBitIdx
  , numBits
  , LitVector
  , LitResult(..)
  , fromLsbf
  , fromMsbf
  , toLsbf_lit
  , toLsbfV
  , toMsbf
  , litToLitVector
  -- * DagType
  -- ** Width expressions
  , WidthVar
  , WidthExpr
  , constantWidth
  , varWidth
  , addWidth
  , widthConstant
  , widthVar
  -- ** DagType expressions
  , ShapeVar
  , SymTypeVar(..)
  , SymRecDef, recDefFieldTypes, recDefCtor, recDefFieldOps
  , DagType(..)
  , ppType
  , defaultValue
  , TypeSubst(..)
  , emptySubst
  , idSubst
  , matchSubst
  , applyTypeSubst
  , recDefParams
  , recFieldTypes
  -- * Constant values
  , CValue(..)
  , fromCArray
  , fromCBool
  , concreteValueLit
  , ConstantInjection(..)
  , ConstantProjection(..)
  , mkCIntFromLsbfV
  -- * Lit operations
  , litImplies
  -- * OpMonad and OpSession
  , OpSession
  , runOpSession
  , definedOp
  , uninterpretedOp
  , getStructuralRecord
  , listStructuralRecords
  -- * Symbolic level operations.
  -- ** Ops
  , OpIndex
  , OpPrec
  , OpDef(..)
  , defaultPrec
  , Op, opDef, opSubst , mkOp
  , groundOp
  , opName
  , opPrec
  , opArgTypes
  , opResultType
  , opCanEval
  , opIsBottomUpInferable
  -- ** Applications
  , App
  , DagApp(..)
  , appOp
  , appArgCount
  , appArg
  , appArgs
  , appArgsList
  -- ** Nodes
  , TypedTerm(..)
  , TermClass(..)
  , SymbolicTerm
  , Node(ConstTerm)
  , getApp
  , termApp
  , termInputLitResult
  , nodeCounts
  -- ** Rewriter terms
  , module Symbolic.Rewriter
  -- ** Monadic interface
  , SymbolicMonad
  , rawApplyOpDef
  , SymbolicEvalMonad
  , symbolicEval
  , evalNode
  , SymbolicExc(..)
  , MonadTerm
  , ApplyOpMonad(..)
  , TermDagMonad(..)
  , BitBlastMonad
  , AigOps(..)
  , AigMonad
  , liftAigMonad
  , liftOpSession
  , liftIO
  , getVarLitLsbf
  , runSymbolic
  , makeSession
  , createSafeAig
  , evalAig
  , writeAiger
  , evalAndBlast
  , SupportsSession(..)
  -- ** Misc
  , boolFromConst
  , boolSeqToHex
  , boolSeqToInt32
  , boolSeqToInt64
  , byteSeqToHex
  , constBool
  , constInt
  , constLong
  , evalAigArgs8
  , evalAigArgs32
  , evalAigArgs64
  , hexToByteSeq
  , hexToIntSeq
  , hexToLongSeq
  , int32Type
  , int64Type
  , intFromConst
  , intSeqToBoolSeq
  , intSeqToHex
  , intToBoolSeq
  , longFromConst
  , outsToInts8
  , outsToInts32
  , outsToInts64
  , PrettyNotation(..)
  , ppCValue
  , ppCValueD
  -- ** SExpression style Pretty printing
  , PPConfig(..), defaultPPConfig, PrettyTerm(..)
  -- * Predefined operations.
  -- ** Common operations.
  , eqOpDef
  , eq
  , iteOpDef
  , truncOpDef
  , signedExtOpDef
  -- ** Boolean operations
  , bNotOpDef
  , bAndOpDef
  , bOrOpDef
  , bXorOpDef
  , bImpliesOpDef
  , bImpliesOp
  , (&&&)
  , (|||)
  -- ** Integer bitwise operations
  , iNotOpDef
  , iAndOpDef
  , iOrOpDef
  , iXorOpDef
  -- ** Integer shift operations
  , shlOpDef
  , shrOpDef
  , ushrOpDef
  , appendIntOpDef
  , ushr
  , (#)
  -- ** Integer arithmetic operations
  , addOpDef
  , mulOpDef
  , negOpDef
  , subOpDef
  , signedDivOpDef
  , signedRemOpDef
  -- ** Arithmetic comparison operations
  , signedLeqOpDef
  , signedLtOpDef
  , unsignedLeqOpDef
  , unsignedLtOpDef
  -- ** Array operations.
  , getArrayValueOpDef
  , setArrayValueOpDef
  , splitOpDef
  , joinOpDef
  , mkArrayOpDef
  , mkArrayOp
  , aget, aset
  , WordMonad(..)
  ) where

import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad
import Control.Monad.Error
import Data.Bits
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as LV
import qualified Data.Vector.Storable.Mutable as LV (new,write)

import Symbolic.Common
import Symbolic.Dag
import Symbolic.Lit
import Symbolic.Rewriter

import IO.Session
import Utils.CatchMIO
import Utils.IOStateT
import Utils.LogMonad

-- | List-based AIG evaluation function.
evalAig :: MonadIO m => [Bool] -> [Lit] -> AigComputation m [Bool]
evalAig bl ll = fmap LV.toList $ evalAigV (LV.fromList bl) (LV.fromList ll)

-- | List-based write Aiger function.
writeAiger :: MonadIO m
           => FilePath -- ^ Filename
           -> [Lit] -- ^ Output lits
           -> AigComputation m ()
writeAiger path = writeAigerV path . LV.fromList

-- SymbolicMonad {{{1

type instance MonadTerm SymbolicMonad  = Node

newtype SymbolicMonad a = SM (TermDag a)
  deriving ( Functor
           , Monad
           , MonadIO
           , CatchMIO
           , ApplyOpMonad
           , BitBlastMonad
           , LogMonad
           , SessionMonad
           , TermDagMonad
           , Typeable
           )

instance SupportsSession SymbolicMonad where
  runSymSession (SM m) = runTermDag m

type App = DagApp
type AigMonad = AigComputation OpSession
type LitVector = LitResult Lit
type SymbolicTerm = Node

litToLitVector :: Lit -> LitVector
litToLitVector = litToLitResult

instance AigOps SymbolicMonad where
  evalAigIntegral f ins out = do
    outIntLit <- toLsbf_lit <$> getVarLit out
    bbits <- liftAigMonad $ evalAig (concatMap (f . intToBoolSeq) ins) outIntLit
    if length bbits <= 32
      then termInt $ boolSeqToInt32 bbits
      else
        if length bbits <= 64
          then termLong $ boolSeqToInt64 bbits
          else error "internal: evalAigIntegral: no support for arbitrary-width integers"

  evalAigArray (Wx n) ins outs = do
    rs <- liftAigMonad . evalAig (toBits ins) =<< concat <$> mapM (fmap toLsbf_lit . getVarLit) outs
    toTermsM (length rs `div` n) rs
    where
      (toBits, toTermsM) =
        case n of
          8  -> (evalAigArgs8 . map intFromConst,  \n' -> mapM termInt .  outsToInts8 n')
          32 -> (evalAigArgs32 . map intFromConst,  \n' -> mapM termInt . outsToInts32 n')
          64 -> (evalAigArgs64 . map longFromConst, \n' -> mapM termLong . outsToInts64 n')
          _  -> error $ "evalAigArray: input array elements have "
                      ++ "unexpected bit width"
  makeLitResult op = opDefDescriptor (opDef op) (opSubst op)
  writeAigToFile fname res = liftAigMonad $ writeAigerV fname res
  unsafeIndexCvt v i   = return (v `LV.unsafeIndex` i)
  getVarLit n@(getAppVector -> Just (op, args)) = do
    mlv <- getAppLitResult n
    case mlv of
      Just lit -> return lit
      Nothing -> do
        lit <- makeLitResult op args
        setAppLitResult n lit
        return lit
  getVarLit n = constLitResult n

-- | Apply an operator definition to the list of symbolic terms while inferring
-- substitution.
-- Returns node if type could be inferred, and Nothing if type substitution could
-- not be found.
rawApplyOpDef :: OpDef
              -> [Node]
              -> SymbolicMonad (Maybe Node)
rawApplyOpDef op args =
  let opdTypes = opDefArgTypes op
      argTypes = map termType args
   in CE.assert (V.length opdTypes == length argTypes) $
        case matchSubst (V.toList opdTypes `zip` argTypes) of
          Nothing -> return Nothing
          Just sub -> fmap Just $ rawOpVector (mkOp op sub) (V.fromList args)

-- | State for symbolic evaluation state.
data SymbolicEvalState = SES {
    unnamedVarValues :: Vector CValue
  , appVarMap :: Map AppIndex CValue
  }

-- | Monad used to evaluate symbolic state.
newtype SymbolicEvalMonad sym a = SEM (StateT SymbolicEvalState sym a)
  deriving (Functor, Monad, MonadIO, MonadTrans, CatchMIO)

-- | Create as a context to evaluate nodes.
symbolicEval :: MonadIO sym
             => Vector CValue -- ^ Evaluation for unnamed variables.
             -> SymbolicEvalMonad sym a
             -> sym a
symbolicEval unnamedVarValues (SEM m) =
  evalStateT m
             SES { unnamedVarValues
                 , appVarMap = Map.empty }

-- | Evaluates symbolic term and returns value associated with it.
evalNode :: ( MonadIO sym
            , TermDagMonad sym
            , WordMonad sym
            , MonadTerm sym ~ Node)
         => MonadTerm sym -> SymbolicEvalMonad sym CValue
evalNode (ConstTerm c _) = return c
evalNode (InputTerm i _ _) = SEM $ fmap (V.! i) $ gets unnamedVarValues
evalNode (AppVar i app) = do
  let opd = opDef (appOp app)
      opsub = opSubst (appOp app)
  avm <- SEM $ gets appVarMap
  case Map.lookup i avm of
    Just c -> return c
    Nothing -> do
      let handleErr =
            case opDefKind opd of
              DefinedOp fn -> evalNode =<< lift (fn opsub (appArgs app))
              UninterpOp ->
                 error $ "internal: TermDag::evalDagV: operator \"" ++ opDefName opd
                         ++ "\" is uninterpreted."
              _ ->
                 error $ "internal: TermDag::evalDagV: operator \""
                         ++ opDefName opd
                         ++ "\" doesn't have a ground evaluation function."
      c <- case app of
             UnaryApp op v1 ->
               case opEval op of
                 UnaryOpEval fn -> liftM (fn (opSubst op)) (evalNode v1)
                 VectorOpEval fn -> do
                   c1 <- evalNode v1
                   return $ fn (opSubst op) (V.singleton c1)
                 _ -> handleErr
             BinaryApp op v1 v2 ->
               case opEval op of
                 BinaryOpEval fn ->
                   liftM2 (fn (opSubst op)) (evalNode v1) (evalNode v2)
                 VectorOpEval fn -> do
                   c1 <- evalNode v1
                   c2 <- evalNode v2
                   return $ fn (opSubst op) (V.fromListN 2 [c1, c2])
                 _ -> handleErr
             TernaryApp op v1 v2 v3 ->
               case opEval op of
                 TernaryOpEval fn ->
                   liftM3 (fn (opSubst op)) (evalNode v1) (evalNode v2) (evalNode v3)
                 VectorOpEval fn -> do
                   c1 <- evalNode v1
                   c2 <- evalNode v2
                   c3 <- evalNode v3
                   return $ fn (opSubst op) (V.fromListN 3 [c1, c2, c3])
                 _ -> handleErr
             App op args ->
               case opEval op of
                 VectorOpEval fn -> fmap (fn (opSubst op)) (V.mapM evalNode args)
                 _ -> handleErr
      SEM $ modify $ \s -> s { appVarMap = Map.insert i c (appVarMap s) }
      return c

instance Applicative SymbolicMonad where
  pure = return
  af <*> aa = af >>= (<$> aa)

-- | Run symbolic monad in IO.
runSymbolic :: SymbolicMonad a -> IO a
runSymbolic m =
  runOpSession (runSymSession m)
    `CE.catch` \(e :: SymbolicExc) -> do
      case e of
        SymExtErr msg _ ->
          CE.throw e { symExtErrMsg =
                         "The symbolic backend encountered a fatal error:\n" ++ msg
                       -- TODO: contact info blurb?
                     }
        _ -> CE.throw e

-- | Returns an aiger that holds if all conditions hold.
createSafeAig :: String -> Node -> SymbolicMonad ()
createSafeAig path assumptions = do
  LV v <- getVarLit assumptions
  if LV.length v == 1
    then liftAigMonad $ writeAigerV path $ LV.singleton $ neg (v LV.! 0)
    else error "internal: Unexpected number of bits in assumptions"

-- | Evaluates each node in dag on inputs and compares against bitblasted
-- version.  This is useful for tracking down bugs in AIG generation.
evalAndBlast :: Vector CValue -> LV.Vector Bool -> SymbolicMonad ()
evalAndBlast cvalues boolInputs = do
  nodes <- fmap sort getAppNodes
  let compareNode n (CInt (Wx w) x) (LV lits) = do
        let expected = LV.map (testBit x) $ LV.enumFromN 0 w
        found <- evalAigV boolInputs lits
        if expected == found
          then return ()
          else error $ "Error in blasting term:\n"
                     ++ show n
                     ++ "Expected: " ++ show expected ++ "\n"
                     ++ "Found:    " ++ show found
      compareNode n (CArray v) (LVN lits) = do
        CE.assert (V.length v == V.length lits) $ do
          let w = V.length v
          V.forM_ (V.enumFromN 0 w) $ \i -> do
            compareNode n (v V.! i) (lits V.! i)
      compareNode n (CBool b) (LV lits) = do
        found <- evalAigV boolInputs lits
        CE.assert (LV.length found == 1) $
          if b == found LV.! 0
            then return ()
            else error $ "Error in blasting term:\n" ++ show n ++ "\n Expected: " ++ show b
      compareNode _ _ _ = error "Incompatible node types"
  cns <- symbolicEval cvalues $ mapM evalNode nodes
  forM_ (nodes `zip` cns) $ \(n,c) -> do
     l <- getVarLit n
     liftAigMonad $ compareNode n c l

-- Lit utility functions {{{1

litAndM :: AigMonad Lit -> AigMonad Lit -> AigMonad Lit
litAndM mx my = do
  lx <- mx
  lFalse <- litFalse
  if lx == lFalse
    then return lFalse
    else do
      lTrue <- litTrue
      if lx == lTrue
        then my
        else litAnd lx =<< my

negM :: AigMonad Lit -> AigMonad Lit
negM = (return . neg =<<)

litOr :: Lit -> Lit -> AigMonad Lit
litOr x y = negM $ litAnd (neg x) (neg  y)

litOrM :: AigMonad Lit -> AigMonad Lit -> AigMonad Lit
litOrM x y = negM $ litAndM (negM x) (negM y)

litEq :: Lit -> Lit -> AigMonad Lit
litEq x y = litAndM (litImplies x y) (litImplies y x)

litImplies :: Lit -> Lit -> AigMonad Lit
litImplies x y = negM $ litAnd x (neg y)

-- | @litIte c t f@ returns @c ? t : f@
litIte :: Lit -> Lit -> Lit -> AigMonad Lit
litIte c t f = do
  lTrue <- litTrue
  if c == lTrue
    then return t
    else do
      lFalse <- litFalse
      if c == lFalse
        then return f
        else if t == f
          then return t
          else litAndM (litOr (neg c) t) (litOr c f)

litXor :: Lit -> Lit -> AigMonad Lit
litXor x y = litOrM (litAnd x (neg y)) (litAnd y (neg x))

-- LitVector Circuit generators {{{1

-- | Literals in an int with least-significant bit first
toLsbf_lit :: LitVector -> [Lit]
toLsbf_lit = LV.toList . toLsbfV

-- | Literals in an int with most-significant bit first
toMsbf :: LitVector -> [Lit]
toMsbf = LV.toList . toMsbfV

-- | LitVector with literals ordered with least-significant bit first
fromLsbf :: BitWidth -> [Lit] -> LitVector
fromLsbf (Wx n) lits = fromLsbfV (LV.fromListN n lits)

-- | LitVector with literals ordered with most-significant bit first
fromMsbf :: BitWidth -> [Lit] -> LitVector
fromMsbf (Wx n) lits = fromMsbfV (LV.fromListN n lits)

-- | @litIteVectorM c mTrue lTrue@ returns the lit vector equal to mTrue if c
-- is true and false otherwise.
litIteVectorM :: AigOps sym
              => Lit
              -> sym (LitResult Lit)
              -> sym (LitResult Lit)
              -> sym (LitResult Lit)
litIteVectorM c mTrue mFalse = do
  lTrue <- liftAigMonad litTrue
  if c == lTrue
    then mTrue
    else do
      lFalse <- liftAigMonad litFalse
      if c == lFalse
        then mFalse
        else do
          t <- mTrue
          f <- mFalse
          liftAigMonad $ mergeFn t f
 where mergeFn (LV vx) (LV vy) =
         CE.assert (LV.length vx == LV.length vy) $
           fmap LV $ LV.zipWithM (litIte c) vx vy
       mergeFn (LVN vx) (LVN vy) =
         CE.assert (V.length vx == V.length vy) $
           fmap LVN $ V.zipWithM mergeFn vx vy
       mergeFn _ _ = error "internal: illegal arguments to litIteVectorM"

-- | Defines addition in the Aiger monad.
litAdder :: LV.Vector Lit -> LV.Vector Lit -> AigMonad (LV.Vector Lit)
litAdder vx vy =
  CE.assert (LV.length vx == LV.length vy) $ do
    let n = LV.length vx
    m <- liftIO $ LV.new n
    let adderStepM c i
          | i == n = return ()
          | otherwise = do
            let a = vx `LV.unsafeIndex` i
            let b = vy `LV.unsafeIndex` i
            ab <- litAnd a b
            abN <- litAnd (neg a) (neg b)
            cp <- litAnd c (neg abN)
            c' <- litOr ab cp
            abEq <- litOr ab abN
            r  <- litEq c abEq
            liftIO $ LV.write m i r
            adderStepM c' (i+1)
    lFalse <- litFalse
    adderStepM lFalse 0
    liftIO $ LV.freeze m

-- | @litLtImpl ifEq lx ly ifEq@ returns true if lx < ly or lx == ly & ifEq is
-- True.
-- @lx@ and @ly@ are expected to be in least-significant bit first order.
litLtImplV :: Bool -> LV.Vector Lit -> LV.Vector Lit -> AigMonad Lit
litLtImplV ifEq lx ly = do
  let cmpStep r i = do
        let x = lx `LV.unsafeIndex` i
            y = ly `LV.unsafeIndex` i
        litAndM (litOr (neg x) y)
                (litOrM (litAnd (neg x) y) (return r))
  initial <- litFromBool ifEq
  LV.foldM cmpStep initial $ LV.enumFromN 0 (LV.length lx)

litIntIte :: Lit
          -> AigMonad (LV.Vector Lit)
          -> AigMonad (LV.Vector Lit)
          -> AigMonad (LV.Vector Lit)
litIntIte c mTrue mFalse = do
  lTrue <- litTrue
  if c == lTrue
    then mTrue
    else do
      lFalse <- litFalse
      if c == lFalse
        then mFalse
        else do
          t <- mTrue
          f <- mFalse
          LV.zipWithM (litIte c) t f

-- | Returns the product of two lit vectors.
litMultiplier :: LV.Vector Lit -> LV.Vector Lit -> AigMonad (LV.Vector Lit)
litMultiplier vx vy = do
  let n = LV.length vx
  lFalse <- litFalse
  let mulStep r i =
        litIntIte (vy LV.! i)
                  (litAdder (litShiftLeft vx lFalse i) r)
                  (return r)
  LV.foldM mulStep
           (LV.replicate n lFalse)
           (LV.enumFromN 0 n)

-- | Returns negation of literal as number.
litNeg :: LV.Vector Lit -> AigMonad (LV.Vector Lit)
litNeg vx = do
  let n = LV.length vx
  m <- liftIO $ LV.new n
  let negStepM c i
        | i == n = return ()
        | otherwise = do
          let x = vx `LV.unsafeIndex` i
          cOrX  <- litOr c x
          cAndX <- litAnd c x
          r     <- litAnd cOrX (neg cAndX)
          liftIO $ LV.write m i r
          negStepM cOrX (i+1)
  lFalse <- litFalse
  negStepM lFalse 0
  liftIO $ LV.freeze m

-- | @litShiftLeft lits fill shift@ shifts @lits@ to the left by @shift@ bits.
-- New bits are given the value @fill@.
litShiftLeft :: LV.Vector Lit -> Lit -> Int -> LV.Vector Lit
litShiftLeft x fill shft =
  let n = LV.length x
   in if shft >= n
        then LV.replicate n fill
        else LV.replicate shft fill LV.++ LV.take (n - shft) x

-- | @litShiftRight lits fill shift@ shifts "lits" to the right by "shift" bits.
-- New bits are given the value "fill".
litShiftRight :: LV.Vector Lit -> Lit -> Int -> LV.Vector Lit
litShiftRight x fill shft =
  let n = LV.length x
   in if shft >= n
        then LV.replicate n fill
        else LV.drop shft x LV.++ LV.replicate shft fill

litSub :: LV.Vector Lit -> LV.Vector Lit -> AigMonad (LV.Vector Lit)
litSub lx ly = litAdder lx =<< litNeg ly

-- Op and Node {{{1

-- | Does the operator have a ground evaluation function?
opCanEval :: Op -> Bool
opCanEval op =
  case opDefEval (opDef op) of
    NoOpEval -> False
    _ -> True

getVarLitLsbf :: AigOps sym => MonadTerm sym -> sym [Lit]
getVarLitLsbf = fmap toLsbf_lit . getVarLit

-- Predefined Symbolic operators {{{1
-- Precedences {{{2
{- Operator precedence:
   Lvl | Prec name  | Operators
   --------------------------------------------------------------------
    85 | arrayPrec  | getArrayValueOp, setArrayValueOp
    80 | unaryPrec  | truncOp, iNotOp, bNotOp, negOp
    75 | mulPrec    | mulOp
    70 | addPrec    | addOp, subOp
    65 | shiftPrec  | shlOp, shrOp, ushrOp
    60 | appendPrec | appendIntOp
    55 | compPrec   | signedLeqOp signedLtOp unsignedLeqOp unsignedLtOp
    50 | eqPrec     | eqOp
    45 | iAndPrec   | iAndOp
    40 | iXorPrec   | iXorOp
    35 | iOrPrec    | iOrOp
    30 | bAndPrec   | bAndOp
    25 | bXorPrec   | bXorOp
    20 | bOrPrec    | bOrOp
    15 | itePrec    | iteOp
-}

arrayPrec :: OpPrec
arrayPrec = 85

unaryPrec :: OpPrec
unaryPrec = 80

mulPrec :: OpPrec
mulPrec = 75

addPrec :: OpPrec
addPrec = 70

shiftPrec :: OpPrec
shiftPrec = 65

appendPrec :: OpPrec
appendPrec = 60

compPrec :: OpPrec
compPrec = 55

eqPrec :: OpPrec
eqPrec = 50

iAndPrec :: OpPrec
iAndPrec = 45

iXorPrec :: OpPrec
iXorPrec = 40

iOrPrec :: OpPrec
iOrPrec = 35

_bAndPrec :: OpPrec
_bAndPrec = 30

_bXorPrec :: OpPrec
_bXorPrec = 25

_bOrPrec :: OpPrec
_bOrPrec = 20

itePrec :: OpPrec
itePrec = 15

-- Common operations {{{2

-- | Compares two lit vectors and returns lit if they are bitwise equal.
litIntEq :: LV.Vector Lit -> LV.Vector Lit -> AigMonad Lit
litIntEq vx vy =
  CE.assert (LV.length vx == LV.length vy) $ do
    lTrue <- litTrue
    lits <- LV.mapM (\i -> litEq (vx LV.! i) (vy LV.! i)) $
            LV.enumFromN 0 (LV.length vx)
    LV.foldM litAnd lTrue lits

-- | Create a binary operator with given shape.
shapeOp :: OpDef -> DagType -> Op
shapeOp d tp =
  mkOp d TypeSubst { shapeSubst = Map.fromList [("x", tp)]
                   , widthSubst = Map.empty
                   }

-- | Create a binary operator with given bitwidth.
mkWidthSubst :: BitWidth -> TypeSubst
mkWidthSubst opWidth =
  TypeSubst { shapeSubst = Map.empty
            , widthSubst = Map.fromList [("x", constantWidth opWidth)]
            }

-- | Create a binary operator with given bitwidth.
widthOp :: OpDef -> BitWidth -> Op
widthOp opDef opWidth = mkOp opDef (mkWidthSubst opWidth)

-- | Returns true if two compatible-length results are equal.
eqOpDef :: OpDef
eqOpDef =
  OpDef { opDefIndex = 0
        , opDefName = "=="
        , opDefPrec = eqPrec
        , opDefArgTypes = V.replicate 2 (SymShapeVar "x")
        , opDefResultType = SymBool
        , opDefKind = PrimOp
        , opDefEval =
            let evalFn _ (CBool x) (CBool y) = CBool (x == y)
                evalFn _ (CInt xw x) (CInt yw y)
                  | xw == yw = CBool (x == y)
                evalFn _ (CArray a1) (CArray a2)
                  | V.length a1 == V.length a2 = CBool (a1 == a2)
                evalFn _ x y =
                  error $ "internal: Symbolic.eqOp: evaluating strange arguments "
                          ++ show [x,y]
             in BinaryOpEval evalFn
        , opDefDescriptor = \_ args ->
           CE.assert (V.length args == 2) $ do
             let x = args V.! 0
             let y = args V.! 1
             lx <- getVarLit x
             ly <- getVarLit y
             let eqFn (LV vx) (LV vy) = litIntEq vx vy
                 eqFn (LVN vx) (LVN vy)
                   | V.length vx == V.length vy = do
                     lTrue <- litTrue
                     lits <- V.mapM (\i -> eqFn (vx V.! i) (vy V.! i))
                           $ V.enumFromN 0 (V.length vx)
                     V.foldM litAnd lTrue lits
                 eqFn _ _ = error $ "internal: eqOp passed mismatched values "
                                    ++ prettyTerm x ++ " and " ++ prettyTerm y
             liftAigMonad $ fmap litToLitVector $ eqFn lx ly
        }

-- | @iteOp@ is a ternary argument taking the condition, true case, and false case.
iteOpDef :: OpDef
iteOpDef =
  OpDef { opDefIndex = 1
        , opDefName = "ite"
        , opDefPrec = itePrec
        , opDefArgTypes = V.fromListN 3 [SymBool, SymShapeVar "x", SymShapeVar "x"]
        , opDefResultType = SymShapeVar "x"
        , opDefKind = PrimOp
        , opDefEval =
            let evalFn _ (CBool b) t f = if b then t else f
                evalFn _ _ _ _ = error "internal: ground ite expected concrete bool"
             in TernaryOpEval evalFn
        , opDefDescriptor = \_ args ->
            CE.assert (V.length args == 3) $ do
              LV (LV.toList -> [lCond]) <- getVarLit (args V.! 0)
              litIteVectorM lCond
                (getVarLit (args V.! 1))
                (getVarLit (args V.! 2))
        }

-- | @truncOp inputWidth resultWidth@ returns truncation unary operator
-- that takes input with given width and truncates to new width.
truncOpDef :: BitWidth -> OpSession OpDef
truncOpDef (Wx wr) =
  cachedOpDef (TruncOpId (Wx wr)) $ \opDefIndex ->
    OpDef { opDefIndex
          , opDefName = nm
          , opDefPrec = unaryPrec
          , opDefArgTypes = V.singleton (SymInt (varWidth "x"))
          , opDefResultType = SymInt (constantWidth (Wx wr))
          , opDefKind = PrimOp
          , opDefEval =
              let evalFn _ (getUVal -> Just v) = mkCInt (Wx wr) v
                  evalFn _ _ = error "internal: Illegal arguments to truncOp"
               in UnaryOpEval evalFn
          , opDefDescriptor = \_ args ->
              CE.assert (V.length args == 1) $ do
                LV lx <- getVarLit (args V.! 0)
                return $ LV $ LV.take wr lx
          }
  where nm = "trunc:" ++ show wr

-- | @signedExtOp inputWidth resultWidth@ returns unary operator that
-- returns sign extended bitvector of input with result.
signedExtOpDef :: BitWidth -> OpSession OpDef
signedExtOpDef (Wx wr) = do
  cachedOpDef (SignedExtOpId (Wx wr)) $ \opDefIndex ->
    OpDef { opDefIndex
          , opDefName = nm
          , opDefPrec = unaryPrec
          , opDefArgTypes = V.singleton (SymInt (varWidth "x"))
          , opDefResultType = SymInt (constantWidth (Wx wr))
          , opDefKind = PrimOp
          , opDefEval =
              let evalFn _ (getSVal -> Just v) = mkCInt (Wx wr) v
                  evalFn _ _ = error "Illegal arguments to signedExtOp"
               in UnaryOpEval evalFn
          , opDefDescriptor = \tm ->
              let Just wx = Map.lookup "x" (widthSubst tm)
                  Just (Wx wi) = widthConstant wx
               in \args -> do
                    LV vx <- getVarLit (args V.! 0)
                    let msb = LV.last vx
                    return $ LV $ vx LV.++ LV.replicate (wr - wi) msb
          }
  where nm = "zext:" ++ show wr

-- | Return term indicating if two terms are equal.
eq :: Term -> Term -> Term
eq x y =
  let tp = termType x
      s = TypeSubst { shapeSubst = Map.fromList [("x", tp)]
                    , widthSubst = Map.empty }
   in appTerm (mkOp eqOpDef s) [x,y]

-- Boolean operations {{{2

-- | Negate boolean argument.
bNotOpDef :: OpDef
bNotOpDef =
  OpDef { opDefIndex = 4
        , opDefName = "not"
        , opDefPrec = unaryPrec
        , opDefArgTypes = V.singleton SymBool
        , opDefResultType = SymBool
        , opDefKind = PrimOp
        , opDefEval =
            let evalFn _ (CBool b) = CBool (not b)
                evalFn _ _ = error "Illegal arguments to bNotOp"
             in UnaryOpEval evalFn
        , opDefDescriptor = \_ args ->
           CE.assert (V.length args == 1) $ do
             LV vx <- getVarLit (args V.! 0)
             return $ LV (LV.map neg vx)
        }

-- | Create a binary operator with strict bit-blasting.
binaryBoolOpDef :: OpIndex
                -> String
                -> (Bool -> Bool -> Bool)
                -> (Lit -> Lit -> AigMonad Lit)
                -> OpDef
binaryBoolOpDef opDefIndex opDefName evalFn litFn =
  OpDef { opDefIndex
        , opDefName
        , opDefPrec = defaultPrec
        , opDefArgTypes = V.replicate 2 SymBool
        , opDefResultType = SymBool
        , opDefKind = PrimOp
        , opDefEval =
            let fn _ (CBool x) (CBool y) = CBool (evalFn x y)
                fn _ _ _ = error $ "internal: Illegal arguments to " ++ opDefName
             in BinaryOpEval fn
        , opDefDescriptor = \_ args ->
           CE.assert (V.length args == 2) $ do
             LV vx <- getVarLit (args V.! 0)
             LV vy <- getVarLit (args V.! 1)
             CE.assert (LV.length vx == 1 && LV.length vy == 1) $
               liftAigMonad $ fmap (LV . LV.singleton)
                            $ litFn (vx LV.! 0) (vy LV.! 0)
        }

-- | And operator applied to Boolean values.
bAndOpDef :: OpDef
bAndOpDef = binaryBoolOpDef 5 "&&" (&&) litAnd

-- | Take conjunction of two rewriter terms.
(&&&) :: Term -> Term -> Term
(&&&) x y = appTerm (groundOp bAndOpDef) [x,y]

-- | Or of two boolean arguments.
bOrOpDef :: OpDef
bOrOpDef = binaryBoolOpDef 6 "||" (||) litOr

-- | Take disjunction of two rewriter terms.
(|||) :: Term -> Term -> Term
(|||) x y = appTerm (mkOp bOrOpDef emptySubst) [x,y]

-- | Exclusive or of arguments.
bXorOpDef :: OpDef
bXorOpDef = binaryBoolOpDef 7 "xor" (/=) litXor

-- | Operator definition for implication.
bImpliesOpDef :: OpDef
bImpliesOpDef = binaryBoolOpDef 8 "implies" (\x y -> not x || y) litImplies

-- | Operator for implication.
bImpliesOp :: Op
bImpliesOp = groundOp bImpliesOpDef

-- Integer bitwise operations {{{2

-- | A unary operator definition with a single width type.
unaryIntOpDef :: OpIndex
              -> String
              -> OpPrec
              -> (Integer -> Integer)
              -> (LV.Vector Lit -> AigMonad (LV.Vector Lit))
              -> OpDef
unaryIntOpDef opDefIndex opDefName opDefPrec intFn litFn =
  let argType = SymInt (varWidth "x")
   in OpDef { opDefIndex
            , opDefName
            , opDefPrec
            , opDefArgTypes = V.singleton argType
            , opDefResultType = argType
            , opDefKind = PrimOp
            , opDefEval =
                let evalFn tm (getSVal -> Just x) =
                      let Just w = widthConstant (widthSubst tm Map.! "x")
                       in mkCInt w $ intFn x
                    evalFn _ _ =
                      error $ "internal: illegal arguments to " ++ opDefName
                 in UnaryOpEval evalFn
            , opDefDescriptor = \_ args ->
               CE.assert (V.length args == 1) $ do
                 LV vx <- getVarLit (args V.! 0)
                 liftAigMonad $ fmap LV $ litFn vx
            }

-- | A binary operator definition with a single width type.
binaryIntOpDef :: OpIndex
               -> String
               -> OpPrec
               -> (Integer -> Integer -> Integer)
               -> (LV.Vector Lit -> LV.Vector Lit -> AigMonad (LV.Vector Lit))
               -> OpDef
binaryIntOpDef opDefIndex opDefName opDefPrec intFn litFn =
  let argType = SymInt (varWidth "x")
   in OpDef { opDefIndex
            , opDefName
            , opDefPrec
            , opDefArgTypes = V.replicate 2 argType
            , opDefResultType = argType
            , opDefKind = PrimOp
            , opDefEval =
                let evalFn tm (getSVal -> Just x) (getSVal -> Just y) =
                      let Just width = widthConstant (widthSubst tm Map.! "x")
                       in mkCInt width $ intFn x y
                    evalFn _ _ _ =
                      error $ "internal: illegal arguments to " ++ opDefName
                 in BinaryOpEval evalFn
            , opDefDescriptor = \_ args ->
               CE.assert (V.length args == 2) $ do
                 LV vx <- getVarLit (args V.! 0)
                 LV vy <- getVarLit (args V.! 1)
                 CE.assert (LV.length vx == LV.length vy) $
                   liftAigMonad $ fmap LV $ litFn vx vy
            }

-- | Negate bits in integer argument.
iNotOpDef :: OpDef
iNotOpDef = unaryIntOpDef 10 "~" unaryPrec complement (return . LV.map neg)

-- | Bitwise-and of arguments.
iAndOpDef :: OpDef
iAndOpDef = binaryIntOpDef 11 "&" iAndPrec (.&.) (LV.zipWithM litAnd)

-- | Bitwise-or of arguments.
iOrOpDef :: OpDef
iOrOpDef = binaryIntOpDef 12 "|" iOrPrec (.|.) (LV.zipWithM litOr)

-- | Bitwise exclusive or of arguments.
iXorOpDef :: OpDef
iXorOpDef = binaryIntOpDef 13 "xor" iXorPrec xor (LV.zipWithM litXor)

-- Integer shift operations {{{2

-- | Given a function from a constant byte to lit values and a litVector,
--  @selectIntFromLit@ returns the value of the litvector on possible inputs.
selectIntFromLit :: String
                 -> (Int -> LV.Vector Lit)
                 -> LV.Vector Lit
                 -> AigMonad (LV.Vector Lit)
selectIntFromLit name fn vx =
  let n = LV.length vx
      bitFn :: Int -> Int -> AigMonad (LV.Vector Lit)
      bitFn 0 y = return $ fn y
      bitFn i y = litIntIte (vx LV.! j) (bitFn j (y `setBit` j)) (bitFn j y)
        where j = i - 1
   in -- Check if length is legal size for an index.
      if 2 ^ n <= toInteger (maxBound :: Int)
        then bitFn n 0
        else error $ "internal: " ++ name ++ " shift size of " ++ show n
                        ++ " is too large."

shiftOpDef :: OpIndex
           -> String
           -> (CValue -> Int -> Integer) -- ^ Evaluation function.
           -> (LV.Vector Lit -> LV.Vector Lit -> AigMonad (LV.Vector Lit))
           -> OpDef
shiftOpDef opDefIndex opDefName intFn litFn =
  OpDef { opDefIndex
        , opDefName
        , opDefPrec = shiftPrec
        , opDefArgTypes = V.fromListN 2 [SymInt (varWidth "v"), SymInt (varWidth "s")]
        , opDefResultType = SymInt (varWidth "v")
        , opDefKind = PrimOp
        , opDefEval =
            let evalFn tm x (getUVal -> Just y) =
                  let Just (Wx valueWidth) = widthConstant (widthSubst tm Map.! "v")
                      val = if y < toInteger valueWidth
                              then fromInteger y
                              else valueWidth
                   in mkCInt (Wx valueWidth) $ intFn x val
                evalFn _ _ _ = error $ "Illegal aruments applied to " ++ opDefName
             in BinaryOpEval evalFn
        , opDefDescriptor = \_ args ->
           CE.assert (V.length args == 2) $ do
             LV vx <- getVarLit (args V.! 0)
             LV vy <- getVarLit (args V.! 1)
             liftAigMonad $ fmap LV $ litFn vx vy
        }

-- | @shlOpDef x y@ shifts @x@ to the left by @y@-bits where @y@ is treated as
-- an unsigned integer.
shlOpDef :: OpDef
shlOpDef = shiftOpDef 14 "<<" evalFn litFn
  where evalFn (getSVal -> Just x) y = x `shiftL` y
        evalFn _ _ = error "internal: shlOp eval"
        litFn vx vy = do
          lFalse <- litFalse
          selectIntFromLit "shlOp" (litShiftLeft vx lFalse) vy


-- | @shrOpDef x y@ shifts @x@ to the right by @y@-bits where @x@ is treated as
-- a signed integer and @y@ is treated as an unsigned integer.
shrOpDef :: OpDef
shrOpDef = shiftOpDef 15 ">>" evalFn litFn
  where evalFn (getSVal -> Just x) y = x `shiftR` y
        evalFn _ _ = error "internal: shrOp eval"
        litFn vx vy =
          selectIntFromLit "shrOp" (litShiftRight vx (LV.last vx)) vy

-- | @ushrOpDef x y@ shifts @x@ to the right by @y@-bits where @x@ and @y@ are
-- treated as unsigned integers.
ushrOpDef :: OpDef
ushrOpDef = shiftOpDef 16 ">>>" evalFn litFn
  where evalFn (getUVal -> Just x) y = x `shiftR` y
        evalFn _ _ = error "internal: ushrOp eval"
        litFn vx vy = do
          lFalse <- litFalse
          selectIntFromLit "ushrOp" (litShiftRight vx lFalse) vy

-- | @appendIntOp@ applied to integers @x@ & @y@ returns
-- @x .|. (y `shiftL` width x)@.
appendIntOpDef :: OpDef
appendIntOpDef =
  let xvw = varWidth "x"
      yvw = varWidth "y"
   in OpDef { opDefIndex = 17
            , opDefName = "#"
            , opDefPrec = appendPrec
            , opDefArgTypes = V.fromListN 2 [SymInt xvw, SymInt yvw]
            , opDefResultType = SymInt (addWidth xvw yvw)
            , opDefKind = PrimOp
            , opDefEval =
                let evalFn tm (getUVal -> Just xv) (getUVal -> Just yv) =
                      let Just (Wx xw) = widthConstant (widthSubst tm Map.! "x")
                          Just (Wx yw) = widthConstant (widthSubst tm Map.! "y")
                       in mkCInt (Wx $ xw + yw) $ xv .|. (yv `shiftL` xw)
                    evalFn _ _ _ = error $ "internal: illegal arguments to appendIntOp"
                 in BinaryOpEval evalFn
            , opDefDescriptor = \_ args ->
               CE.assert (V.length args == 2) $ do
                 LV vx <- getVarLit (args V.! 0)
                 LV vy <- getVarLit (args V.! 1)
                 return $ LV (vx LV.++ vy)
            }

ushr :: Term -> Term -> Term
ushr v i =
  let SymInt wv = termType v
      SymInt ws = termType i
      s = TypeSubst { shapeSubst = Map.empty
                    , widthSubst = Map.fromList [("v", wv), ("s", ws)] }
   in appTerm (mkOp ushrOpDef s) [v, i]

(#) :: Term -> Term -> Term
(#) x y =
  case (termType x, termType y) of
    (SymInt wx, SymInt wy) ->
      let s = TypeSubst { shapeSubst = Map.empty
                        , widthSubst = Map.fromList [("x", wx), ("y", wy)] }
       in appTerm (mkOp appendIntOpDef s) [x, y]
    _ -> error "internal: (#) applied to arguments with incorrect types"

-- Integer arithmetic operations {{{2

-- | Add arguments.
addOpDef :: OpDef
addOpDef = binaryIntOpDef 20 "+" addPrec (+) litAdder

-- | Multiply two @n@-bit arguments to obtain a @2n@-bit argument.
mulOpDef :: OpDef
mulOpDef = binaryIntOpDef 21 "*" mulPrec (*) litMultiplier

-- | Return twos-complement negation of integer argument.
negOpDef :: OpDef
negOpDef = unaryIntOpDef 22 "unary-" unaryPrec negate litNeg

-- | Subtract arguments.
subOpDef :: OpDef
subOpDef = binaryIntOpDef 23 "-" addPrec (-) litSub

-- | (a `litQuotRem` b) computes (a `quotRem` b) in the aiger monad
litQuotRem :: LV.Vector Lit
           -> LV.Vector Lit
           -> AigMonad (LV.Vector Lit, LV.Vector Lit)
litQuotRem dividend' divisor' = do
  CE.assert (LV.length dividend' == LV.length divisor') $ do
    let n = LV.length dividend'
    -- Perform quotRem on the absolute value of the operands.  Then, negate the
    -- quotient if the signs of the operands differ and make the sign of a nonzero
    -- remainder to match that of the dividend.
    lFalse <- litFalse
    lTrue <- litTrue
    dividend <- absLsbf dividend'
    divisor <- absLsbf divisor'
    let divStep :: LV.Vector Lit -- msbf remainder register
                -> Int           -- bit index
                -> AigMonad (LV.Vector Lit)
        divStep rrOrig _i = do
          let q = LV.take n rrOrig
          let r = LV.drop n rrOrig
          -- Subtract the divisor from the left half of the "remainder register"
          s <- litSub r divisor
          litIntIte (signLitLsbf s) -- sign bit of the remainder
                    (return $ litShiftLeft rrOrig lFalse 1) -- rem < 0, orig rr's quot lsl'd w/ 0
                    (return $ litShiftLeft (q LV.++ s) lTrue 1)   -- rem >= 0, new rr's quot lsl'd w/ 1
    -- Given an n-bit dividend and divisor, 'initial' is the starting value of
    -- the 2n-bit "remainder register" that carries both the quotient and remainder;
    let initial = dividend LV.++ LV.replicate n lFalse
    res <- LV.foldM divStep (litShiftLeft initial lFalse 1) (LV.enumFromN 0 n)
    let q = LV.take n res
    let r = LV.drop n res
    q' <- q `negWhenM` litXor dsign (signLitLsbf divisor')
    r' <- litShiftRight r lFalse 1 `negWhenM` (return dsign)
    CE.assert (LV.length r' == n) $
      CE.assert (LV.length q' == n) $
        return (q', r')
  where
    dsign              = signLitLsbf dividend'
    il `negWhenM` lM   = lM >>= \l -> litIntIte l (litNeg il) (return il)
    absLsbf x          = litIntIte (signLitLsbf x) (litNeg x) (return x)
    signLitLsbf        = LV.last

-- | Performs signed integer division.
signedDivOpDef :: OpDef
signedDivOpDef = binaryIntOpDef 24 "div" mulPrec evalFn litFn
  where evalFn x (-1) = negate x
        evalFn x y = x `quot` y
        litFn x y = fmap fst $ litQuotRem x y

-- | Performs signed integer remainder.
signedRemOpDef :: OpDef
signedRemOpDef = binaryIntOpDef 25 "rem" mulPrec rem litFn
  where litFn x y = fmap snd $ litQuotRem x y

-- Integer comparison relations {{{2

-- | Create a binary operator with strict bit-blasting.
intRelDef :: OpIndex
          -> String
          -> (CValue -> CValue -> Bool)
          -> (LV.Vector Lit -> LV.Vector Lit -> AigMonad Lit)
          -> OpDef
intRelDef opDefIndex opDefName evalFn litFn =
  OpDef { opDefIndex
        , opDefName
        , opDefPrec = compPrec
        , opDefArgTypes = V.replicate 2 (SymInt (varWidth "x"))
        , opDefResultType = SymBool
        , opDefKind = PrimOp
        , opDefEval = BinaryOpEval (\_ x y -> CBool (evalFn x y))
        , opDefDescriptor = \_ args ->
           CE.assert (V.length args == 2) $ do
             LV vx <- getVarLit (args V.! 0)
             LV vy <- getVarLit (args V.! 1)
             CE.assert (LV.length vx == LV.length vy) $
               liftAigMonad $ fmap (LV . LV.singleton) $ litFn vx vy
        }

-- | @litSignedLt ifEq lx ly ifEq@ returns true if lx < ly or lx == ly & ifEq
-- is True.
litSignedLt :: Bool -> LV.Vector Lit -> LV.Vector Lit -> AigMonad Lit
litSignedLt ifEq vx vy = do
  let l = LV.length vx - 1
      x = LV.last vx
      y = LV.last vy
   in litOrM -- Return true if x is negative and y is positive
             (litAnd x (neg y))
             (litAndM -- Return false if x is negative and y is positive.
                      (negM (litAnd (neg x) y))
                      (litLtImplV ifEq (LV.slice 0 l vx) (LV.slice 0 l vy)))

-- | Signed less than or equal comparison.
signedLeqOpDef :: OpDef
signedLeqOpDef = intRelDef 30 "<=s" evalFn (litSignedLt True)
  where evalFn (getSVal -> Just x) (getSVal -> Just y) = x <= y
        evalFn _ _ = error "internal: Illegal arguments to signedLeqOp"

-- | Signed less than comparison.
signedLtOpDef :: OpDef
signedLtOpDef = intRelDef 31 "<s" evalFn (litSignedLt False)
  where evalFn (getSVal -> Just x) (getSVal -> Just y) = x < y
        evalFn _ _ = error "internal: Illegal arguments to signedLtOp"

-- | Unsigned less than or equal comparison.
unsignedLeqOpDef :: OpDef
unsignedLeqOpDef = intRelDef 32 "<=u" evalFn (litLtImplV True)
  where evalFn (getUVal -> Just x) (getUVal -> Just y) = x <= y
        evalFn _ _ = error "internal: Illegal arguments to unsignedLeqOp"

-- | Unsigned less than comparison.
unsignedLtOpDef :: OpDef
unsignedLtOpDef = intRelDef 33 "<u" evalFn (litLtImplV False)
  where evalFn (getUVal -> Just x) (getUVal -> Just y) = x < y
        evalFn _ _ = error $ "Illegal arguments to unsignedLtOp"

-- Array operations {{{2

-- | Returns value if litvector equals constant.
litEqConstant :: LV.Vector Lit -> Int -> AigMonad Lit
litEqConstant v i = do
  lTrue <- litTrue
  LV.foldM (\r j ->
              let l = v LV.! j
               in litAnd r (if i `testBit` j then l else neg l))
           lTrue
           (LV.enumFromN 0 (LV.length v))

-- | ternary operation that returns an after updating a value at a given location.
--   First argument: A zero-based array indexed by 32-bit integers.
--   Second argument: A 32-bit wide integer used as the index.
--   Third argument: Value to store in array
setArrayValueOpDef :: OpDef
setArrayValueOpDef =
    let arrayType = SymArray (varWidth "l") eltType
        idxType = SymInt (varWidth "i")
        eltType = SymShapeVar "e"
     in OpDef { opDefIndex = 40
              , opDefName = "set"
              , opDefPrec = arrayPrec
              , opDefArgTypes = V.fromListN 3 [arrayType, idxType, eltType]
              , opDefResultType = arrayType
              , opDefKind = PrimOp
              , opDefEval =
                  let evalFn (CArray a) (getUVal -> Just i) v
                        | 0 <= i && i < fromIntegral (V.length a)
                        = CArray $ a V.// [(fromIntegral i,v)]
                      evalFn arr idx val =
                        error $ "evalSetArrayValue applied to illegal arguments:"
                          ++ "\n Array: " ++ show arr
                          ++ "\n Index: " ++ show idx
                          ++ "\n Value: " ++ show val
                   in TernaryOpEval (const evalFn)
              , opDefDescriptor = \_ args ->
                  CE.assert (V.length args == 3) $ do
                    LVN va <- getVarLit (args V.! 0)
                    LV vi <- getVarLit (args V.! 1)
                    vv <- getVarLit (args V.! 2)
                    fmap LVN $ V.forM (V.enumFromN 0 (V.length va)) $ \i -> do
                      c <- liftAigMonad $ litEqConstant vi i
                      litIteVectorM c (return vv) (return (va V.! i))
              }

aset :: Term -> Term -> Term -> Term
aset a i v =
  case (termType a, termType i) of
    (SymArray l e, SymInt idxType) ->
      let s = TypeSubst { shapeSubst = Map.fromList [("e", e)]
                        , widthSubst = Map.fromList [("l", l), ("i", idxType)] }
       in appTerm (mkOp setArrayValueOpDef s) [a, i, v]
    _ -> error "Illegal types to aset"

-- | binary operation that returns the value of at a given index in an array.
--   First argument: A zero-based array indexed by signed 32-bit integers.
--   Second argument: A signed 32-bit integer used as the index.
getArrayValueOpDef :: OpDef
getArrayValueOpDef =
  OpDef { opDefIndex = 41
        , opDefName = "get"
        , opDefPrec = arrayPrec
        , opDefArgTypes = V.fromListN 2
            [ SymArray (varWidth "l") (SymShapeVar "e")
            , SymInt (varWidth "i") ]
        , opDefResultType = SymShapeVar "e"
        , opDefKind = PrimOp
        , opDefEval =
           let evalFn :: CValue -> CValue -> CValue
               evalFn (CArray a) (CInt _ i)= a V.! fromIntegral i
               evalFn _ _ = error "evalGetArrayValue applied to illegal arguments"
            in BinaryOpEval (const evalFn)
        , opDefDescriptor = \types args ->
           CE.assert (V.length args == 2) $ do
             let arr = args V.! 0
                 idx = args V.! 1
                 Just (Wx len) = widthConstant (widthSubst types Map.! "l")
             LVN va <- getVarLit arr
             LV vi <- getVarLit idx
             let caseFn 0 al = return $ va V.! al
                 caseFn i al = do
                   let j = i - 1
                       ah = setBit al (fromIntegral j)
                   if ah < len
                     then litIteVectorM (vi LV.! j)
                                        (caseFn j ah)
                                        (caseFn j al)
                     else caseFn j al
             caseFn (LV.length vi - 1) 0
        }

aget :: Term -> Term -> Term
aget a i =
  case (termType a, termType i) of
    (SymArray l e, SymInt idxType) ->
      let s = TypeSubst { shapeSubst = Map.fromList [("e", e)]
                        , widthSubst = Map.fromList [("l", l), ("i", idxType)] }
       in appTerm (mkOp getArrayValueOpDef s) [a, i]
    _ -> error "Illegal types to aget"

-- | @splitOpDef l w@ returns join operator takes an integer with type @[l*w]@ and returns
-- a single integer with type @[l][w]@.
splitOpDef :: BitWidth -> BitWidth -> OpSession OpDef
splitOpDef (Wx l) (Wx w) = do
  cachedOpDef (SplitOpId (Wx l) (Wx w)) $ \opDefIndex ->
    OpDef { opDefIndex
          , opDefName = "split"
          , opDefPrec = defaultPrec
          , opDefArgTypes = V.singleton $
              SymInt (constantWidth (Wx (l * w)))
          , opDefResultType =
              SymArray (constantWidth (Wx l)) (SymInt (constantWidth (Wx w)))
          , opDefKind = PrimOp
          , opDefEval =
              let evalFn _ (getUVal -> Just v) =
                    let getVal i = mkCInt (Wx w) $ v `shiftR` (i * w)
                     in CArray $ V.map getVal (V.enumFromN 0 l)
                  evalFn _ _ = error $ "internal: illegal arguments to splitOpDef"
               in UnaryOpEval evalFn
          , opDefDescriptor = \_ args ->
              CE.assert (V.length args == 1) $ do
                LV lx <- getVarLit (args V.! 0)
                let getVal i = LV $ LV.take w $ LV.drop (w * i) lx
                return $ LVN $ V.map getVal (V.enumFromN 0 l)
          }

-- | @joinOpDef l w@ returns join operator takes an array with type @[l][w]@
-- and returns a single integer with type @l*w@.
-- N.B. Making this operator into a constant requires supporting multiplication
-- in width variables.
joinOpDef :: BitWidth -> BitWidth -> OpSession OpDef
joinOpDef (Wx l) (Wx w) = do
  cachedOpDef (JoinOpId (Wx l) (Wx w)) $ \opDefIndex ->
    OpDef { opDefIndex
          , opDefName = "join"
          , opDefPrec = defaultPrec
          , opDefArgTypes = V.singleton $
              SymArray (constantWidth (Wx l)) (SymInt (constantWidth (Wx w)))
          , opDefResultType = SymInt (constantWidth (Wx (l * w)))
          , opDefKind = PrimOp
          , opDefEval =
              let evalFn _ (CArray v) =
                    let getVal i =
                          let Just uv = getUVal (v V.! i)
                           in uv `shiftL` (i * w)
                     in mkCInt (Wx (l * w)) $
                         V.foldl (.|.) 0 (V.map getVal (V.enumFromN 0 l))
                  evalFn _ _ = error $ "internal: illegal arguments to joinOpDef"
               in UnaryOpEval evalFn
          , opDefDescriptor = \_ args ->
              CE.assert (V.length args == 1) $ do
                LVN lx <- getVarLit (args V.! 0)
                return $ LV $ LV.concat $ V.toList $ V.map toLsbfV lx
          }

-- | @mkArrayOpDef l@ returns an operator definition that takes @l@ arguments,
-- and joins them into a single array with @l@ elements.
mkArrayOpDef :: Int -> OpSession OpDef
mkArrayOpDef l = do
  cachedOpDef (MkArrayOpId l) $ \idx ->
    OpDef { opDefIndex = idx
          , opDefName = "mkArray"
          , opDefPrec = defaultPrec
          , opDefArgTypes = V.replicate l (SymShapeVar "e")
          , opDefResultType =
              SymArray (constantWidth (Wx l)) (SymShapeVar "e")
          , opDefKind = PrimOp
          , opDefEval = VectorOpEval $ \_ args -> CArray args
          , opDefDescriptor = \_ args -> do
              fmap LVN $ V.mapM getVarLit args
          }

-- | @mkArrayOp l eltType@ returns the mkArray operator that takes @l@ arguments
-- with type @eltType@ and joins them into a single array with @l@ elements of
-- type @eltType@.
mkArrayOp :: Int -> DagType -> OpSession Op
mkArrayOp l eltType = do
  op <- mkArrayOpDef l
  let sub = emptySubst { shapeSubst = Map.fromList [("e", eltType)] }
  return $ mkOp op sub

-- WordMonad SymbolicMonad instance {{{1

-- | Helper function for applying binary operations in symbolic monad.
applyBinaryIntOp :: OpDef -> String -> Node -> Node -> SymbolicMonad Node
applyBinaryIntOp opDef name x y = do
  case termType x of
    SymInt (widthConstant -> Just xw) -> do
      applyBinaryOp (widthOp opDef xw) x y
    _ -> error $ "internal: illegal value to " ++ name ++ ": " ++ show x

-- | Helper function for applying shift operations in symbolic monad.
applyShift :: OpDef -> String -> Node -> Node -> SymbolicMonad Node
applyShift opDef name x y = do
  case (termType x, termType y) of
    (SymInt vw, SymInt sw) -> do
      let opSubst = TypeSubst {
                      shapeSubst = Map.empty
                    , widthSubst = Map.fromList [ ("v", vw), ("s", sw) ]
                    }
      applyBinaryOp (mkOp opDef opSubst) x y
    _ -> error $ "internal: illegal value to " ++ name ++ ": " ++ show x

int32Type :: DagType
int32Type = SymInt (constantWidth 32)

int64Type :: DagType
int64Type = SymInt (constantWidth 64)

instance WordMonad SymbolicMonad where
  termInt  = return . mkCInt (Wx 32) . fromIntegral
  termLong = return . mkCInt (Wx 64) . fromIntegral
  termBool = return . mkCBool

  freshByte = do
    lv <- liftAigMonad $ do
      inputs <- LV.replicateM 7 makeInputLit
      msb <- makeInputLit
      return $ inputs LV.++ LV.replicate 25 msb
    freshVar int32Type (LV lv)

  freshInt = do
    lv <- liftAigMonad $ LV.replicateM 32 makeInputLit
    freshVar int32Type (LV lv)

  freshLong = do
    lv <- liftAigMonad $ LV.replicateM 64 makeInputLit
    freshVar int64Type (LV lv)

  freshUninterpretedVar tp = freshInput Nothing tp
  freshVar tp lv = freshInput (Just lv) tp

  applyEq x y = applyBinaryOp (shapeOp eqOpDef (termType x)) x y
  applyIte (getBool -> Just True)  t _ = return t
  applyIte (getBool -> Just False) _ f = return f
  applyIte b t f
    | t == f = return t
    | otherwise = do
        applyTernaryOp (shapeOp iteOpDef (termType t)) b t f

  applyBNot = applyUnaryOp (groundOp bNotOpDef)
  applyBAnd = applyBinaryOp (groundOp bAndOpDef)
  applyBOr  = applyBinaryOp (groundOp bOrOpDef)
  applyBXor = applyBinaryOp (groundOp bXorOpDef)

  applyINot x = do
    case termType x of
      SymInt (widthConstant -> Just xw) ->
        applyUnaryOp (mkOp iNotOpDef (mkWidthSubst xw)) x
      _ -> error $ "internal: illegal value to applyINot: " ++ show x
  applyIAnd = applyBinaryIntOp iAndOpDef "applyIAnd"
  applyIOr = applyBinaryIntOp iOrOpDef "applyIOr"
  applyIXor = applyBinaryIntOp iXorOpDef "applyIXor"

  applyAppendInt x y =
    case (termType x, termType y) of
     (SymInt (widthConstant -> Just xw), SymInt (widthConstant -> Just yw)) -> do
       let s = TypeSubst {
                       shapeSubst = Map.empty
                     , widthSubst = Map.fromList [ ("x", constantWidth xw)
                                                 , ("y", constantWidth yw)]
                     }
       applyBinaryOp (mkOp appendIntOpDef s) x y
     _ -> error "internal: illegal arguments to applyAppendInt"
  applyTrunc resultWidth t = do
    case termType t of
      SymInt (widthConstant -> Just inputWidth)
       | inputWidth >= resultWidth -> do
         op <- liftOpSession $ truncOpDef resultWidth
         applyUnaryOp (mkOp op (mkWidthSubst inputWidth)) t
      _ -> error "internal: illegal arguments to applyTrunc"
  applySignedExt resultWidth t =
    case termType t of
      SymInt (widthConstant -> Just inputWidth) -> do
        op <- liftOpSession $ signedExtOpDef resultWidth
        applyUnaryOp (mkOp op (mkWidthSubst inputWidth)) t
      _ -> error "internal: illegal value to applySignedExt"

  applyShl  = applyShift shlOpDef  "applyShl"
  applyShr  = applyShift shrOpDef  "applyShr"
  applyUshr = applyShift ushrOpDef "applyUshr"

  applySignedLeq   = applyBinaryIntOp signedLeqOpDef   "applySignedLeq"
  applySignedLt    = applyBinaryIntOp signedLtOpDef    "applySignedLt"
  applyUnsignedLeq = applyBinaryIntOp unsignedLeqOpDef "applyUnsignedLeq"
  applyUnsignedLt  = applyBinaryIntOp unsignedLtOpDef  "applyUnsignedLt"

  applyAdd = applyBinaryIntOp addOpDef "applyAdd"
  applyMul = applyBinaryIntOp mulOpDef "applyMul"
  applyNeg x =
    case termType x of
      SymInt (widthConstant -> Just xw) ->
        applyUnaryOp (widthOp negOpDef xw) x
      _ -> error $ "internal: illegal value to applyNeg"
  applySignedDiv = applyBinaryIntOp signedDivOpDef "applySignedDiv"
  applySignedRem = applyBinaryIntOp signedRemOpDef "applySignedRem"
  applySub = applyBinaryIntOp subOpDef "applySub"

  applyGetArrayValue a i =
    case (termType a, termType i) of
      (SymArray len eltType, SymInt idxType) -> do
        let sub = TypeSubst {
                        shapeSubst = Map.fromList [("e", eltType)]
                      , widthSubst = Map.fromList [("l", len), ("i", idxType)]
                      }
        applyBinaryOp (mkOp getArrayValueOpDef sub) a i
      _ -> error $ "internal: illegal arguments to applyGetArrayValue "
                    ++ prettyTerm a ++ "\n" ++ show (termType a)
  applySetArrayValue a i v =
    case (termType a,termType i) of
      (SymArray len eltType, SymInt idxType) -> do
        let sub = TypeSubst {
                        shapeSubst = Map.fromList [("e", eltType)]
                      , widthSubst = Map.fromList [("l", len), ("i", idxType)]
                      }
        applyTernaryOp (mkOp setArrayValueOpDef sub) a i v
      _ -> error "internal: illegal arguments to applySetArrayValue"
  symbolicArrayFromV eltType values = do
    op <- liftOpSession $ mkArrayOpDef (V.length values)
    let sub = emptySubst { shapeSubst = Map.fromList [("e", eltType)] }
    applyOpVector (mkOp op sub) values
