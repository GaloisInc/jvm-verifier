{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
module SAWScript.SmtLib2 (translate, TransParams(..), MetaData(..)) where

import GHC.Exts(IsString(fromString))
import SMTLib2 as SMT
import SMTLib2.Core as SMT
import SMTLib2.BitVector as SMT
import SMTLib2.Array as SMT
import MonadLib
import Verinf.Symbolic.Common
  ( CValue(..), Op(..), OpDef(..), TermSemantics(..)
  , WidthExpr, widthConstant
  , DagType(..)
  , opArgTypes, opResultType, numBits
  , OpSem(..), OpIndex
  )
import Verinf.Symbolic(Node,deEval,getSValW)
import qualified Verinf.Symbolic.Common as Op (OpIndex(..))
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Text.PrettyPrint



data TransParams = TransParams
  { transInputs   :: [DagType]
  , transAssume   :: Node
  , transCheck    :: [Node]
  , transEnabled  :: S.Set OpIndex
  , transExtArr   :: !Bool   -- use array extensionality?
  }


data MetaData = MetaData
  { trAsmp     :: Expr
  , trGoals    :: [Expr]
  , trInputs   :: [Ident]
  , trUninterp :: [(Name, String)]
  , trDefined  :: M.Map String [ (V.Vector Expr, Expr) ]
  , trArrays   :: M.Map Ident [Ident]
  }


translate :: TransParams -> IO (Script, MetaData)
translate ps =
  toScript (transExtArr ps) $
  do (xs,ts) <- unzip `fmap` mapM mkInp (transInputs ps)
     let js = V.fromList ts
     eval <- io $ deEval (\i _ _ -> js V.! i) (translateOps (transEnabled ps))
     as <- asExpr `fmap` eval (transAssume ps)
     addAssumpt as
     gs <- mapM (fmap asExpr . eval) (transCheck ps)
     ops <- getExtraOps
     dops <- getDefinedOps
     arrs <- getArrayUpdates
     return ( case gs of
                [] -> SMT.true
                _  -> SMT.not (foldr1 SMT.and gs)
            , MetaData
                { trAsmp = as
                , trGoals = gs
                , trInputs = xs
                , trUninterp = IM.elems ops
                , trDefined = M.fromList
                            [ (f, map fixupExpr $ M.toList m) |
                                                    (f,m) <- IM.elems dops ]
                , trArrays = arrs
                }
            )

  where
  fixupExpr (as,b) = (V.map asExpr as, asExpr b)

  mkInp ty = do t    <- cvtType ty
                term <- newConst t
                x    <- toVar term
                addNote $
                  text "input:" <+> pp term <+> text "::" <+> text (show t)
                return (x, FTerm { smtType = t, asExpr = term })


type X = String



toScript :: Bool -> M (Expr, a) -> IO (Script, a)
toScript useExt (M m) =
  do res <- runExceptionT $ runStateT s0 $ runReaderT r0 m
     case res of
       Left xx -> fail xx -- XXX: Throw a custom exception.
       Right ((a,other),s) -> return (Script $
           [ CmdSetLogic (fromString "QF_AUFBV")
           ] ++
           -- , CmdNotes $ show $ notes s
           [ CmdDeclareFun i (map toSort as) (toSort b)
                                      | (i,as,b) <- globalDefs s
           ] ++
           [ CmdDeclareFun p (map toSort as) tBool | (p,as) <- globalPreds s
           ] ++
           [ CmdAssert f | f <- globalAsmps s
           ] ++
           [ CmdAssert a
           , CmdCheckSat
           , CmdExit
           ]
         , other
         )

    where s0 = S { names    = 0
                 , globalDefs = []
                 , globalPreds = []
                 , globalAsmps = []
                 , extraAbs = IM.empty
                 , extraDef = IM.empty
                 , notes    = text "Detail about variables:"
                 , arrayUpdates = M.empty
                 }

          r0 = R { useExtArr = useExt }

io :: IO a -> M a
io m = M (lift $ lift $ lift m)


bug :: String -> String -> M a
bug f x = M $ raise $ unlines [ "Internal error."
                              , "*** detected in: module SAWScript.SmtLib"
                              , "*** function: " ++ f
                              , "*** " ++ x
                              ]


err :: String -> M a
err x = M $ raise $ unlines [ "Error whilte translating to SMTLIB:"
                            , "*** " ++ x
                            ]

toVar :: Expr -> M Ident
toVar (App x _ [])  = return x
toVar r             = bug "translate.toVar"
                        $ unlines [ "Argument is not a variable"
                                  , show (pp r)
                                  ]


--------------------------------------------------------------------------------
-- The Monad

newtype M a = M (ReaderT R (StateT S (ExceptionT X IO)) a)
                deriving (Functor, Monad)

data R = R { useExtArr :: Bool }

data S = S { names        :: !Int
           , globalDefs   :: [(Name,[SmtType],SmtType)]
           , globalPreds  :: [(Name,[SmtType])]
           , globalAsmps  :: [Expr]
           , extraAbs     :: IM.IntMap (Name, String)  -- ^ uninterpreted ops.
           , extraDef     :: IM.IntMap
                              (String, M.Map (V.Vector FTerm) FTerm)
            -- ^ defined ops
           , arrayUpdates :: M.Map Ident [Ident]
           , notes        :: Doc
           }

useExtArrays :: M Bool
useExtArrays = M (useExtArr `fmap` ask)

addNote :: Doc -> M ()
addNote d = M $ sets_ $ \s -> s { notes = notes s $$ d }

addAssumpt :: Expr -> M ()
addAssumpt f = M $ sets_ $ \s -> s { globalAsmps = f : globalAsmps s }


addArrayUpdate :: Ident -> Ident -> M ()
addArrayUpdate old new =
  M $ sets_ $ \s -> s { arrayUpdates = M.insertWith (++) old [new]
                                                        (arrayUpdates s) }

getArrayUpdates :: M (M.Map Ident [Ident])
getArrayUpdates = M $ arrayUpdates `fmap` get


getDefinedOps :: M (IM.IntMap (String, M.Map (V.Vector FTerm) FTerm))
getDefinedOps = M $ extraDef `fmap` get

lkpDefinedOp :: Int -> V.Vector FTerm -> M (Maybe FTerm)
lkpDefinedOp x ts =
  do mp <- getDefinedOps
     return (M.lookup ts . snd =<< IM.lookup x mp)

addDefinedOp :: Int -> String -> V.Vector FTerm -> FTerm -> M ()
addDefinedOp x name ts t = M $ sets_ $
  \s -> s { extraDef = IM.alter upd x (extraDef s) }
  where upd val = Just (case val of
                          Nothing -> (name,M.singleton ts t)
                          Just (n,mp) -> (n,M.insert ts t mp))



padArray :: SmtType -> Expr -> M Expr
padArray ty@(TArray n w) t =
  do useExt <- useExtArrays
     if useExt
       then do let ixW = needBits n
               arr <- saveT ty t
               forM_ [ n .. 2^ixW - 1 ] $ \i ->
                 addAssumpt (select arr (bv i ixW) === bv 0 w)
               return arr
       else return t

padArray _ t = return t

getExtraOps :: M (IM.IntMap (Name, String))
getExtraOps = M $ extraAbs `fmap` get

-- Note: does not do any padding on arrays.
-- This happens where the fun is used.
newFun :: [SmtType] -> SmtType -> M Name
newFun ts t = M $ sets $ \s -> let n = names s
                                   i = fromString ("x" ++ show n)
                               in ( i
                                  , s { names = n + 1
                                      , globalDefs = (i,ts,t) : globalDefs s
                                      }
                                  )

newConst :: SmtType -> M Expr
newConst ty =
  do f <- newFun [] ty
     padArray ty (app (I f []) [])




-- Give an explicit name to a term.
-- This is useful so that we can share term representations.
-- Note that this assumes that we don't use "lets" in formulas,
-- (which we don't) because otherwise lifting things to the top-level
-- might result in undefined local variables.
saveT :: SmtType -> Expr -> M Expr
saveT ty t =
  do useExt <- useExtArrays
     case t of
       App _ _ (_ : _) -> case ty of
                            TArray _ _ | Prelude.not useExt -> return t
                            _ -> doSave
       _               -> return t

  where doSave = do x <- newConst ty
                    addAssumpt (x === t)
                    addNote $ pp x <> char ':' <+> pp t
                    return x

save :: FTerm -> M FTerm
save t = do t1 <- saveT (smtType t) (asExpr t)
            return t { asExpr = t1 }

-- For now, we work only with staticlly known sizes of things.
wToI :: WidthExpr -> M Integer
wToI x = case widthConstant x of
           Just y -> return (numBits y)
           _      -> err "Non-constnat width"



--------------------------------------------------------------------------------
-- Converting from HAJava types into SMTLIB types.

-- | The array type contains the number of elements, not the
-- the number of bits, as in SMTLIB!
data SmtType = TBool | TArray Integer Integer | TBitVec Integer
               deriving (Eq,Ord,Show)

cvtType :: DagType -> M SmtType
cvtType ty =
  case ty of

    SymBool  -> return TBool

    SymInt w -> TBitVec `fmap` wToI w

    SymArray w ty1 ->
      do n <- wToI w
         s <- cvtType ty1
         case s of
           TBitVec m -> return (TArray n m)
           -- We might be able to support mult-dimensional arrays
           -- by concatenating their index bits
           -- Example:  Array[2+4:8]    is [4][16][8]
           _         -> err "Complex nested array"

    SymRec _ _    -> err "Record"

    SymShapeVar _ -> err "Type variable"

toSort :: SmtType -> SMT.Type
toSort ty =
  case ty of
    TBool       -> tBool
    TArray x y  -> tArray (tBitVec (needBits x)) (tBitVec y)
    TBitVec n   -> tBitVec n

-- How many bits do we need to represent the given number.
needBits :: Integer -> Integer
needBits n | n <= 0     = 0
needBits n | odd n      = 1 + needBits (div n 2)
           | otherwise  = needBits (n + 1)




--------------------------------------------------------------------------------
data FTerm = FTerm { asExpr   :: Expr
                   , smtType  :: SmtType      -- Type of the term
                   } deriving (Show)

instance Eq FTerm where x == y = asExpr x == asExpr y
instance Ord FTerm where compare x y = compare (asExpr x) (asExpr y)


translateOps :: S.Set OpIndex -> TermSemantics M FTerm
translateOps enabled = termSem
  where
  termSem = TermSemantics
    { tsEqTerm = same
    , tsConstant = \c t -> mkConst c =<< cvtType t
    , tsApplyUnary = apply1
    , tsApplyBinary = apply2
    , tsApplyTernary = apply3
    , tsApplyOp = apply
    }


  -- Spot when things are obviously the same.
  same x y = case (asExpr x, asExpr y) of
               (App c s [], App d t []) -> (c,s) == (d,t)
               (Lit c, Lit d)           -> c == d
               _                        -> False


  apply1 op = lift1 $
    case opDefIndex (opDef op) of
      Op.Trunc w     -> truncOp (numBits w)
      Op.SignedExt w -> signedExtOp (numBits w)
      Op.Not         -> bNotOp
      Op.INot        -> iNotOp
      Op.Neg         -> negOp
      Op.Split w1 w2 -> splitOp (numBits w1) (numBits w2)
      Op.Join w1 w2  -> joinOp (numBits w1) (numBits w2)
      Op.Dynamic x m -> \t -> dynOp x op m (V.fromList [t])

      i -> \_ -> err $ "Unknown unary operator: " ++ show i
                    ++ " (" ++ opDefName (opDef op) ++ ")"

  apply2 op = lift2 $
    case opDefIndex (opDef op) of
      Op.Eq            -> eqOp
      Op.And           -> bAndOp
      Op.Or            -> bOrOp
      Op.Xor           -> bXorOp
      Op.Implies       -> bImpliesOp
      Op.IAnd          -> iAndOp
      Op.IOr           -> iOrOp
      Op.IXor          -> iXorOp
      Op.Shl           -> shlOp
      Op.Shr           -> shrOp
      Op.Ushr          -> ushrOp
      Op.AppendInt     -> appendOp
      Op.Add           -> addOp
      Op.Mul           -> mulOp
      Op.Sub           -> subOp
      Op.SignedDiv     -> signedDivOp
      Op.SignedRem     -> signedRemOp
      Op.UnsignedDiv   -> unsignedDivOp
      Op.UnsignedRem   -> unsignedRemOp
      Op.SignedLeq     -> signedLeqOp
      Op.SignedLt      -> signedLtOp
      Op.UnsignedLeq   -> unsignedLeqOp
      Op.UnsignedLt    -> unsignedLtOp
      Op.GetArrayValue -> getArrayValueOp
      Op.Dynamic x m   -> \s t -> dynOp x op m (V.fromList [s,t])

      i -> \_ _ -> err $ "Unknown binary operator: " ++ show i
                    ++ " (" ++ opDefName (opDef op) ++ ")"

  apply3 op = lift3 $
    case opDefIndex (opDef op) of
      Op.ITE           -> iteOp
      Op.SetArrayValue -> setArrayValueOp
      Op.Dynamic x m   -> \r s t -> dynOp x op m (V.fromList [r,s,t])

      i -> \_ _ _ -> err $ "Unknown ternary operator: " ++ show i
                    ++ " (" ++ opDefName (opDef op) ++ ")"

  apply op = liftV $
    case opDefIndex (opDef op) of
      Op.MkArray _  -> \vs -> do t <- cvtType (opResultType op)
                                 mkArray t vs
      Op.Dynamic x m   -> \xs -> dynOp x op m (V.fromList xs)

      i -> \_ -> err $ "Unknown variable arity operator: " ++ show i
                    ++ " (" ++ opDefName (opDef op) ++ ")"


  lift1 op t        = do s1 <- save t
                         op s1

  lift2 op t1 t2    = do s1 <- save t1
                         s2 <- save t2
                         op s1 s2

  lift3 op t1 t2 t3 = do s1 <- save t1
                         s2 <- save t2
                         s3 <- save t3
                         op s1 s2 s3

  liftV op ts       = do ss <- mapM save (V.toList ts)
                         op ss


  dynOp x op mbSem args =
    case mbSem of
      Just sem | opDefIndex (opDef op) `S.member` enabled ->
        do as <- V.mapM save args
           mb <- lkpDefinedOp x as
           case mb of
             Just t -> return t
             Nothing ->
               do t <- save =<< applyOpSem sem (opSubst op) termSem as
                  addDefinedOp x (opDefName (opDef op)) as t
                  return t

      _ ->
        do as <- mapM cvtType (V.toList (opArgTypes op))
           b  <- cvtType (opResultType op)
           let name = opDefName (opDef op)

           -- Check to see if we already generated a funciton for this op.
           known <- getExtraOps
           f <- case IM.lookup x known of
                  Nothing ->
                    do f <- newFun as b
                       M $ sets_ $ \s -> s { extraAbs = IM.insert x (f,name)
                                                           (extraAbs s) }
                       return f
                  Just (f,_) -> return f

           arr <- padArray b (app (I f []) (map asExpr (V.toList args)))
           return FTerm { asExpr = arr, smtType = b }


--------------------------------------------------------------------------------
-- Operations



mkConst :: CValue -> SmtType -> M FTerm
mkConst val t =
  case val of
    _ | Just (w,v) <- getSValW val ->
      return FTerm { asExpr  = bv v (numBits w)
                   , smtType = t
                   }
    CBool b ->
      return FTerm { asExpr  = if b then true else false
                   , smtType = t
                   }

    CArray vs ->
      case t of
        TArray _ w ->
          do xs <- mapM (`mkConst` TBitVec w) (V.toList vs)
             mkArray t xs

        _ -> bug "mkConst" "Type error---array constant of non-array type."

    CRec {} -> err "mkConst does not support records at the moment"


-- NOTE: Arrays whose size is not a power of 2 are padded with 0s.
-- This is needed because otherwise  we can detect fake differences
-- between arrays, that are equal on their proper range, but differ
-- in the padding.
mkArray :: SmtType -> [FTerm] -> M FTerm
mkArray t xs =
  case t of
    TArray n _ ->
      do a  <- newConst t
         let ixW = needBits n
         zipWithM_ (\i x -> addAssumpt (select a (bv i ixW) === x))
                   [ 0 .. ] (map asExpr xs)
         return FTerm { asExpr = a
                      , smtType = t
                      }

    _ -> bug "mkArray" "Type error---the type of mkArray is not an array."


-- for ops of type: a -> a -> bool
relOp :: (Expr -> Expr -> Expr) -> FTerm -> FTerm -> M FTerm
relOp f s t = return FTerm { asExpr = f (asExpr s) (asExpr t), smtType = TBool }

-- for ops of type: a -> a -> a
binOp :: (Expr -> Expr -> Expr) -> FTerm -> FTerm -> M FTerm
binOp f s t = return FTerm { asExpr = f (asExpr s) (asExpr t)
                           , smtType = smtType s }

eqOp :: FTerm -> FTerm -> M FTerm
eqOp s t =
  case smtType s of
    TArray n _
      | n == 0  -> return FTerm { asExpr = SMT.true, smtType = TBool }
      | otherwise ->
          do a <- asExpr `fmap` save s
             b <- asExpr `fmap` save t
             let w = needBits n
             let cmp i = select a (bv i w) === select b (bv i w)
             return FTerm { asExpr = foldr1 SMT.and $ map cmp [ 0 .. n - 1 ]
                          , smtType = TBool }
    _ -> relOp (===) s t


-- = relOp (===)

iteOp :: FTerm -> FTerm -> FTerm -> M FTerm
iteOp b t2 t3 =
  return FTerm { asExpr = ite (asExpr b) (asExpr t2) (asExpr t3)
               , smtType = smtType t2   -- or t3
               }

truncOp :: Integer -> FTerm -> M FTerm
truncOp n t =
  return FTerm { asExpr = if n == 0 then bv 0 0
                                    else extract (n-1) 0 (asExpr t)
               , smtType = TBitVec n
               }


signedExtOp :: Integer -> FTerm -> M FTerm
signedExtOp n t =
  case smtType t of
    TBitVec m | m < n  -> return FTerm { asExpr = sign_extend (n - m) (asExpr t)
                                       , smtType = TBitVec n
                                       }
              | m == n -> return t
    _ -> bug "signedExtOp"
             "Sign extending to a smaller value, or type error."


bNotOp :: FTerm -> M FTerm
bNotOp t = return FTerm { asExpr = SMT.not (asExpr t), smtType = TBool }

bAndOp :: FTerm -> FTerm -> M FTerm
bAndOp = binOp SMT.and

bOrOp :: FTerm -> FTerm -> M FTerm
bOrOp = binOp SMT.or

bXorOp :: FTerm -> FTerm -> M FTerm
bXorOp = binOp SMT.xor

bImpliesOp :: FTerm -> FTerm -> M FTerm
bImpliesOp = binOp (==>)

iNotOp :: FTerm -> M FTerm
iNotOp t = return FTerm { asExpr  = bvnot (asExpr t)
                        , smtType = smtType t
                        }


iAndOp :: FTerm -> FTerm -> M FTerm
iAndOp = binOp bvand

iOrOp :: FTerm -> FTerm -> M FTerm
iOrOp = binOp bvor

iXorOp :: FTerm -> FTerm -> M FTerm
iXorOp = binOp bvxor

mkShiftOp :: (Expr -> Expr -> Expr) -> FTerm -> FTerm -> M FTerm
mkShiftOp f s t =
  case smtType s of
    TBitVec n ->
      do t1 <- coerce n t
         return FTerm { asExpr  = f (asExpr s) (asExpr t1)
                      , smtType = smtType s
                      }
    _ -> bug "mkShiftOp" "Type error---shifting a non-bit vector"


shlOp :: FTerm -> FTerm -> M FTerm
shlOp = mkShiftOp bvshl

shrOp :: FTerm -> FTerm -> M FTerm
shrOp = mkShiftOp bvashr

ushrOp :: FTerm -> FTerm -> M FTerm
ushrOp = mkShiftOp bvlshr

appendOp :: FTerm -> FTerm -> M FTerm
appendOp s t    =
  case (smtType s, smtType t) of
    (TBitVec m, TBitVec n) ->
      return FTerm { asExpr  = SMT.concat (asExpr t) (asExpr s)
                   , smtType = TBitVec (m + n)
                   }
    _ -> bug "appendOp" "Type error---arguments are not bit vectors"


addOp :: FTerm -> FTerm -> M FTerm
addOp = binOp bvadd

mulOp :: FTerm -> FTerm -> M FTerm
mulOp = binOp bvmul

subOp :: FTerm -> FTerm -> M FTerm
subOp = binOp bvsub

negOp :: FTerm -> M FTerm
negOp s = return FTerm { asExpr  = bvneg (asExpr s), smtType = smtType s }


signedDivOp :: FTerm -> FTerm -> M FTerm
signedDivOp = binOp bvsdiv

signedRemOp :: FTerm -> FTerm -> M FTerm
signedRemOp = binOp bvsdiv

unsignedDivOp :: FTerm -> FTerm -> M FTerm
unsignedDivOp = binOp bvudiv

unsignedRemOp :: FTerm -> FTerm -> M FTerm
unsignedRemOp = binOp bvurem

signedLeqOp :: FTerm -> FTerm -> M FTerm
signedLeqOp = relOp bvsle

signedLtOp :: FTerm -> FTerm -> M FTerm
signedLtOp = relOp bvslt

unsignedLeqOp :: FTerm -> FTerm -> M FTerm
unsignedLeqOp = relOp bvule

unsignedLtOp :: FTerm -> FTerm -> M FTerm
unsignedLtOp = relOp bvugt


coerce :: Integer -> FTerm -> M FTerm
coerce w t =
  case smtType t of
    TBitVec n | n == w    -> return t
              | n > w     -> truncOp w t
              | otherwise -> return t {asExpr = zero_extend (w - n) (asExpr t)}
    _ -> bug "coerce" "Type error---coercing a non-bit vector"

getArrayValueOp :: FTerm -> FTerm -> M FTerm
getArrayValueOp a i =
  case smtType a of
    TArray w n ->
      do j <- coerce (needBits w) i
         return FTerm { asExpr  = select (asExpr a) (asExpr j)
                      , smtType = TBitVec n
                      }
    _ -> bug "getArrayValueOp" "Type error---selecting from a non-array."


setArrayValueOp :: FTerm -> FTerm -> FTerm -> M FTerm
setArrayValueOp a i v =
  case smtType a of
    ty@(TArray w _) ->
      do j <- coerce (needBits w) i
         extArr <- useExtArrays
         new <- if extArr
                   then do old <- saveT ty (asExpr a)
                           new <- saveT ty (store old (asExpr j) (asExpr v))
                           oi  <- toVar old
                           ni  <- toVar new
                           addArrayUpdate oi ni
                           return new

                   else return (store (asExpr a) (asExpr j) (asExpr v))

         return FTerm { asExpr  = new
                      , smtType = smtType a
                      }
    _ -> bug "setArrayValueOp" "Type error---updating a non-array."


splitOp :: Integer -> Integer -> FTerm -> M FTerm
splitOp l w t0 =
  do  t <- save t0
      let vs = [ FTerm { asExpr  = extract ((i+1) * w - 1) (i * w) (asExpr t)
                       , smtType = TBitVec w
                       } | i <- [ 0 .. l - 1 ] ]
      mkArray (TArray l w) vs



joinOp :: Integer -> Integer -> FTerm -> M FTerm
joinOp 0 _ _ = return FTerm { asExpr = bv 0 0
                            , smtType = TBitVec 0
                            }
joinOp l w t0 =
  do t <- save t0
     let n = needBits l
     return FTerm
       { asExpr = foldr1 (flip SMT.concat)
                     [ select (asExpr t) (bv i n) | i <- [ 0 .. l - 1 ] ]
       , smtType = TBitVec (l * w)
       }


