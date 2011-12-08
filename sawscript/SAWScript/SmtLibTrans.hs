{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
module SAWScript.SmtLibTrans (translate, TransParams(..), MetaData(..)) where

import GHC.Exts(IsString(fromString))
import SMTLib1.QF_AUFBV as BV
import MonadLib
import Verinf.Symbolic.Common
  ( CValue(..), Op(..), OpDef(..), TermSemantics(..)
  , WidthExpr, widthConstant
  , DagType(..)
  , opArgTypes, opResultType, numBits
  , OpIndex
  , recDefFieldNames, recFieldTypes
  )
import Verinf.Symbolic(DagTerm, evalDagTerm, opDefDefinition, evalDagTermFn, getSValW)
import qualified Verinf.Symbolic.Common as Op (OpIndex(..))
import Verinf.Utils.CacheM
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.List as L
import Text.PrettyPrint

data TransParams = TransParams
  { transName     :: String
  , transInputs   :: [DagType]
  , transAssume   :: DagTerm
  , transCheck    :: [DagTerm]
  , transEnabled  :: S.Set OpIndex
  , transExtArr   :: Bool
  }

data MetaData = MetaData
  { trAsmp     :: Formula
  , trGoals    :: [Formula]
  , trInputs   :: [Ident]
  , trUninterp :: [(Ident, String)]
  , trDefined  :: M.Map String [ (V.Vector (BV.Term), BV.Term) ]
  , trArrays   :: M.Map Ident [Ident]
  }

translate :: TransParams -> IO (Script, MetaData)
translate ps =
  toScript ps (transName ps) (transExtArr ps) $
  do (xs,ts) <- unzip `fmap` mapM mkInp (transInputs ps)
     let js = V.fromList ts
     let inputFn i _ = return (js V.! i)
     eval <- evalDagTermFn inputFn (translateOps (transEnabled ps))
     as <- toForm =<< eval (transAssume ps)
     addAssumpt as
     gs <- mapM (toForm <=< eval) (transCheck ps)
     ops <- getExtraOps
     dops <- getDefinedOps
     arrs <- getArrayUpdates
     return ( Conn Not [ Conn And gs ]
            , MetaData
                { trAsmp = as
                , trGoals = gs
                , trInputs = xs
                , trUninterp = IM.elems ops
                , trDefined = M.fromList
                            [ (f, map fixupTerm $ M.toList m) |
                                                    (f,m) <- IM.elems dops ]
                , trArrays = arrs
                }
            )

  where
  fixupTerm (as,b) = (V.map asTerm as, asTerm b)

  toForm x = case asForm x of
               Nothing -> bug "translate" "Type error---not a formula"
               Just f  -> return f

  mkInp ty = do t    <- cvtType ty
                term <- newConst t
                x    <- toVar term
                addNote $ text "input:" <+> pp term <+> text "::" <+> text (show t)
                return (x, fromTerm t term)


type X = String

toScript :: TransParams -> String -> Bool -> M (Formula, a) -> IO (Script, a)
toScript ps n extArr (M m) =
  do res <- runExceptionT $ runStateT s0 $ runReaderT r0 m
     case res of
       Left xx -> fail xx -- XXX: Throw a custom exception.
       Right ((a,other),s) -> return (Script
         { scrName     = fromString n
         , scrCommands =
           [ CmdLogic (fromString "QF_AUFBV")
           , CmdNotes $ show $ notes s
           , CmdExtraSorts (globalSorts s)
           , CmdExtraFuns
               [ FunDecl { funName = i
                         , funArgs = map toSort as
                         , funRes  = toSort b
                         , funAnnots = [] }
               | (i,as,b) <- globalDefs s
               ]
           , CmdExtraPreds
               [ PredDecl { predName = p
                          , predArgs = map toSort as
                          , predAnnots = [] }
               | (p,as) <- globalPreds s
               ]
           ] ++
           [ CmdAssumption f | f <- globalAsmps s
           ] ++
           [ CmdFormula a
           ]
         }
         , other
         )

    where s0 = S { names        = 0
                 , globalSorts  = []
                 , globalDefs   = []
                 , globalPreds  = []
                 , globalAsmps  = []
                 , extraAbs     = IM.empty
                 , extraDef     = IM.empty
                 , notes        = text "Detail about variables:"
                 , arrayUpdates = M.empty
                 , recTys       = M.empty
                 , transParams  = ps
                 }
          r0 = R { useExtArr = extArr }

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

toVar :: BV.Term -> M Ident
toVar (App x [])  = return x
toVar _           = bug "translate.toVar" "Argument is not a variable"

--------------------------------------------------------------------------------
-- The Monad

newtype M a = M (ReaderT R (StateT S (ExceptionT X IO)) a)
              deriving (Functor, Monad)

data R = R { useExtArr :: Bool }

data S = S { names        :: !Int
           , globalSorts  :: [Sort]
           , globalDefs   :: [(Ident,[SmtType],SmtType)]
           , globalPreds  :: [(Ident,[SmtType])]
           , globalAsmps  :: [Formula]
           , extraAbs     :: IM.IntMap (Ident, String)  -- ^ uninterpreted ops
           , extraDef     :: IM.IntMap
                              (String, M.Map (V.Vector FTerm) FTerm)
                                                        -- ^ defined ops
           , arrayUpdates :: M.Map Ident [Ident]
           , recTys       :: M.Map String SmtType
           , notes        :: Doc
           , transParams  :: TransParams
           }

instance CacheM M where
  newCache          = M $ inBase newCache
  updateCache r k v = M $ inBase $ updateCache r k v
  lookupCache r k   = M $ inBase $ lookupCache r k

useExtArrays :: M Bool
useExtArrays = M (useExtArr `fmap` ask)

addNote :: Doc -> M ()
addNote d = M $ sets_ $ \s -> s { notes = notes s $$ d }

addAssumpt :: Formula -> M ()
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

padArray :: SmtType -> BV.Term -> M BV.Term
padArray ty@(TArray n w) t =
  do ext <- useExtArrays
     if ext
       then do let ixW = needBits n
               arr <- saveT ty t
               forM_ [ n .. 2^ixW - 1 ] $ \i ->
                 addAssumpt (select arr (bv i ixW) === bv 0 w)
               return arr
       else return t -- No need to pad, because we only compare
                     -- meaningful elements

padArray _ t = return t

getExtraOps :: M (IM.IntMap (Ident, String))
getExtraOps = M $ extraAbs `fmap` get

-- Note: does not do any padding on arrays.
-- This happens where the fun is used.
newFun :: [SmtType] -> SmtType -> M Ident
newFun ts t = M $ sets $ \s -> let n = names s
                                   i = fromString ("x" ++ show n)
                               in ( i
                                  , s { names = n + 1
                                      , globalDefs = (i,ts,t) : globalDefs s
                                      }
                                  )

newPred :: [SmtType] -> M Ident
newPred ts = M $ sets $ \s -> let n = names s
                                  p = fromString ("p" ++ show n)
                              in ( p
                                 , s { names = n + 1
                                     , globalPreds = (p,ts) : globalPreds s
                                     }
                                 )

newConst :: SmtType -> M BV.Term
newConst ty =
  do f <- newFun [] ty
     padArray ty (App f [])




-- Give an explicit name to a term.
-- This is useful so that we can share term representations.
-- Note that this assumes that we don't use "lets" in formulas,
-- (which we don't) because otherwise lifting things to the top-level
-- might result in undefined local variables.
saveT :: SmtType -> BV.Term -> M BV.Term
saveT ty t =
  do ext <- useExtArrays
     let saveArr = case ty of
                     TArray {} -> ext
                     _ -> True
     case t of
       App _ (_ : _) | saveArr -> doSave
       ITE _ _ _     | saveArr -> doSave
       _ -> return t

  where doSave = do x <- newConst ty
                    addAssumpt (x === t)
                    addNote $ pp x <> char ':' <+> pp t
                    return x

saveF :: Formula -> M Formula
saveF f =
  case f of
    FPred {}  -> return f
    FTrue     -> return f
    FFalse    -> return f

    FVar {}   -> bad "formula variables"
    Let {}    -> bad "term let"
    FLet {}   -> bad "formula let"

    _         -> do p <- newPred []
                    let f1 = FPred p []
                    addAssumpt (Conn Iff [f1,f])
                    addNote $ pp f1 <> char ':' <+> pp f
                    return f1

  where bad x = bug "saveF" ("We do not support " ++ x)

save :: FTerm -> M FTerm
save t = do t1 <- saveT (smtType t) (asTerm t)
            f1 <- case asForm t of
                    Nothing -> return Nothing
                    Just f  -> Just `fmap` saveF f
            return t { asTerm = t1, asForm = f1 }

-- For now, we work only with staticlly known sizes of things.
wToI :: WidthExpr -> M Integer
wToI x = case widthConstant x of
           Just y -> return (numBits y)
           _      -> err "Non-constnat width"



--------------------------------------------------------------------------------
-- Converting from HAJava types into SMTLIB types.

-- | The array type contains the number of elements, not the
-- the number of bits, as in SMTLIB!
data SmtType = TBool
             | TArray Integer Integer
             | TBitVec Integer
             | TRecord [FldInfo]
               deriving (Eq,Ord,Show)

type Offset = Integer
type Width  = Integer

data FldInfo = FldInfo { fiName  :: String
                       , fiOff   :: Offset
                       , fiWidth :: Width
                       , _fiType :: SmtType
                       }
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

    SymRec srd ts ->
      -- NB: To see an alternate implementation strategy for record support, see
      -- commit hash ddebf9a8fffbeb5f8cffcead901350197eea03a8.  It defines
      -- explicit record constructor/selector functions and sorts, but we
      -- abandoned that strategy when we realized that the necessary
      -- quantification over records/record components in defining some of these
      -- functions was problematic w.r.t. our use of QF_AUFBV :(.  At some point
      -- in the future, we may wish to resurrect the initial approach.

      do ftys <- mapM cvtType $ V.toList (recFieldTypes srd ts)
         let fields = V.toList (recDefFieldNames srd) `zip` ftys
         return . TRecord . reverse . fst . foldl mkFldInfo ([], 0) $ fields

    SymShapeVar _ -> err "Type variable"

mkFldInfo :: ([FldInfo], Offset) -> (String, SmtType) -> ([FldInfo], Offset)
mkFldInfo (acc, accOff) (fldNm, fldTy) = (fi : acc, accOff + fiWidth fi)
  where
    w        = width fldTy
    fi       = FldInfo fldNm accOff w fldTy
    width ty = case ty of
                 TBool      -> 1
                 TArray x y -> x * y
                 TBitVec x  -> x
                 TRecord{}  -> width (flattenRecTy ty)

flattenRecTy :: SmtType -> SmtType
flattenRecTy (TRecord fis) = TBitVec $ sum (map fiWidth fis)
flattenRecTy ty            = ty

toSort :: SmtType -> Sort
toSort ty =
  case ty of
    TBool        -> tBitVec 1
    TArray x y   -> tArray (needBits x) y
    TBitVec n    -> tBitVec n
    x@TRecord{}  -> toSort $ flattenRecTy x

-- How many bits do we need to represent the given number.
needBits :: Integer -> Integer
needBits n | n <= 0     = 0
needBits n | odd n      = 1 + needBits (div n 2)
           | otherwise  = needBits (n + 1)



--------------------------------------------------------------------------------
{- We have two ways of translating boolean expressions: as a formula, or
as a term.  The type 'FTerm' allows us to do both.  Values of this type
should satisfy the following invariant:

fterm_prop f = case asForm f of
                 Just _  -> smtType f == TBool
                 Nothing -> smtType f /= TBool
-}


data FTerm = FTerm { asForm   :: Maybe Formula
                   , asTerm   :: BV.Term
                   , smtType  :: SmtType      -- Type of the term
                   } deriving (Show)

instance Eq FTerm  where x == y = asTerm x == asTerm y
instance Ord FTerm where compare x y = compare (asTerm x) (asTerm y)

-- This is useful for expressions that are naturally expressed as formulas.
toTerm :: Formula -> FTerm
toTerm f = FTerm { asForm = Just f
                 , asTerm = ITE f bit1 bit0
                 , smtType = TBool
                 }

fromTerm :: SmtType -> BV.Term -> FTerm
fromTerm ty t = FTerm { asForm = case ty of
                                   TBool -> Just (t === bit1)
                                   _     -> Nothing
                      , asTerm = t
                      , smtType = ty
                      }

--------------------------------------------------------------------------------




translateOps :: S.Set OpIndex -> TermSemantics M FTerm
translateOps enabled = termSem
  where
  termSem = TermSemantics
    { tsConstant = \c t -> mkConst c =<< cvtType t
    , tsApplyUnary = apply1
    , tsApplyBinary = apply2
    , tsApplyTernary = apply3
    , tsApplyOp = apply
    }


  -- Spot when things are obviously the same.
  same x y = case (asTerm x, asTerm y) of
               (App c [], App d []) -> c == d
               (Lit c, Lit d)       -> c == d
               _                    -> False


  apply1 op = lift1 $
    case opDefIndex (opDef op) of
      Op.Trunc w     -> truncOp (numBits w)
      Op.SignedExt w -> signedExtOp (numBits w)
      Op.Not         -> bNotOp
      Op.INot        -> iNotOp
      Op.Neg         -> negOp
      Op.Split w1 w2 -> splitOp (numBits w1) (numBits w2)
      Op.Join w1 w2  -> joinOp (numBits w1) (numBits w2)
      Op.Dynamic x   -> \t -> dynOp x op (V.fromList [t])

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
      Op.Dynamic x     -> \s t -> dynOp x op (V.fromList [s,t])

      i -> \_ _ -> err $ "Unknown binary operator: " ++ show i
                    ++ " (" ++ opDefName (opDef op) ++ ")"

  apply3 op = lift3 $
    case opDefIndex (opDef op) of
      Op.ITE           -> iteOp
      Op.SetArrayValue -> setArrayValueOp
      Op.Dynamic x     -> \r s t -> dynOp x op (V.fromList [r,s,t])

      i -> \_ _ _ -> err $ "Unknown ternary operator: " ++ show i
                    ++ " (" ++ opDefName (opDef op) ++ ")"

  apply op = liftV $
    case opDefIndex (opDef op) of
      Op.MkArray _  -> \vs -> do t <- cvtType (opResultType op)
                                 mkArray t vs
      Op.Dynamic x  -> \xs -> dynOp x op (V.fromList xs)

      i -> \_ -> err $ "Unknown variable arity operator: " ++ show i
                    ++ " (" ++ opDefName (opDef op) ++ ")"


  lift1 op t        = do s1 <- save =<< t
                         op s1

  lift2 op t1 t2    = do s1 <- save =<< t1
                         s2 <- save =<< t2
                         op s1 s2

  lift3 op t1 t2 t3 = do s1 <- save =<< t1
                         s2 <- save =<< t2
                         s3 <- save =<< t3
                         op s1 s2 s3

  liftV op ts       = do ss <- mapM (save =<<) (V.toList ts)
                         op ss


  dynOp x op args =
    case opDefDefinition (opDef op) of
      Just rhs | opDefIndex (opDef op) `S.member` enabled ->
        do as0 <- V.mapM save args
           mb <- lkpDefinedOp x as0
           case mb of
             Just t -> return t
             Nothing ->
               do let inputFn i _ = return (as0 V.! i)
                  t <- save =<< evalDagTerm inputFn termSem rhs
                  addDefinedOp x (opDefName (opDef op)) as0 t
                  return t

      _ ->
        do as  <- mapM cvtType (V.toList (opArgTypes op))
           rty <- cvtType (opResultType op)

           let name       = opDefName (opDef op)
               rslt t     = fromTerm rty `fmap` padArray rty t
               args'      = map asTerm (V.toList args)
               isCtor nms = name == "{ " ++ L.intercalate ", " nms ++ " }"
               isSel fis  = length args' == 1 && name `elem` map fiName fis

           case (rty, as) of
             -- Record constructor
             (TRecord fis, _) | isCtor (map fiName fis) ->
               rslt (foldr1 BV.concat (reverse args'))

             -- Record selector
             (_, [TRecord fis]) | isSel fis ->
                 case L.find (\FldInfo{ fiName = n } -> n == name) fis of
                   Nothing -> bug "dynOp" "Failed to find FldInfo as expected"
                   Just fi -> case args' of
                     [t] -> rslt $ BV.extract (fiOff fi + fiWidth fi - 1)
                                     (fiOff fi) t
                     _   -> bug "dynOp" "Expected single op arg"

             -- Uninterpreted ops
             _ ->
               do known <- getExtraOps
                  f     <- case IM.lookup x known of
                    Just (f,_) -> return f -- Already generated?
                    Nothing    -> -- Generate a function for this op
                      do f <- newFun as rty
                         M $ sets $ \s ->
                           (f, s{ extraAbs = IM.insert x (f,name)
                                               (extraAbs s) })
                  rslt $ App f (map asTerm (V.toList args))

--------------------------------------------------------------------------------
-- Operations

mkConst :: CValue -> SmtType -> M FTerm
mkConst val t =
  case val of
    _ | Just (w,v) <- getSValW val ->
      return FTerm { asForm  = Nothing
                   , asTerm  = bv v (numBits w)
                   , smtType = t
                   }
    CBool b ->
      return FTerm { asForm  = Just (if b then FTrue else FFalse)
                   , asTerm  = if b then bit1 else bit0
                   , smtType = t
                   }

    CArray vs ->
      case t of
        TArray _ w ->
          do xs <- mapM (`mkConst` TBitVec w) (V.toList vs)
             mkArray t xs

        _ -> bug "mkConst" "Type error---array constant of non-array type."

    CRec _ _ vs ->
      case t of
        TRecord fs ->
          do let ts = map (TBitVec . fiWidth) fs
             xs <- mapM (uncurry mkConst) (zip (V.toList vs) ts)
             return FTerm { asForm = Nothing
                          , asTerm = foldr1 BV.concat (map asTerm xs)
                          , smtType = t
                          }
        _ -> bug "mkConst" "Type error---record constant of non-record type."

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
                   [ 0 .. ] (map asTerm xs)
         return FTerm { asForm = Nothing
                      , asTerm = a
                      , smtType = t
                      }

    _ -> bug "mkArray" "Type error---the type of mkArray is not an array."


eqOp :: FTerm -> FTerm -> M FTerm
eqOp t1 t2 =
  do ext <- useExtArrays
     if ext
       then return $ toTerm (asTerm t1 === asTerm t2)

       -- If we are aiming for a theory that does not support array
       -- extensionality (i.e. arrays are equal if their elems are equal),
       -- then we simulate the behavior by comparing each array element
       -- separately.  For large arrays, this can become big, of course.
       else case smtType t1 of
              TArray n v
                | n == 0  -> return FTerm { asForm  = Just FTrue
                                          , asTerm  = bit1
                                          , smtType = TBool
                                          }
                | otherwise ->
                    do a <- asTerm `fmap` save t1
                       b <- asTerm `fmap` save t2
                       let w        = needBits n
                           el arr i = fromTerm (TBitVec v) (select arr (bv i w))
                           rng      = [ 0 .. n - 1 ]
                           conj m1 m2 = do f1 <- m1
                                           f2 <- m2
                                           bAndOp f1 f2
                       foldr1 conj (zipWith eqOp [ el a i | i <- rng ]
                                                 [ el b i | i <- rng ])

              _ -> return $ toTerm (asTerm t1 === asTerm t2)




iteOp :: FTerm -> FTerm -> FTerm -> M FTerm
iteOp t1 t2 t3 =
  case asForm t1 of
    Just b ->
      return FTerm { asForm = do s2 <- asForm t2
                                 s3 <- asForm t3
                                 return (Conn IfThenElse [b,s2,s3])
                   , asTerm = ITE b (asTerm t2) (asTerm t3)
                   , smtType = smtType t2   -- or t3
                   }
    Nothing -> bug "iteOpt"
                   "Type error---discriminator of ITE is not a formula."



truncOp :: Integer -> FTerm -> M FTerm
truncOp n t =
  return FTerm { asForm = Nothing
               , asTerm = if n == 0 then bv 0 0
                                    else extract (n-1) 0 (asTerm t)
               , smtType = TBitVec n
               }


signedExtOp :: Integer -> FTerm -> M FTerm
signedExtOp n t =
  case smtType t of
    TBitVec m | m < n  -> return FTerm { asForm = Nothing
                                       , asTerm = sign_extend (n - m) (asTerm t)
                                       , smtType = TBitVec n
                                       }
              | m == n -> return t
    _ -> bug "signedExtOp"
             "Sign extending to a smaller value, or type error."


bNotOp :: FTerm -> M FTerm
bNotOp t = return FTerm { asForm = do a <- asForm t
                                      return (Conn Not [a])
                        , asTerm = bvnot (asTerm t)
                        , smtType = TBool
                        }


bAndOp :: FTerm -> FTerm -> M FTerm
bAndOp s t = return FTerm { asForm = do a <- asForm s
                                        b <- asForm t
                                        return (Conn And [a,b])

                          , asTerm = bvand (asTerm s) (asTerm t)
                          , smtType = TBool
                          }


bOrOp :: FTerm -> FTerm -> M FTerm
bOrOp s t  = return FTerm { asForm = do a <- asForm s
                                        b <- asForm t
                                        return (Conn Or [a,b])

                          , asTerm = bvor (asTerm s) (asTerm t)
                          , smtType = TBool
                          }


bXorOp :: FTerm -> FTerm -> M FTerm
bXorOp s t  = return FTerm { asForm = do a <- asForm s
                                         b <- asForm t
                                         return (Conn Xor [a,b])

                          , asTerm = bvxor (asTerm s) (asTerm t)
                          , smtType = TBool
                          }


bImpliesOp :: FTerm -> FTerm -> M FTerm
bImpliesOp s t  =
  case asForm s of
    Just f -> return FTerm { asForm = do b <- asForm t
                                         return (Conn Implies [f,b])

                           , asTerm = ITE f (asTerm t) bit0
                           , smtType = TBool
                           }
    Nothing -> bug "bImpliesOp" "Missing formula translation."


iNotOp :: FTerm -> M FTerm
iNotOp t = return FTerm { asForm  = Nothing
                        , asTerm  = bvnot (asTerm t)
                        , smtType = smtType t
                        }


iAndOp :: FTerm -> FTerm -> M FTerm
iAndOp s t  = return FTerm { asForm  = Nothing
                           , asTerm  = bvand (asTerm s) (asTerm t)
                           , smtType = smtType s
                           }

iOrOp :: FTerm -> FTerm -> M FTerm
iOrOp s t   = return FTerm { asForm  = Nothing
                           , asTerm  = bvor (asTerm s) (asTerm t)
                           , smtType = smtType s
                           }

iXorOp :: FTerm -> FTerm -> M FTerm
iXorOp s t   = return FTerm { asForm  = Nothing
                            , asTerm  = bvxor (asTerm s) (asTerm t)
                            , smtType = smtType s
                            }


mkShiftOp :: (BV.Term -> BV.Term -> BV.Term) -> FTerm -> FTerm -> M FTerm
mkShiftOp f s t =
  case smtType s of
    TBitVec n ->
      do t1 <- coerce n t
         return FTerm { asForm  = Nothing
                      , asTerm  = f (asTerm s) (asTerm t1)
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
      return FTerm { asForm  = Nothing
                   , asTerm  = BV.concat (asTerm t) (asTerm s)
                   , smtType = TBitVec (m + n)
                   }
    _ -> bug "appendOp" "Type error---arguments are not bit vectors"


addOp :: FTerm -> FTerm -> M FTerm
addOp s t = return FTerm { asForm  = Nothing
                         , asTerm  = bvadd (asTerm s) (asTerm t)
                         , smtType = smtType s
                         }


mulOp :: FTerm -> FTerm -> M FTerm
mulOp s t = return FTerm { asForm  = Nothing
                         , asTerm  = bvmul (asTerm s) (asTerm t)
                         , smtType = smtType s
                         }


subOp :: FTerm -> FTerm -> M FTerm
subOp s t = return FTerm { asForm  = Nothing
                         , asTerm  = bvsub (asTerm s) (asTerm t)
                         , smtType = smtType s
                         }


negOp :: FTerm -> M FTerm
negOp s = return FTerm { asForm  = Nothing
                       , asTerm  = bvneg (asTerm s)
                       , smtType = smtType s
                       }



signedDivOp :: FTerm -> FTerm -> M FTerm
signedDivOp s t = return FTerm { asForm  = Nothing
                               , asTerm  = bvsdiv (asTerm s) (asTerm t)
                               , smtType = smtType s
                               }

signedRemOp :: FTerm -> FTerm -> M FTerm
signedRemOp s t = return FTerm { asForm  = Nothing
                               , asTerm  = bvsdiv (asTerm s) (asTerm t)
                               , smtType = smtType s
                               }




unsignedDivOp :: FTerm -> FTerm -> M FTerm
unsignedDivOp s t = return FTerm { asForm  = Nothing
                                 , asTerm  = bvudiv (asTerm s) (asTerm t)
                                 , smtType = smtType s
                                 }

unsignedRemOp :: FTerm -> FTerm -> M FTerm
unsignedRemOp s t = return FTerm { asForm  = Nothing
                                 , asTerm  = bvurem (asTerm s) (asTerm t)
                                 , smtType = smtType s
                                 }


signedLeqOp :: FTerm -> FTerm -> M FTerm
signedLeqOp s t = return $ toTerm $ bvsle (asTerm s) (asTerm t)

signedLtOp :: FTerm -> FTerm -> M FTerm
signedLtOp s t = return $ toTerm $ bvslt (asTerm s) (asTerm t)

unsignedLeqOp :: FTerm -> FTerm -> M FTerm
unsignedLeqOp s t = return $ toTerm $ bvule (asTerm s) (asTerm t)

unsignedLtOp :: FTerm -> FTerm -> M FTerm
unsignedLtOp s t = return $ toTerm $ bvugt (asTerm t) (asTerm s)


coerce :: Integer -> FTerm -> M FTerm
coerce w t =
  case smtType t of
    TBitVec n | n == w    -> return t
              | n > w     -> truncOp w t
              | otherwise -> return t {asTerm = zero_extend (w - n) (asTerm t)}
    _ -> bug "coerce" "Type error---coercing a non-bit vector"

getArrayValueOp :: FTerm -> FTerm -> M FTerm
getArrayValueOp a i =
  case smtType a of
    TArray w n ->
      do j <- coerce (needBits w) i
         return FTerm { asForm  = Nothing
                      , asTerm  = select (asTerm a) (asTerm j)
                      , smtType = TBitVec n
                      }
    _ -> bug "getArrayValueOp" "Type error---selecting from a non-array."


setArrayValueOp :: FTerm -> FTerm -> FTerm -> M FTerm
setArrayValueOp a i v =
  case smtType a of
    ty@(TArray w _) ->
      do ext <- useExtArrays
         j <- coerce (needBits w) i
         new <- if ext then do old <- saveT ty (asTerm a)
                               new <- saveT ty (store old (asTerm j) (asTerm v))
                               oi  <- toVar old
                               ni  <- toVar new
                               addArrayUpdate oi ni
                               return new
                       -- we can't save things without assuming equality
                       -- between arrays.
                       else return (store (asTerm a) (asTerm j) (asTerm v))
         return FTerm { asForm  = Nothing
                      , asTerm  = new
                      , smtType = smtType a
                      }
    _ -> bug "setArrayValueOp" "Type error---updating a non-array."


splitOp :: Integer -> Integer -> FTerm -> M FTerm
splitOp l w t0 =
  do  t <- save t0
      let vs = [ FTerm { asForm  = Nothing
                       , asTerm  = extract ((i+1) * w - 1) (i * w) (asTerm t)
                       , smtType = TBitVec w
                       } | i <- [ 0 .. l - 1 ] ]
      mkArray (TArray l w) vs



joinOp :: Integer -> Integer -> FTerm -> M FTerm
joinOp 0 _ _ = return FTerm { asForm = Nothing
                            , asTerm = bv 0 0
                            , smtType = TBitVec 0
                            }
joinOp l w t0 =
  do t <- save t0
     let n = needBits l
     return FTerm
       { asForm = Nothing
       , asTerm = foldr1 (flip BV.concat)
                     [ select (asTerm t) (bv i n) | i <- [ 0 .. l - 1 ] ]
       , smtType = TBitVec (l * w)
       }
