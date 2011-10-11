{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SAWScript.SmtLib where

import GHC.Exts(IsString(fromString))
import SMTLib1.QF_AUFBV as BV
import MonadLib hiding (lift)
import Verinf.Symbolic.Common
  ( CValue(..), Op(..), OpDef(..), TermSemantics(..)
  , WidthExpr, widthConstant
  , DagType(..)
  , opResultType, numBits
  )
import qualified Verinf.Symbolic.Common as Op (OpIndex(..))
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- The Monad

newtype M a = M (StateT S (ExceptionT X Id) a) deriving (Functor, Monad)

data S = S { names       :: !Int
           , globalDefs  :: [(Ident,[SmtType],SmtType)]
           , globalAsmps :: [Formula]
           }

type X = String



toScript :: String -> M Formula -> Either X Script
toScript n (M m) =
  case runId $ runExceptionT $ runStateT s0 m of
    Left xx -> Left xx
    Right (a,s) -> Right Script
      { scrName     = fromString n
      , scrCommands =
        [ CmdLogic (fromString "QF_AUFBV")
        , CmdExtraFuns [ FunDecl { funName = i
                                 , funArgs = map toSort as
                                 , funRes  = toSort b
                                 , funAnnots = [] } | (i,as,b) <- globalDefs s ]
        ] ++
        [ CmdAssumption f | f <- globalAsmps s
        ] ++
        [ CmdFormula a
        ]
      }

    where s0 = S { names = 0, globalDefs = [], globalAsmps = [] }


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

addAssumpt :: Formula -> M ()
addAssumpt f = M $ sets_ $ \s -> s { globalAsmps = f : globalAsmps s }

newFun :: [SmtType] -> SmtType -> M BV.Term
newFun ts t = M $ sets $
  \s -> let n = names s
            i = fromString ("x" ++ show n)
        in ( App i []
           , s { names = n + 1
               , globalDefs = (i,ts,t) : globalDefs s
               }
           )

newConst :: SmtType -> M BV.Term
newConst = newFun []

-- Give an explicit name to a term.
-- This is useful so that we can share term representations.
-- Note that this assumes that we don't use "lets" in formulas,
-- (which we don't) because otherwise lifting things to the top-level
-- might result in undefined local variables.
save :: FTerm -> M FTerm
save t =
  case asTerm t of
    App _ (_ : _) -> do x <- newConst (smtType t)
                        addAssumpt (x === asTerm t)
                        return t { asTerm = x }
    _             -> return t

-- How many bits do we need to represent the given number.
needBits :: Integer -> Integer
needBits n | n <= 0     = 0
needBits n | odd n      = 1 + needBits (div n 2)
           | otherwise  = needBits (n + 1)

-- For now, we work only with staticlly known sizes of things.
wToI :: WidthExpr -> M Integer
wToI x = case widthConstant x of
           Just y -> return (numBits y)
           _      -> err "Non-constnat width"



--------------------------------------------------------------------------------
-- Converting from HAJava types into SMTLIB types.

-- XXX: In the array case, and integer tracking the actual number of
-- elements that we have.
data SmtType = TBool | TArray Integer Integer | TBitVec Integer
               deriving (Eq,Show)

cvtType :: DagType -> M SmtType
cvtType ty =
  case ty of

    SymBool  -> return TBool

    SymInt w -> TBitVec `fmap` wToI w

    SymArray w ty1 ->
      do n <- needBits `fmap` wToI w
         s <- cvtType ty1
         case s of
           TBitVec m -> return (TArray n m)
           -- We might be able to support mult-dimensional arrays
           -- by concatenating their index bits
           -- Example:  Array[2+4:8]    is [4][16][8]
           _         -> err "Complex nested array"

    SymRec _ _    -> err "Record"

    SymShapeVar _ -> err "Type variable"

toSort :: SmtType -> Sort
toSort ty =
  case ty of
    TBool       -> tBool
    TArray x y  -> tArray x y
    TBitVec n   -> tBitVec n


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
                   }

-- This is useful for expressions that are naturally expressed as formulas.
toTerm :: Formula -> FTerm
toTerm f = FTerm { asForm = Just f
                 , asTerm = ITE f true false
                 , smtType = TBool
                 }


--------------------------------------------------------------------------------

translateOps :: TermSemantics M FTerm
translateOps = TermSemantics
  { tsEqTerm = same
  , tsConstant = \c t -> mkConst c =<< cvtType t
  , tsApplyUnary = apply1
  , tsApplyBinary = apply2
  , tsApplyTernary = apply3
  , tsApplyOp = apply
  }

  where

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

      i -> \_ -> err $ "Unknown unary operator: " ++ show i

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

      i -> \_ _ -> err $ "Unknown binary operator: " ++ show i

  apply3 op = lift3 $
    case opDefIndex (opDef op) of
      Op.ITE           -> iteOp
      Op.SetArrayValue -> setArrayValueOp

      i -> \_ _ _ -> err $ "Unknown ternary operator: " ++ show i

  apply op = lift $
    case opDefIndex (opDef op) of
      Op.MkArray _  -> \vs -> do t <- cvtType (opResultType op)
                                 mkArray t vs

      i -> \_ -> err $ "Unknown variable arity operator: " ++ show i


  lift1 op t        = do s1 <- save t
                         op s1

  lift2 op t1 t2    = do s1 <- save t1
                         s2 <- save t2
                         op s1 s2

  lift3 op t1 t2 t3 = do s1 <- save t1
                         s2 <- save t2
                         s3 <- save t3
                         op s1 s2 s3

  lift op ts        = do ss <- mapM save (V.toList ts)
                         op ss




--------------------------------------------------------------------------------
-- Operations





mkConst :: CValue -> SmtType -> M FTerm
mkConst val t =
  case val of

    CBool b ->
      return FTerm { asForm  = Just (if b then FTrue else FFalse)
                   , asTerm  = if b then true else false
                   , smtType = t
                   }

    CInt w v ->
      return FTerm { asForm  = Nothing
                   , asTerm  = bv v (numBits w)
                   , smtType = t
                   }

    CArray vs ->
      case t of
        TArray _ w ->
          do xs <- mapM (`mkConst` TBitVec w) (V.toList vs)
             mkArray t xs

        _ -> bug "mkConst" "Type error---array constant of non-array type."

    CRec {} -> err "mkConst does not support records at the moment"


mkArray :: SmtType -> [FTerm] -> M FTerm
mkArray t xs =
  do a  <- newConst t
     zipWithM_ (\i x ->
        addAssumpt (select a (fromInteger i) === asTerm x)) [ 0 .. ] xs
     return FTerm { asForm = Nothing
                  , asTerm = a
                  , smtType = t
                  }



eqOp :: FTerm -> FTerm -> M FTerm
eqOp t1 t2 = return $ toTerm (asTerm t1 === asTerm t2)



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
                        , asTerm = BV.not (asTerm t)
                        , smtType = TBool
                        }


bAndOp :: FTerm -> FTerm -> M FTerm
bAndOp s t = return FTerm { asForm = do a <- asForm s
                                        b <- asForm t
                                        return (Conn And [a,b])

                          , asTerm = BV.and (asTerm s) (asTerm t)
                          , smtType = TBool
                          }


bOrOp :: FTerm -> FTerm -> M FTerm
bOrOp s t  = return FTerm { asForm = do a <- asForm s
                                        b <- asForm t
                                        return (Conn Or [a,b])

                          , asTerm = BV.or (asTerm s) (asTerm t)
                          , smtType = TBool
                          }


bXorOp :: FTerm -> FTerm -> M FTerm
bXorOp s t  = return FTerm { asForm = do a <- asForm s
                                         b <- asForm t
                                         return (Conn Xor [a,b])

                          , asTerm = BV.xor (asTerm s) (asTerm t)
                          , smtType = TBool
                          }


bImpliesOp :: FTerm -> FTerm -> M FTerm
bImpliesOp s t  = return FTerm { asForm = do a <- asForm s
                                             b <- asForm t
                                             return (Conn Implies [a,b])

                               , asTerm = BV.implies (asTerm s) (asTerm t)
                               , smtType = TBool
                               }


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


shlOp :: FTerm -> FTerm -> M FTerm
shlOp s t    = return FTerm { asForm  = Nothing
                            , asTerm  = bvshl (asTerm s) (asTerm t)
                            , smtType = smtType s
                            }



shrOp :: FTerm -> FTerm -> M FTerm
shrOp s t    = return FTerm { asForm  = Nothing
                            , asTerm  = bvashr (asTerm s) (asTerm t)
                            , smtType = smtType s
                            }

ushrOp :: FTerm -> FTerm -> M FTerm
ushrOp s t    = return FTerm { asForm  = Nothing
                             , asTerm  = bvshr (asTerm s) (asTerm t)
                             , smtType = smtType s
                             }

appendOp :: FTerm -> FTerm -> M FTerm
appendOp s t    =
  case (smtType s, smtType t) of
    (TBitVec m, TBitVec n) ->
      return FTerm { asForm  = Nothing
                   , asTerm  = BV.concat (asTerm s) (asTerm t)
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
                                 , asTerm  = bvdiv (asTerm s) (asTerm t)
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


coerceArrayIx :: Integer -> FTerm -> M FTerm
coerceArrayIx w t =
  case smtType t of
    TBitVec n | n == w    -> return t
              | n > w     -> truncOp w t   -- XXX: Or, report an error?
              | otherwise -> return t {asTerm = zero_extend (w - n) (asTerm t)}
    _ -> bug "coerceArrayIx" "Type error---array index is not a bit-vector."

getArrayValueOp :: FTerm -> FTerm -> M FTerm
getArrayValueOp a i =
  case smtType a of
    TArray w n ->
      do j <- coerceArrayIx w i
         return FTerm { asForm  = Nothing
                      , asTerm  = select (asTerm a) (asTerm j)
                      , smtType = TBitVec n
                      }
    _ -> bug "getArrayValueOp" "Type error---selecting from a non-array."


setArrayValueOp :: FTerm -> FTerm -> FTerm -> M FTerm
setArrayValueOp a i v =
  case smtType a of
    TArray w _ ->
      do j <- coerceArrayIx w i
         return FTerm { asForm  = Nothing
                      , asTerm  = store (asTerm a) (asTerm j) (asTerm v)
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
      mkArray (TArray (needBits l) w) vs



joinOp :: Integer -> Integer -> FTerm -> M FTerm
joinOp 0 _ _ = return FTerm { asForm = Nothing
                            , asTerm = bv 0 0
                            , smtType = TBitVec 0
                            }
joinOp l w t0 =
  do t <- save t0
     return FTerm
       { asForm = Nothing
       , asTerm = foldr1 BV.concat
                     [ select (asTerm t) (fromInteger i) | i <- [ 0 .. l - 1 ] ]
       , smtType = TBitVec (l * w)
       }


