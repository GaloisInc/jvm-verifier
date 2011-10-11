{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
module SAWScript.SmtLib where

import GHC.Exts(IsString(fromString))
import SMTLib1.QF_AUFBV as BV
import MonadLib
import Verinf.Symbolic hiding (mkConst, OpIndex(..))
import qualified Verinf.Symbolic as Op
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


bug :: String -> M a
bug x = M $ raise $ unlines [ "Internal error."
                            , "*** detected in: SAWScript.SmtLib"
                            , "*** " ++ x
                            ]


err :: String -> M a
err x = M $ raise x

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
save t | App _ [] <- asTerm t = return t    -- no need to keep renaming things.
save t =
  do x <- newConst (smtType t)
     addAssumpt (x === asTerm t)
     return t { asTerm = x }

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

        _ -> bug "Type error: array constant of non-array type"

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
    Nothing -> bug "type error: discriminator of ITE not a formula?"



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
    _ -> bug "sign extending to a smaller value, or type error"


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
    _ -> bug "appendOp: type error, arguments are not bit vectors"


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


getArrayValueOp :: FTerm -> FTerm -> M FTerm
getArrayValueOp a i =
  case smtType a of
    TArray _ n -> return FTerm { asForm  = Nothing
                               , asTerm  = select (asTerm a) (asTerm i)
                               , smtType = TBitVec n
                               }
    _ -> bug "Type error, selecting from a non-array"


setArrayValueOp :: FTerm -> FTerm -> FTerm -> M FTerm
setArrayValueOp a i v =
  return FTerm { asForm  = Nothing
               , asTerm  = store (asTerm a) (asTerm i) (asTerm v)
               , smtType = smtType a
               }


splitOp :: Integer -> FTerm -> M FTerm
splitOp m t0 =
  case smtType t0 of
    TBitVec n | 0 < m, m <= n, (w,0) <- divMod n m ->
      do  t <- save t0
          let vs = [ FTerm { asForm = Nothing
                           , asTerm = extract ((i+1) * w - 1) (i * w) (asTerm t)
                           , smtType = TBitVec w
                           } | i <- [ 0 .. m - 1 ] ]
          mkArray (TArray (needBits m) w) vs
    _ -> bug "Type error, splitOp applied on a non-bit vector"


{-
joinOp :: FTerm -> M FTerm
joinOp t =
  case smtType t of
-}


