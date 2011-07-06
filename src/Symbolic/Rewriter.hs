{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
module Symbolic.Rewriter (
  -- * TermCtor
    TermCtor
  , evalTerm
  , nodeToTermCtor
  -- * Terms
  , Term(..)
  , mkConst
  , mkVar
  , appTerm
  -- * Rules
  , Rule(..)
  , mkRule
  , mkRuleFromCtor
  -- * RewriteProgram
  , RewriteProgram
  , emptyProgram
  , addRule
  , addOpDefRule
  -- * Rewriting
  , Rewriter
  , rewriterProgram
  , mkRewriter
  , reduce
  ) where

-- Module imports {{{1

import Control.Exception
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import Data.List(sort, union, intercalate)
import qualified Data.Vector as V
import Text.PrettyPrint.HughesPJ

import Symbolic.Common
import Symbolic.Dag
import qualified Symbolic.UnionFind as UF

-- TermCtor {{{1
type Var = String

data TermCtor
   = TApp !AppIndex !OpDef !TypeSubst !(Vector TermCtor)
   | TConst !CValue DagType
   | TVar Var DagType
  deriving (Eq, Ord)

instance Show TermCtor where
  show (TApp _ o _ as) = show o ++ showList (V.toList as) ""
  show (TConst c t)  = show c ++ " : " ++ ppType t
  show (TVar v t)    = v ++ " : " ++ ppType t

-- | Converts a node into a term ctor.
nodeToTermCtor :: (Node -> Maybe String)
               -> Node
               -> TermCtor
nodeToTermCtor showFn initNode = evalState (impl initNode) Map.empty
  where impl n@(InputTerm i _ tp) = return $ TVar (maybe (show i) id (showFn n)) tp
        impl (ConstTerm c tp) = return $ TConst c tp
        impl n@AppVar{} = do
          let Just i = termAppIndex n
              Just (op,sub,args) = getTypedApp n
          m <- get
          case Map.lookup i m of
            Just t -> return t
            Nothing -> do
              targs <- V.mapM impl args
              let t = TApp i op sub targs
              seq t $ do modify (Map.insert i t)
                         return t

-- Term interface {{{1
data TermState = TS {
   termAppMap :: !(Map (OpDef, TypeSubst, Vector TermCtor) TermCtor)
 , termIndex :: !AppIndex
 }

data Term = T { unTerm :: StateT TermState Identity TermCtor
              , intTermType :: DagType
              }

instance TypedTerm Term where
  termType = intTermType

-- | Parse term to get constructors.
evalTerm :: Term -> TermCtor
evalTerm (T m _) =
  let ts = TS { termAppMap = Map.empty, termIndex = 0 }
   in runIdentity $ evalStateT m ts

-- | Create a constant term.
mkConst :: CValue -> DagType -> Term
mkConst n tp = T (return $ TConst n tp) tp

-- | Create a variable term.
mkVar :: String -> DagType -> Term
mkVar n tp = T (return $ TVar n tp) tp

instance ConstantInjection Term where
  mkCBool b  = mkConst (mkCBool b) SymBool
  mkCInt w v = mkConst (mkCInt w v) (SymInt (constantWidth w))

-- | Apply op definition with given substitution to term.
appTerm :: Op -> [Term] -> Term
appTerm op args = T m (opResultType op)
  where m = do argCtors <- V.mapM unTerm (V.fromList args)
               s <- get
               let app = ((opDef op), (opSubst op), argCtors)
               case Map.lookup app (termAppMap s) of
                 Just t -> return t
                 Nothing -> do
                   let i = termIndex s
                       t = TApp i (opDef op) (opSubst op) argCtors
                   put s { termAppMap = Map.insert app t (termAppMap s)
                         , termIndex = i + 1 }
                   return t

-- Rule interface {{{1

-- | A named rule.
data Rule = Rule
  { ruleName :: String -- ^ Rule name
  , _ruleLHS :: TermCtor -- ^ Rule left-hand side.
  , _ruleRHS :: TermCtor -- ^ Rule right-hand side.
  } deriving (Show)

-- Construct a new rule with the given name, left and right hand sides.
mkRule :: String -> Term -> Term -> Rule
mkRule nm mlhs mrhs =
  case mkRuleFromCtor nm (evalTerm mlhs) (evalTerm mrhs) of
    Left msg -> error msg
    Right rl -> rl

-- Construct a new rule with the given name, left and right hand sides.
mkRuleFromCtor :: String -> TermCtor -> TermCtor -> Either String Rule
mkRuleFromCtor nm lhs rhs =
  case checkRule lhs rhs of
    Left m   -> Left $ "INTERNAL: failed to sanity check rule construction for: " ++ show nm
                      ++ "\n  LHS   : " ++ show lhs
                      ++ "\n  RHS   : " ++ show rhs
                      ++ "\n  Reason: " ++ intercalate "\n            " (lines m)
    Right () -> Right $ Rule nm lhs rhs

type Infer a = Either String a

complainIf :: Bool -> String -> Infer ()
complainIf t = when t . Left

checkRule :: TermCtor -> TermCtor -> Infer ()
checkRule lhs rhs = do
  -- lhs must be a TApp
  complainIf (not (isTApp lhs)) $ "unsupported rule with variable or constant left hand side"
  (tl, lfv) <- inferType lhs
  (tr, rfv) <- inferType rhs
  complainIf (tl /= tr) $
          "cannot unify types"
       ++ "\nLHS type: " ++ ppType tl
       ++ "\nRHS type: " ++ ppType tr
  let extras = filter (`notElem` lfv) rfv
  complainIf (not (null extras)) $
          "rhs contains more free variables than the lhs"
       ++ "\nLHS free vars: " ++ show lfv
       ++ "\nRHS free vars: " ++ show rfv
       ++ "\nExtra vars   : " ++ show extras
  where isTApp (TApp{}) = True
        isTApp _        = False

-- | Given a term, this parses the structure of the term to check that the rule
-- is correct.
inferType :: TermCtor -> Infer (DagType, [Var])
inferType trm = do (t, fs) <- go trm []
                   return (t, sort fs)
  where go (TVar   v t)  fv = return (t, [v] `union` fv)
        go (TConst _ t)  fv = return (t, fv)
        go (TApp _ o s as) fv = do
                let next (ts, fvs) c = do (ct, fvs') <- go c fvs
                                          return (ct : ts, fvs')
                (rts, fvs) <- foldM next ([], fv) (V.toList as)
                t <- inferApp o s (reverse rts)
                return (t, fvs)

inferApp :: OpDef -> TypeSubst -> [DagType] -> Infer DagType
inferApp o s ts = do
        complainIf (lts /= largTys) $
          "Operator " ++ show on ++ " requires " ++ show largTys
            ++ " argument(s), but is given " ++ show lts
        zipWithM_ (checkOpType on) [1..] argTys
        zipWithM_ (matchTys on) [1..] (zip ts (map (flip applyTypeSubst s) argTys))
        return resTy
 where on      = opDefName o
       argTys  = V.toList $ opDefArgTypes o
       resTy   = flip applyTypeSubst s $ opDefResultType o
       lts     = length ts
       largTys = length argTys

matchTys :: String -> Int -> (DagType, DagType) -> Infer ()
matchTys o ac (g, e) = complainIf (g /= e) $  "Type mismatch in argument " ++ show ac ++ " for operator " ++ o
                                           ++ "\nExpected: " ++ ppType e
                                           ++ "\nGiven   : " ++ ppType g

-- an op type can be either a constant or just one variable
checkOpType :: String -> Int -> DagType -> Infer ()
checkOpType o ac = checkT
  where checkT (SymInt we)      = checkWE we
        checkT (SymArray _ t) = checkT t
        checkT _                = return ()
        checkWE we = complainIf (not (isJust (widthConstant we) || isJust (widthVar we)))
                                $  "Non-singleton variable type in argument " ++ show ac ++ " for operator " ++ o
                                ++ "\nExpected: single variable or constant"
                                ++ "\nFound   : " ++ ppWidthExpr we ""

-- Compiled match representation {{{1

-- | Register index for storing terms.
type TermRegIdx = Int

-- | Register index for storing types
type TypeRegIdx = Int

-- | Deterministic program used for matching terms.
data MatchExpr r
  -- | Abort program without a substitution.
  = MatchAbort
  -- | Yield a substitution using current registry indices.
  | MatchYield String r !(MatchExpr r)
  -- | @MatchBind r op t f@ attempts to match term at register @r@ against
  -- application @op(t1, ..., tn)@.  If successful, @t1@ to @tn@ are stored
  -- in new registers, and @t@ is called.  Otherwise @f@ is called.
  | MatchBind !TermRegIdx !OpDef !(MatchExpr r) !(MatchExpr r)
  -- | @MatchCheck r c t f@ compares @r@ against constant @c@, calling @t@
  -- if they are equivalent and @f@ if distint.
  | MatchCheck !TermRegIdx !CValue !(MatchExpr r) !(MatchExpr r)
  -- | @MatchCompare r1 r2 t f@ compares terms at @r1@ and @r2@, calling @t@
  -- if they are equivalent and @f@ if not.
  | MatchCompare !TermRegIdx !TermRegIdx !(MatchExpr r) !(MatchExpr r)
  -- | @MatchTypeBind r t@ stores the type of the term in @r@ in a new type
  -- register and executes @t@.
  | MatchTypeBind !TermRegIdx !(MatchExpr r)
  -- | @MatchTypeInt tr t f@ checks whether the term in @tr@ is an int type:
  -- if so, it executes @t@; if not, it executes @f@.
  | MatchTypeInt !TypeRegIdx !(MatchExpr r) !(MatchExpr r)
  -- | @MatchTypeArray tr t f@ checks whether the type in @tr@ is an array:
  -- if so, it binds the length type and element type to new registry indices
  -- and executes @t@; if not, it executes @f@.
  | MatchTypeArray !TypeRegIdx !(MatchExpr r) !(MatchExpr r)
  -- | @MatchTypeRec tr recDef t f@ checks whether the type in @tr@ is an instance
  -- of the given record definition -- if so, it binds the arguments to the record
  -- parameter to new type registers and executes @t@; if not, it executes @f@.
  | MatchTypeRec !TypeRegIdx !SymRecDef !(MatchExpr r) !(MatchExpr r)
  -- | @MatchTypeCheck tr tp t f@ checks whether type at @tr@ equals @tp@:
  -- if so, it executes @t@, and if not, it executes @f@.
  -- N.B. tp should be a ground type.
  | MatchTypeCheck !TypeRegIdx !DagType !(MatchExpr r) !(MatchExpr r)
  -- | @MatchTypeCompare tr tr' t f@ checks whether the types in @tr@ and
  -- @tr'@ are equal.  It executes @t@ if so, and @f@ if not.
  | MatchTypeCompare !TypeRegIdx !TypeRegIdx !(MatchExpr r) !(MatchExpr r)

colonEquals :: Doc
colonEquals = colon <> equals

-- | Print match program
ppMatchExpr :: MatchExpr r -> Doc
ppMatchExpr MatchAbort = text "abort"
ppMatchExpr (MatchBind r op t f) =
  text "bind" <+> text (show op) <> text "(..)" <+> colonEquals <+> text (show r) $$
    nest 1 (ppMatchExpr t) $$
    nest 1 (ppMatchExpr f)
ppMatchExpr (MatchCheck r c t f) =
  text "check" <+> text (show c) <+> equals <+> text (show r) $$
    nest 1 (ppMatchExpr t) $$
    nest 1 (ppMatchExpr f)
ppMatchExpr (MatchCompare r r' t f) =
  text "compare" <+> text (show r) <+> equals <+> text (show r') $$
    nest 1 (ppMatchExpr t) $$
    nest 1 (ppMatchExpr f)
ppMatchExpr (MatchTypeBind r t) =
  text "tbind" <+> text (show r) $$
    nest 1 (ppMatchExpr t)
ppMatchExpr (MatchTypeInt tr t f) =
  text "tIsInt" <+> text (show tr) $$
    nest 1 (ppMatchExpr t) $$
    nest 1 (ppMatchExpr f)
ppMatchExpr (MatchTypeArray tr t f) =
  text "tarray" <+> colonEquals <+> text (show tr) $$
    nest 1 (ppMatchExpr t) $$
    nest 1 (ppMatchExpr f)
ppMatchExpr (MatchTypeRec tr recDef t f) =
  text "trec" <+> text (show recDef) <+> colonEquals <+> text (show tr) $$
    nest 1 (ppMatchExpr t) $$
    nest 1 (ppMatchExpr f)
ppMatchExpr (MatchTypeCheck tr tc t f) =
  text "tcheck" <+> text (show tc) <+> equals <+> text (show tr) $$
    nest 1 (ppMatchExpr t) $$
    nest 1 (ppMatchExpr f)
ppMatchExpr (MatchTypeCompare tr tr' t f) =
  text "tcompare" <+> text (show tr) <+> equals <+> text (show tr') $$
    nest 1 (ppMatchExpr t) $$
    nest 1 (ppMatchExpr f)
ppMatchExpr (MatchYield name _ f) =
  text ("yield" ++ " " ++ name) $$
    nest 1 (ppMatchExpr f)

_ppMatchExpr :: MatchExpr r -> Doc
_ppMatchExpr = ppMatchExpr

-- Match compilation {{{1

-- | Application function provides input substitutions.
type TermVarRegMap = Map Var TermRegIdx
type TypeVarRegMap = Map SymTypeVar TypeRegIdx
type YieldFn r = TermVarRegMap -> TypeVarRegMap -> r
type NamedYieldFn r = (String,YieldFn r)

-- | Represents a set of registers that are known to be equivalent.
type TermRegClass = UF.Class TermClassDesc

type TypeRegClass = UF.Class TypeClassDesc

-- | Describes facts known about a particular equivalence class of term
-- registers.
data TermRegApp
    -- | Set of ops that are not root symbol of this register.
  = MCNotOps (Set OpDef)
  | MCApp OpDef (Vector TermRegClass)

-- | Information associated with a registry class.
data TermClassDesc
  = TermClassDesc {
        termRegIdx :: TermRegIdx
      , termRegApp :: TermRegApp
      , typeRegClass :: Maybe TypeRegClass
      }

data TypeClassDesc
  = TypeClassDesc {
        typeRegIdx :: TypeRegIdx
      }

data MatchBuildState = MatchBuildState {
       -- | Matches register indices to class.
         termRegClassMap :: Map TermRegIdx TermRegClass
       -- | Index of next term register to use.
       , nextTermReg :: TermRegIdx
       -- | Union find structure for term and type classes.
       , termUnionFind :: UF.UnionFind TermClassDesc
       -- | Match type register indices to associated class.
       , typeRegClassMap :: Map TypeRegIdx TypeRegClass
       -- | Index of next type register to use.
       , nextTypeReg :: TypeRegIdx
       -- | Union find structure for term and type classes.
       , typeUnionFind :: UF.UnionFind TypeClassDesc
       }

-- Invariants:
--     (Identity)             t == t
--     (Symetry)         t == u -> u == t
--   (Transitivity) t == u /\ u == v -> t == v
--   f(t1, ..., tn) == f(u1, ..., un) -> ti == ui (i \in [1,n])
--   rep(t) = u & v \in (!= t) => v \in (!= u)
-- Noninvariants:
-- We do not propagate equalities to superterms, so it is possible to have
-- the following:
--   t1 == u1 /\ ... /\ tn == un /\ f(t1, ..., tn) != f(u1, ..., un))

type TermUFAction = UF.Action TermClassDesc
type TypeUFAction = UF.Action TypeClassDesc

type MatchBuilder a = State MatchBuildState a

evalMatchState :: MatchBuildState -> MatchBuilder a -> a
evalMatchState s m = evalState m s

modifyMatchState :: MatchBuildState -> MatchBuilder () -> MatchBuildState
modifyMatchState s m = snd $ runState m s

-- | Get register index from reg class (using eager path compression).
getTermRegIdx :: TermRegClass -> TermUFAction TermRegIdx
getTermRegIdx c = do
  TermClassDesc { termRegIdx = idx } <- UF.readClassDesc c
  return idx

-- | Get register index from reg class (using eager path compression).
getTypeRegIdx :: TypeRegClass -> TypeUFAction TypeRegIdx
getTypeRegIdx c = do
  TypeClassDesc { typeRegIdx = idx } <- UF.readClassDesc c
  return idx

termUF :: TermUFAction r -> MatchBuilder r
termUF m = do
  s <- get
  let (d,ufs') = UF.runAction (termUnionFind s) m
  put s { termUnionFind = ufs' }
  return d

typeUF :: TypeUFAction r -> MatchBuilder r
typeUF m = do
  s <- get
  let (d,ufs') = UF.runAction (typeUnionFind s) m
  put $ s { typeUnionFind = ufs' }
  return d

type TermMatchEq = (TermRegClass, TermRegClass)
type TypeMatchEq = (TypeRegClass, TypeRegClass)

-- | Merge two match classes.
mergeApp :: TermRegApp -> TermRegApp -> Maybe (TermRegApp, [TermMatchEq])
mergeApp (MCNotOps xops) (MCNotOps yops) =
  Just (MCNotOps (xops `Set.union` yops), [])
mergeApp (MCNotOps xops) yapp@(MCApp yop _yargs) = do
  if yop `Set.member` xops
   then Nothing
   else Just (yapp,[])
mergeApp x@MCApp{} y@MCNotOps{} = mergeApp y x
mergeApp x@(MCApp xOp xCl) (MCApp yOp yCl)
  | xOp == yOp && V.length xCl == V.length yCl
  = Just (x, V.toList (xCl `V.zip` yCl))
  | otherwise = Nothing

mergeTypeRegs :: [TypeMatchEq] -> TypeUFAction UF.AssertResult
mergeTypeRegs eqs = impl eqs UF.AssertRedundant
  where impl [] res = return res
        impl _ UF.AssertFailed = return UF.AssertFailed
        impl ((xc,yc):r) prevRes = do
          TypeClassDesc xi <- UF.readClassDesc xc
          TypeClassDesc yi <- UF.readClassDesc yc
          eqRes <- UF.setEqual xc yc
                   TypeClassDesc {
                       typeRegIdx = xi `min` yi
                     }
          impl r (if eqRes == UF.AssertRedundant then prevRes else eqRes)

type MergeTermResult = (UF.AssertResult,[TypeMatchEq])

-- | Merge equivalence class pairs.
mergeTermRegs :: [TermMatchEq] -> TermUFAction MergeTermResult
mergeTermRegs eqs = impl eqs (UF.AssertRedundant,[])
  where impl :: [TermMatchEq] -> MergeTermResult -> TermUFAction MergeTermResult
        impl [] res = return res
        impl ((xc1,yc1):r) (prevRes,typeEqs) = do
          xc <- UF.classRep xc1
          yc <- UF.classRep yc1
          xd <- UF.readClassDesc xc
          yd <- UF.readClassDesc yc
          case mergeApp (termRegApp xd) (termRegApp yd) of
            Nothing -> return (UF.AssertFailed,[])
            Just (zapp, newEqs) -> do
              let xtype = typeRegClass xd
              let ytype = typeRegClass yd
              let ztype = listToMaybe $ catMaybes [xtype, ytype]
              eqRes <- UF.setEqual xc yc
                         TermClassDesc {
                             termRegIdx = (termRegIdx xd) `min` (termRegIdx yd)
                           , termRegApp = zapp
                           , typeRegClass = ztype
                           }
              case eqRes of
                UF.AssertFailed -> return (UF.AssertFailed,[])
                UF.AssertRedundant -> impl r (prevRes,typeEqs)
                UF.AssertSuccess -> do
                  -- Merge types
                  case (xtype,ytype) of
                    (Just xtp, Just ytp) ->
                      impl (newEqs++r)
                           (UF.AssertSuccess,(xtp,ytp):typeEqs)
                    _ -> impl (newEqs++r) (UF.AssertSuccess,typeEqs)


mergeRegs :: TermRegClass -> TermRegClass -> MatchBuilder UF.AssertResult
mergeRegs x y = do
  (r,typeEqs) <- termUF $ mergeTermRegs [(x,y)]
  case r of
    UF.AssertFailed -> return UF.AssertFailed
    UF.AssertRedundant -> return UF.AssertRedundant
    UF.AssertSuccess -> do
      _ <- typeUF $ mergeTypeRegs typeEqs
      return UF.AssertSuccess

mkTermClass :: Int -> TermUFAction TermRegClass
mkTermClass i =
  UF.freshClass TermClassDesc { termRegApp = MCNotOps Set.empty
                              , termRegIdx = i
                              , typeRegClass = Nothing  }

-- | @assertApp c op n@ returns unused registers [r1,..., rn]@ and modifies the
-- environment so that @c@ is assumed to have the form @op(t1, ... tn)@, and
-- each @ti@ is stored in @ri$.
assertApp :: TermRegClass -> OpDef -> MatchBuilder (Vector TermRegClass)
assertApp c op = do
  s <- get
  let nIdx = nextTermReg s
      ufs = termUnionFind s
      n = opArity op
      indices = V.enumFromN nIdx n
      (args,ufs') =
         UF.runAction ufs $ do
           a <- V.mapM mkTermClass indices
           d <- UF.readClassDesc c
           UF.writeClassDesc c d { termRegApp = MCApp op a }
           return a
  -- Add new register aruments to termRegClassMap
  let newRegMap = V.foldr (uncurry Map.insert) (termRegClassMap s)
                $ indices `V.zip` args
  put s { termRegClassMap = newRegMap
        , nextTermReg = nIdx + n
        , termUnionFind = ufs' }
  return args

-- | Allocates a type register to store the type of the term register.
allocTypeClass :: MatchBuilder TypeRegClass
allocTypeClass = do
  tr <- gets nextTypeReg
  typeClass <- typeUF $ UF.freshClass TypeClassDesc { typeRegIdx = tr }
  modify $ \s -> s { typeRegClassMap = Map.insert tr typeClass (typeRegClassMap s)
                   , nextTypeReg = tr + 1 }
  return typeClass

-- | Allocates a type register to store the type of the term register.
readTypeClass :: TermRegClass -> MatchBuilder TypeRegClass
readTypeClass termClass = do
  typeClass <- allocTypeClass
  termUF $ UF.modifyClassDesc termClass $ \d -> d { typeRegClass = Just typeClass }
  return typeClass

-- | @applyMatch instr contM@ extends a match program with
-- operations from a continuation whenever matching in the
-- original program fails.
applyMatch :: MatchExpr r -- ^ Previous program
           -> (MatchBuildState -> MatchExpr r) -- ^ Continuation
           -> MatchBuildState -- ^ State
           -> MatchExpr r
applyMatch MatchAbort contM s = contM s
applyMatch instr@MatchYield{} _contM _ = instr
applyMatch (MatchBind xr op oldSucc oldFail) contM s = do
  let tcm = termRegClassMap s
      c = maybe (error $ "Bad MatchBind " ++ show xr ++ "\n" ++ show (Map.keys tcm))
                id
                (Map.lookup xr tcm)
      -- Succ case
      newSucc = applyMatch oldSucc contM $ modifyMatchState s $ do
                  let n = opArity op
                      indices = V.enumFromN (nextTermReg s) n
                  args <- termUF $ do
                    a <- V.mapM mkTermClass indices
                    UF.modifyClassDesc c $ \d -> d { termRegApp = MCApp op a }
                    return a
                  modify $ \st ->
                    st { -- Add new register aruments to termRegClassMap
                        termRegClassMap =
                          V.foldr (uncurry Map.insert)
                                  (termRegClassMap s)
                                  (indices `V.zip` args)
                      , nextTermReg = nextTermReg s + n }
      -- Fail case
      newFail = applyMatch oldFail contM $ modifyMatchState s $ termUF $ do
                  UF.modifyClassDesc c $ \d ->
                    let notOps =
                          case termRegApp d of
                            MCNotOps nOps -> assert (op `Set.notMember` nOps) $ nOps
                            MCApp _ _->
                              error $ "internal: Match same register twice in path"
                     in d { termRegApp = MCNotOps (Set.insert op notOps) }
   in MatchBind xr op newSucc newFail
-- TODO: Add constraints for MatchCheck
applyMatch (MatchCheck xr c oldSucc oldFail) contM s =
  MatchCheck xr c
             (applyMatch oldSucc contM s)
             (applyMatch oldFail contM s)
applyMatch (MatchCompare xr yr oldSucc oldFail) contM s =
  let Just xc = Map.lookup xr (termRegClassMap s)
      Just yc = Map.lookup yr (termRegClassMap s)
      newSucc = applyMatch oldSucc contM $ modifyMatchState s $ do
                  mergeRegs xc yc >> return ()
      newFail = applyMatch oldFail contM $ modifyMatchState s $ termUF $ do
                  UF.setUnequal xc yc >> return ()
   in MatchCompare xr yr newSucc newFail
applyMatch (MatchTypeBind r oldSucc) contM s =
  let Just termClass = Map.lookup r (termRegClassMap s)
      newSucc = applyMatch oldSucc contM $ modifyMatchState s $ do
                  tr <- gets nextTypeReg
                  typeClass <- typeUF $ UF.freshClass TypeClassDesc { typeRegIdx = tr }
                  modify $ \st -> st { typeRegClassMap = Map.insert tr typeClass (typeRegClassMap s)
                                   , nextTypeReg = tr + 1 }
                  termUF $ UF.modifyClassDesc termClass $ \d -> d { typeRegClass = Just typeClass }
   in MatchTypeBind r newSucc
applyMatch (MatchTypeInt tr oldSucc oldFail) contM s = do
  MatchTypeInt tr
               (applyMatch oldSucc contM s)
               (applyMatch oldFail contM s)
applyMatch (MatchTypeArray tr oldSucc oldFail) contM s = do
  let newSucc = applyMatch oldSucc contM $ modifyMatchState s $ do
                  _lenTp <- allocTypeClass
                  _eltTp <- allocTypeClass
                  return ()
      newFail = applyMatch oldFail contM s
   in MatchTypeArray tr newSucc newFail
applyMatch (MatchTypeRec tr recDef oldSucc oldFail) contM s =
  let newSucc = applyMatch oldSucc contM $ modifyMatchState s $
        replicateM_ (V.length (recDefFieldTypes recDef)) allocTypeClass
      newFail = applyMatch oldFail contM s
   in MatchTypeRec tr recDef newSucc newFail
applyMatch (MatchTypeCheck tr tp oldSucc oldFail) contM s =
  MatchTypeCheck tr tp
                 (applyMatch oldSucc contM s)
                 (applyMatch oldFail contM s)
applyMatch (MatchTypeCompare xr yr oldSucc oldFail) contM s =
  let Just xc = Map.lookup xr (typeRegClassMap s)
      Just yc = Map.lookup yr (typeRegClassMap s)
      newSucc = applyMatch oldSucc contM $ modifyMatchState s $ do
                  UF.AssertSuccess <- typeUF $ mergeTypeRegs [(xc,yc)]
                  return ()
      newFail = applyMatch oldFail contM $ modifyMatchState s $ do
                  res <- typeUF $ UF.setUnequal xc yc
                  assert (UF.assertSucceeded res) $ return ()
   in MatchTypeCompare xr yr newSucc newFail

-- | Maps node variables to associated register index
type TermClassMap = Map Var TermRegClass

-- | Maps symbolic type variables to associated type register index
type TypeClassMap = Map SymTypeVar TypeRegClass

mergeExpr :: Monad m => m (MatchExpr r)
          -> (MatchExpr r -> MatchExpr r)
          -> m (MatchExpr r)
mergeExpr m fn = do
  e <- m
  case e of
    MatchAbort -> return MatchAbort
    _ -> return $ fn e

-- | Function with yields an evalation function given mapping from variables
-- to classes.
-- The d parameter is the op descriptor.

matchType -- | Constraints between register equivalence classes and the term it
          -- should match.
          :: [(TypeRegClass, DagType)]
          -- | Maps type variables to associated registry class.
          -> TypeClassMap
          -- | Continuation to call with new type bindings when constraints
          -- succeed.
          -> (TypeClassMap -> MatchBuilder (MatchExpr r))
          -- | Match builder expression.
          -> MatchBuilder (MatchExpr r)
matchType ((reg, tp@SymBool{}):r) typeBindings contFn = do
  regIdx <- typeUF $ getTypeRegIdx reg
  mergeExpr (matchType r typeBindings contFn) $ \newSucc ->
    MatchTypeCheck regIdx tp newSucc MatchAbort
matchType ((reg, tp@(SymInt (widthConstant -> Just _))):r) typeBindings contFn = do
  regIdx <- typeUF $ getTypeRegIdx reg
  mergeExpr (matchType r typeBindings contFn) $ \newSucc -> do
    MatchTypeCheck regIdx tp newSucc MatchAbort
matchType ((reg, SymInt (widthVar -> Just v)):r) typeBindings contFn = do
  case Map.lookup (WidthVar v) typeBindings of
    Nothing -> do
      regIdx <- typeUF $ getTypeRegIdx reg
      let newTypeBindings = Map.insert (WidthVar v) reg typeBindings
      mergeExpr (matchType r newTypeBindings contFn) $ \newSucc -> do
        MatchTypeInt regIdx newSucc MatchAbort
    Just prev -> do
      regIdx <- typeUF $ getTypeRegIdx reg
      prevIdx <- typeUF $ getTypeRegIdx prev
      res <- typeUF $ mergeTypeRegs [(reg,prev)]
      case res of
        UF.AssertRedundant -> matchType r typeBindings contFn
        UF.AssertFailed -> return MatchAbort
        UF.AssertSuccess -> do
          mergeExpr (matchType r typeBindings contFn) $ \newSucc ->
            MatchTypeCompare regIdx prevIdx newSucc MatchAbort
matchType ((_reg, SymInt _):_) _ _ =
  error "Match engine requires width expressions are constants or single variables"
matchType ((reg, SymArray len eltTp):r) typeBindings contFn = do
  lenTypeClass <- allocTypeClass
  eltTypeClass <- allocTypeClass
  regIdx <- typeUF $ getTypeRegIdx reg
  let newTypes = (lenTypeClass, SymInt len):(eltTypeClass, eltTp):r
  mergeExpr (matchType newTypes typeBindings contFn) $ \newSucc -> do
    MatchTypeArray regIdx newSucc MatchAbort
matchType ((reg, SymRec recDef recSubst):r) typeBindings contFn = do
  let fieldTypes = recFieldTypes recDef recSubst
  fieldClasses <- replicateM (V.length fieldTypes) allocTypeClass
  let newR = fieldClasses `zip` V.toList fieldTypes
  regIdx <- typeUF $ getTypeRegIdx reg
  mergeExpr (matchType (newR ++ r) typeBindings contFn) $ \newSucc -> do
    MatchTypeRec regIdx recDef newSucc MatchAbort
matchType ((reg, SymShapeVar v):r) typeBindings contFn = do
  case Map.lookup (ShapeVar v) typeBindings of
    Nothing -> do
      matchType r (Map.insert (ShapeVar v) reg typeBindings) contFn
    Just prev -> do
      regIdx <- typeUF $ getTypeRegIdx reg
      prevIdx <- typeUF $ getTypeRegIdx prev
      res <- typeUF $ mergeTypeRegs [(reg,prev)]
      case res of
        UF.AssertRedundant -> matchType r typeBindings contFn
        UF.AssertFailed -> return MatchAbort
        UF.AssertSuccess -> do
          mergeExpr (matchType r typeBindings contFn) $ \newSucc ->
            MatchTypeCompare regIdx prevIdx newSucc MatchAbort
matchType [] typeRegMap contFn = contFn typeRegMap

-- | matchTerm validates that a set of term constraints are satisified by the
-- registry entries.
matchTerm :: -- | Constraints between register equivalence classes and the term it
             -- should match.
             [(TermRegClass, TermCtor)]
          -- | Maps term variables to associated registry class.
          -> TermClassMap
          -- | Maps type variables to associated registry class.
          -> TypeClassMap
          -- | Continuation to call with new term bindings when constraints succeed.
          -> NamedYieldFn r
          -- | Match builder expression.
          -> MatchBuilder (MatchExpr r)
-- Finished matching term.
matchTerm [] termBindings typeBindings (nm,contFn) = do
  let termVars = Map.keys termBindings
  termRegs <- termUF $ mapM getTermRegIdx (Map.elems termBindings)
  let termRegMap = Map.fromList $ termVars `zip` termRegs
  let typeVars = Map.keys typeBindings
  typeRegs <- typeUF $ mapM getTypeRegIdx (Map.elems typeBindings)
  let typeRegMap = Map.fromList $ typeVars `zip` typeRegs
  return $ MatchYield nm (contFn termRegMap typeRegMap) MatchAbort
-- TConst case (TODO: Add optimizations)
matchTerm ((reg, TConst c _):r) termBindings typeBindings contFn = do
  regIdx <- termUF $ getTermRegIdx reg
  mergeExpr (matchTerm r termBindings typeBindings contFn) $ \newSucc -> do
    MatchCheck regIdx c newSucc MatchAbort
-- TVar case
matchTerm ((reg, TVar v tp):r) termBindings typeBindings contFn =
  case Map.lookup v termBindings of
    Nothing -> do
      d <- termUF $ UF.readClassDesc reg
      case typeRegClass d of
        Just tpClass -> do
          matchType [(tpClass, tp)] typeBindings $ \newTypeBindings ->
            matchTerm r (Map.insert v reg termBindings) newTypeBindings contFn
        Nothing -> do
          tpClass <- readTypeClass reg
          let me = matchType [(tpClass, tp)] typeBindings $ \newTypeBindings ->
                     matchTerm r
                               (Map.insert v reg termBindings)
                               newTypeBindings
                               contFn
          regIdx <- termUF $ getTermRegIdx reg
          mergeExpr me $ \newSucc -> MatchTypeBind regIdx newSucc
    Just prev -> do
      regIdx <- termUF $ getTermRegIdx reg
      prevIdx <- termUF $ getTermRegIdx prev
      res <- mergeRegs reg prev
      case res of
        UF.AssertRedundant -> matchTerm r termBindings typeBindings contFn
        UF.AssertFailed -> return MatchAbort
        UF.AssertSuccess ->
          mergeExpr (matchTerm r termBindings typeBindings contFn) $ \newSucc -> do
            MatchCompare regIdx prevIdx newSucc MatchAbort
-- TApp case
matchTerm ((reg, TApp _ op s args):r) termBindings typeBindings contFn =
  -- Verify op is bottom-up type inferable.
  assert (opIsBottomUpInferable (mkOp op s)) $ do
    d <- termUF $ UF.readClassDesc reg
    case termRegApp d of
      MCNotOps notOps
        | op `Set.member` notOps -> return MatchAbort
        | otherwise -> do
           regs <- assertApp reg op
           let newR = V.toList (regs `V.zip` args) ++ r
           mergeExpr (matchTerm newR termBindings typeBindings contFn) $ \newSucc ->
             MatchBind (termRegIdx d) op newSucc MatchAbort
      MCApp oldOp oldRegs
        | op == oldOp -> do
           let newR = V.toList (oldRegs `V.zip` args) ++ r
            in matchTerm newR termBindings typeBindings contFn
        | otherwise -> return MatchAbort

-- | Main function for extending match programs with additional rewrites.
extendProgram :: TermCtor
              -> NamedYieldFn r
              -> MatchExpr r
              -> MatchExpr r
extendProgram l@TApp{} yieldFn ins = do
  let desc = TermClassDesc { termRegIdx = 0
                           , termRegApp = MCNotOps Set.empty
                           , typeRegClass = Nothing }
  let (cZero,ufs) = UF.runAction UF.empty $ UF.freshClass desc
  let initState = MatchBuildState {
                    termRegClassMap = Map.singleton 0 cZero
                  , nextTermReg = 1
                  , termUnionFind = ufs
                  , typeRegClassMap = Map.empty
                  , nextTypeReg = 0
                  , typeUnionFind = UF.empty
                  }
  let contM = flip evalMatchState $ do
        matchTerm [(cZero,l)] Map.empty Map.empty yieldFn
   in applyMatch ins contM initState
extendProgram _ _ _ = error "Term for extend program must be a nonvariable."

-- | Create a type substitution from mapping from variables to register indices
-- and vector.
makeTypeSubst :: TypeVarRegMap -> Vector DagType -> TypeSubst
makeTypeSubst (Map.toList -> typeVarRegs) typeRegs =
  let shapeVarRegs = [ (sv,i) | (ShapeVar sv, i) <- typeVarRegs]
      widthVarRegs = [ (wv,i) | (WidthVar wv, i) <- typeVarRegs]
      shapeFn (v,i) = (v, typeRegs V.! i)
      widthFn (v,i) = case typeRegs V.! i of
                        SymInt w -> (v,w)
                        _ -> error "Unexpected value for widthVar"
   in TypeSubst {
          shapeSubst = Map.fromList $ map shapeFn shapeVarRegs
        , widthSubst = Map.fromList $ map widthFn widthVarRegs
        }

-- Rewriter and RewritePrograms {{{1

-- | Function yield identifies.
type ExprResult m
  = Rewriter m
    -> Vector (MonadTerm m)
    -> Vector DagType
    -> m (Maybe (MonadTerm m))

type RewriteExpr m = MatchExpr (ExprResult m)

newtype RewriteProgram m = RS (Map OpDef (RewriteExpr m))

-- | The rewrite cache is used to store normalized terms for a set of rules.
data Rewriter m = RC {
    rewriterProgram :: RewriteProgram m
  , cacheRef :: IORef (Map (MonadTerm m) (MonadTerm m))
  }

-- | @cacheResult rew t u@ caches that @t@ reduces to @u@ in the writer.
cacheResult :: (ApplyOpMonad m, MonadIO m, Ord (MonadTerm m))
            => Rewriter m -> MonadTerm m -> MonadTerm m -> m ()
cacheResult rew t r =
   liftIO $ modifyIORef (cacheRef rew) (Map.insert t r)

-- | Create an empty rewriter
mkRewriter :: RewriteProgram m -> IO (Rewriter m)
mkRewriter rls = do
  ref <- newIORef Map.empty
  return RC { rewriterProgram = rls, cacheRef = ref }

-- | Create a rule state with no rules.
emptyProgram :: RewriteProgram m
emptyProgram = RS Map.empty

-- | Runs match program against given node.
runMatchExpr :: (ApplyOpMonad m, TermClass (MonadTerm m))
             => Rewriter m
             -> RewriteExpr m
             -> MonadTerm m
             -> m (Maybe (MonadTerm m))
runMatchExpr rew pgrm n = impl pgrm (V.singleton n) V.empty
  where impl MatchAbort _termRegs _typeRegs = return Nothing
        impl (MatchYield _ fn f) termRegs typeRegs = do
          maybeR <- fn rew termRegs typeRegs
          case maybeR of
            Just r -> return (Just r)
            Nothing -> impl f termRegs typeRegs
        impl (MatchBind r op t f) termRegs typeRegs =
          case getTypedApp (termRegs V.! r) of
            Just (op', _, v) | op == op' -> impl t (termRegs V.++ v) typeRegs
            _ -> impl f termRegs typeRegs
        impl (MatchCheck r c' t f) termRegs typeRegs =
          case termConst (termRegs V.! r) of
            Just c | c == c' -> impl t termRegs typeRegs
            _ -> impl f termRegs typeRegs
        impl (MatchCompare r r' t f) termRegs typeRegs =
          let next = if termRegs V.! r == termRegs V.! r' then t else f
           in impl next termRegs typeRegs
        impl (MatchTypeBind r t) termRegs typeRegs =
          impl t termRegs (typeRegs V.++ (V.singleton (termType (termRegs V.! r))))
        impl (MatchTypeInt tr t f) termRegs typeRegs =
          let next = case typeRegs V.! tr of
                       SymInt _ -> t
                       _ -> f
           in impl next termRegs typeRegs
        impl (MatchTypeArray tr t f) termRegs typeRegs =
          case typeRegs V.! tr of
            SymArray trLen eltType ->
              impl t termRegs (typeRegs V.++ V.fromListN 2 [SymInt trLen, eltType])
            _ -> impl f termRegs typeRegs
        impl (MatchTypeRec tr recDef t f) termRegs typeRegs =
          case typeRegs V.! tr of
            SymRec trRecDef trSubst | recDef == trRecDef ->
                 let newRegs = recFieldTypes trRecDef trSubst
                  in impl t termRegs (typeRegs V.++ newRegs)
            _ -> impl f termRegs typeRegs
        impl (MatchTypeCheck tr tp t f) termRegs typeRegs =
          let next = if typeRegs V.! tr == tp then t else f
           in impl next termRegs typeRegs
        impl (MatchTypeCompare tr tr' t f) termRegs typeRegs =
          let next = if typeRegs V.! tr == typeRegs V.! tr' then t else f
           in impl next termRegs typeRegs

-- | Normalize a node assuming all subnodes are normalized.
norm :: (ApplyOpMonad m, MonadIO m, TermClass (MonadTerm m)) => Rewriter m -> MonadTerm m -> m (MonadTerm m)
norm rew n@(getTypedApp -> Just (op,_,_)) = do
  let ref = cacheRef rew
  cache <- liftIO $ readIORef ref
  case Map.lookup n cache of
    Just res -> return res
    Nothing -> do
      let RS rls = rewriterProgram rew
      let pgrm = maybe MatchAbort id (Map.lookup op rls)
      maybeR <- runMatchExpr rew pgrm n
      r <- maybe (return n) (norm rew) maybeR
      cacheResult rew n r
      return r
norm _ n = return n

reduce :: ( ApplyOpMonad m
          , MonadIO m
          , TermClass (MonadTerm m))
       => Rewriter m -> MonadTerm m -> m (MonadTerm m)
reduce rew n@(getAppVector -> Just (op,args)) = do
  cache <- liftIO $ readIORef (cacheRef rew)
  case Map.lookup n cache of
    Just res -> return res
    Nothing -> do
      redArgs <- V.mapM (reduce rew) args
      res <- applyOpVector op redArgs
      r' <- norm rew res
      cacheResult rew n r'
      return r'
reduce _ n = return n

{-
type RuleAction m =
  MonadTerm m
    -> TypeSubst op
    -> Map Var (MonadTerm m)
    -> Rewriter m
    -> m (Maybe (MonadTerm m), Rewriter m)

yieldFnFromAct :: ApplyOpMonad m
               => String
               -> RuleAction m
               -> YieldFn (MonadTerm m) (RewriteComp m (Maybe (MonadTerm m)))
yieldFnFromAct name act termRegMap typeRegMap =
  AppFn nm $ \termVec typeVec ->
        act (termVec V.! 0)
            typeSubst
            termSubst

addRuleAct :: ApplyOpMonad m
           => RewriteProgram m
           -> String
           -> MonadTerm m
           -> RuleAction m
           -> RewriteProgram
addRuleApp (RS m) nm (evalTerm -> l@(TApp _ op _sub _)) act =
  let oldPgrm = maybe MatchAbort id (Map.lookup op m)
      newPgrm = extendProgram l (yieldFnFromAct nm act) oldPgrm
   in RS (Map.insert op newPgrm m)
addRuleApp _ nm _ _act =
  error $ "internal: addRule: Unsupported rule " ++ show nm ++
          " with a variable or constant left-hand side"
          -}

-- | @makeTerm t varMap typeMap@ constructs a function which given the term and
-- type registers, returns a term in the new rewriter monad.
makeTerm :: ( ApplyOpMonad m
            , MonadIO m
            , TermClass (MonadTerm m))
         => TermCtor -> YieldFn (ExprResult m)
makeTerm initTerm termRegMap typeRegMap rew termRegs typeRegs =
  let impl (TConst c tp) = makeConstant c tp
      impl (TVar v _) = return $ termRegs V.! (termRegMap Map.! v)
      impl (TApp i op s args) = do
        m <- get
        case Map.lookup i m of
          Just t -> return t
          Nothing -> assert (opIsBottomUpInferable (mkOp op s)) $ do
            let sub = composeTypeSubst s (makeTypeSubst typeRegMap typeRegs)
            vals <- V.mapM impl args
            t <- lift $ do
              u <- applyOpVector (mkOp op sub) vals
              norm rew u
            modify (Map.insert i t)
            return t
   in fmap Just $ evalStateT (impl initTerm) Map.empty

-- | Modify expression associated with op in a program.
modifyExpr :: RewriteProgram m
           -> OpDef
           -> (RewriteExpr m -> RewriteExpr m)
           -> RewriteProgram m
modifyExpr (RS m) op fn =
  let pgrm = maybe MatchAbort id (Map.lookup op m)
   in RS (Map.insert op (fn pgrm) m)

-- | @addRule@ adds a rewrite rule @r@ to rewriter and flushes reduced nodes cache.
addRule :: (ApplyOpMonad m, MonadIO m, TermClass (MonadTerm m))
        => RewriteProgram m -> Rule -> RewriteProgram m
addRule p (Rule name l@(TApp _ op _sub _) r) =
  modifyExpr p op $
    extendProgram l (name, makeTerm r)
addRule _ (Rule nm _ _) =
  error $ "internal: addRule: Unsupported rule " ++ show nm ++
          " with a variable or constant left-hand side"

-- | @addOpDefRule@ adds a substitution that expands an operator to its
-- definition.
addOpDefRule :: (ApplyOpMonad m, MonadIO m, TermClass (MonadTerm m))
             => RewriteProgram m
             -> OpDef
             -> (TypeSubst -> V.Vector (MonadTerm m) -> m (MonadTerm m))
             -> RewriteProgram m
addOpDefRule p op rhsFn =
  let argTypes = opDefArgTypes op
      opArgs = V.enumFromN 0 (V.length argTypes)
      mkArgVar i = TVar (show i) (argTypes V.! i)
      l = TApp 0 op (idSubst (opDefParams op)) (V.map mkArgVar opArgs)
      yieldFn termRegMap typeRegMap =
        let shapeRegMap = Map.fromList [ (v,i) | (ShapeVar v, i) <- Map.toList typeRegMap ]
            widthRegMap = Map.fromList [ (v,i) | (WidthVar v, i) <- Map.toList typeRegMap ]
            termIndices = V.map ((termRegMap Map.!) . show) opArgs
        in \_ terms types ->
             let widthFn i = let SymInt e = types V.! i in e
                 sub = TypeSubst { shapeSubst = Map.map (types V.!) shapeRegMap
                                 , widthSubst = Map.map widthFn widthRegMap }
              in fmap Just $ rhsFn sub (V.map (terms V.!) termIndices)
   in modifyExpr p op $
        extendProgram l (opDefName op ++ "-def",yieldFn)
