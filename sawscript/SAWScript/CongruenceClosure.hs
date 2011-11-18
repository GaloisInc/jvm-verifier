module SAWScript.CongruenceClosure 
  ( Term(..)
  , Set
  , empty
  , insertTerm
  , insertEquation
  , findEquivalent
  , fromList
  , toList
  ) where

import Control.Applicative ((<$), (<$>))
import Control.Monad.State
import Data.List (foldl', sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (const)

-- Term {{{1

-- | A term with the given operator.
data Term op = Term op [Term op]
  deriving (Eq, Ord, Show)

-- CCSet definitions {{{1

newtype Class = Class Integer
  deriving (Eq, Ord)

data App op = App !op ![Class]
  deriving (Eq, Ord)


data CCSet op = CC {
         -- | Number of classes added so far.
         ccCnt :: !Integer
         -- | Maps representative to equivalent terms that were added to set.
       , ccElementMap :: !(Map Class (Set (Term op)))
         -- | Maps classes to the representative.
       , ccRepMap :: !(Map Class Class)
         -- | Maps applications to the representative.
       , ccAppMap :: !(Map (App op) Class)
       }

instance Ord op => Eq (CCSet op) where
  x == y = toSet x == toSet y

instance Ord op => Ord (CCSet op) where
  x `compare` y = toSet x `compare` toSet y

instance (Ord op, Show op) => Show (CCSet op) where
  show cc = "fromList " ++ show (toList cc)

-- | Return empty set.
empty :: CCSet op
empty = CC { ccCnt = 0
           , ccElementMap = Map.empty
           , ccRepMap = Map.empty
           , ccAppMap = Map.empty
           }

-- | Insert term into set.
insertTerm :: Ord op => Term op -> CCSet op -> CCSet op
insertTerm = execState . getClass

-- | Assert that two terms are equal.
insertEquation :: Ord op => Term op -> Term op -> CCSet op -> CCSet op
insertEquation x y s =
  flip execState s $ do
    xCl <- getClass x
    yCl <- getClass y
    processEquivalences [(xCl, yCl)]

-- | Returns all terms equivalent to term in set.
-- N.B. The list may be empty if the term is not in the set.
findEquivalent :: Ord op => Term op -> CCSet op -> [Term op]
findEquivalent t cc =
  case getExistingClass t cc of
    Just cl -> Set.toList $
      Map.findWithDefault Set.empty cl (ccElementMap cc)
    Nothing -> []

-- | Assert all elements in list are equal.
assertAllEqual :: Ord op => [Term op] -> CCSet op -> CCSet op
assertAllEqual [] s = s
assertAllEqual [x] s = s
assertAllEqual (x:y:r) s = assertAllEqual (y:r) $! insertEquation x y s

fromSet :: Ord op => Set (Set (Term op)) -> CCSet op
fromSet = Set.fold (assertAllEqual . Set.toList) empty

toSet :: Ord op => CCSet op -> Set (Set (Term op))
toSet = Set.fromList . Map.elems . ccElementMap

-- | Create a congruence closure set from the double list of terms,
-- where each list of terms denotes an equivalence class.
fromList :: Ord op => [[Term op]] -> CCSet op
fromList = foldl' (flip assertAllEqual) empty

-- | Convert a congruence closure set into a double list of terms.
toList :: Ord op => CCSet op -> [[Term op]]
toList = map Set.toList . Set.toList . toSet

-- Utilities {{{1

mapAppArgs :: (Class -> Class) -> App op -> App op
mapAppArgs fn (App op args) = App op (map fn args)

-- | Returns a class for term if one exists.
getExistingClass :: Ord op => Term op -> CCSet op -> Maybe Class
getExistingClass t cc = impl t
  where impl (Term op args) = do
          argClasses <- mapM impl args
          Map.lookup (App op argClasses) (ccAppMap cc)

-- State monad for manipulating Set {{{1

type Comp op a = State (CCSet op) a

type Pair t = (t,t)

processEquivalences :: Ord op => [Pair Class] -> Comp op ()
processEquivalences [] = return ()
processEquivalences ((x,y):rest) = do
  cc <- get
  let repMap = ccRepMap cc
  let xCl = Map.findWithDefault x x repMap
  let yCl = Map.findWithDefault y y repMap
  if xCl == yCl then
    processEquivalences rest
  else do
    let mapFn z = if z == xCl then yCl else z
    let appClassListMap 
          = Map.fromListWith (++)
          $ map (\(app,cl) -> (mapAppArgs mapFn app, [mapFn cl]))
          $ Map.toList (ccAppMap cc)
    put cc { ccElementMap = Map.fromListWith Set.union
                          $ map (\(cl,tl) -> (mapFn cl, tl))
                          $ Map.toList (ccElementMap cc)
           , ccRepMap = Map.insert xCl yCl $ Map.map mapFn repMap
           , ccAppMap = Map.map head appClassListMap
           }
    let new = [ (l,r) | l:rl <- Map.elems appClassListMap, r <- rl ]
    processEquivalences (new ++ rest)

addTerm :: Ord op => Class -> Term op -> Comp op ()
addTerm cl t = do
  modify $ \cc ->
    cc { ccElementMap 
          = Map.insertWith Set.union cl (Set.singleton t)
          $ ccElementMap cc }

getClass :: Ord op => Term op -> Comp op Class
getClass t@(Term op args) = do
  argClasses <- mapM getClass args
  let app = App op argClasses
  appMap <- gets ccAppMap
  case Map.lookup app appMap of
    Just cl -> cl <$ addTerm cl t
    Nothing -> do
      cc <- get
      let cnt = ccCnt cc
      let cl = Class cnt
      put cc { ccCnt = cnt + 1
             , ccAppMap = Map.insert app cl (ccAppMap cc)
             }
      cl <$ addTerm cl t
