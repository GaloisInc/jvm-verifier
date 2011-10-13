module SAWScript.Yices
  ( yices, BV(..), YVal(..), YResult(..), ppVal
  , resolveInputs
  ) where

import Text.ParserCombinators.ReadP as P
import Text.PrettyPrint as PP hiding (parens)
import Data.Char
import Control.Monad(mplus,msum)
import Numeric
import Data.List
import Data.Function
import System.Process
import qualified Data.Map as M
import SMTLib1

data BV   = BV { val :: !Integer, width :: !Int } deriving Show

data YVal = YArr [(BV,BV)] BV
          | YVal BV
          | YVar String
           deriving Show

data YResult  = YUnknown
              | YUnsat
              | YSat (M.Map String YVal)

yices :: Maybe Int -> Script -> IO YResult
yices mbTime script =
  do txt <- readProcess "yices" (["--full-model"] ++ timeOpts)
                (show (pp script))
     case parseOutput txt of
       Just a -> return a
       _      -> fail "yices: Failed to parse the output from Yices"
  where timeOpts = case mbTime of
                     Nothing -> []
                     Just t  -> ["--timeout=" ++ show t]


resolveInputs :: M.Map String YVal -> [Ident] -> [YVal]
resolveInputs model ins = map getIdent ins
  where
  getIdent i = getVar (show (pp i))

  getVar x = case M.lookup x model of
               Just (YVar y) -> getVar y
               Just v        -> v
               Nothing       -> YVar x    -- Should not happen!



--------------------------------------------------------------------------------

str     :: String -> ReadP ()
str x    = pSpaces >> string x >> return ()

pSpaces :: ReadP ()
pSpaces  = munch isSpace >> return ()

parens  :: ReadP a -> ReadP a
parens p = pSpaces >> between (P.char '(') (P.char ')') p

pName   :: ReadP String
pName    = do pSpaces
              x <- satisfy isAlpha
              xs <- munch isAlphaNum
              return (x:xs)

pBV :: ReadP BV
pBV = do str "0b"
         ds <- many (P.char '0' `mplus` P.char '1')
         let twos = 1 : map (2*) twos
             dig '0' = 0
             dig _   = 1
         return BV { val   = sum (zipWith (*) twos (reverse (map dig ds)))
                   , width = length ds
                   }

pVal :: ReadP (String, YVal)
pVal = parens $ do str "="
                   x <- pName
                   v <- (YVar `fmap` pName) `mplus` (YVal `fmap` pBV)
                   return (x, v)

pArr :: ReadP (String, YVal)
pArr = do str "---"
          n <- pName
          str "---"
          vs <- many $ parens $
                do str "="
                   k <- parens (pName >> pBV)
                   v <- pBV
                   return (k,v)
          str "default:"
          v <- pBV
          return (n, YArr (sortBy (compare `on` (val . fst)) vs) v)

pOut :: ReadP YResult
pOut =
  do r <- msum [ do str "sat"
                    str "MODEL"
                    xs <- many (pVal `mplus` pArr)
                    str "----"
                    return $ YSat $ M.fromList xs
               , str "unsat" >> return YUnsat
               , str "unknown" >> return YUnknown
               ]
     pSpaces
     return r

parseOutput :: String -> Maybe YResult
parseOutput txt =
  case filter (null . snd) (readP_to_S pOut txt) of
    [(a,_)] -> return a
    _       -> Nothing

--------------------------------------------------------------------------------


ppVal :: (String, YVal) -> Doc
ppVal (x,vv) =
  case vv of
    YVar v -> text x <+> text "=" <+> text v
    YVal n -> text x <+> text "=" <+> ppV n
    YArr vs v -> vcat (map ppEnt vs) $$
                 text x <> brackets (text "_") <+> text "=" <+> ppV v
      where ppEnt (a,b) = text x <> brackets (integer (val a))
                      <+> text "=" <+> ppV b

  where hex v = text "0x" <> text (showHex v "")
        ppV n = hex (val n) <+> text ":" <+> brackets (int (width n))


