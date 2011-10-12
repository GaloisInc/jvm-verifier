import Text.ParserCombinators.ReadP as P
import Text.PrettyPrint as PP hiding (parens)
import MonadLib
import Data.Char
import Numeric
import Data.List
import Data.Function

data BV = BV { val :: !Integer, width :: !Int } deriving Show

data Val  = Arr [(BV,BV)] BV
          | Val BV
          | Var String
           deriving Show

str x    = spaces >> string x
spaces   = munch isSpace >> return ()
parens p = spaces >> between (P.char '(') (P.char ')') p

pName    = do spaces
              x <- satisfy isAlpha
              xs <- munch isAlphaNum
              return (x:xs)

pBV :: ReadP BV
pBV = do spaces
         string "0b"
         ds <- many (P.char '0' `mplus` P.char '1')
         let twos = 1 : map (2*) twos
             dig '0' = 0
             dig _   = 1
         return BV { val   = sum (zipWith (*) twos (reverse (map dig ds)))
                   , width = length ds
                   }

pVal = parens $ do str "="
                   x <- pName
                   v <- (Var `fmap` pName) `mplus` (Val `fmap` pBV)
                   return (x, v)

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
          return (n, Arr (sortBy (compare `on` (val . fst)) vs) v)

pOut =
  do str "sat"
     str "MODEL"
     xs <- many (pVal `mplus` pArr)
     str "----"
     spaces
     return xs

ppVal (x,v) =
  case v of
    Var v -> text x <+> text "=" <+> text v
    Val n -> text x <+> text "=" <+> ppV n
    Arr vs v -> text " " $$ vcat (map ppEnt vs) $$
                text x <> brackets (text "_") <+> text "=" <+> ppV v
      where ppEnt (a,b) = text x <> brackets (integer (val a))
                      <+> text "=" <+> ppV b

  where hex v = text "0x" <> text (showHex v "")
        ppV n = hex (val n) <+> text ":" <+> brackets (int (width n))

main = interact ((++ "\n") . show . vcat . map ppVal . one . readP_to_S pOut)
  where one xs = case filter (null . snd) xs of
                   [(a,_)] -> a
                   [] -> error "parse error"

