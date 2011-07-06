{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : lerkok, matthews
-}

{-# OPTIONS -Wall #-}

module PPTest(main) where

import qualified Data.Vector as V
import qualified SBVModel.SBV as SBV
import qualified SBVParser as SBV
import Verinf.Symbolic

pp :: TermDescriptor d => Node d -> String
pp = ppSymTermSExpWith
       defaultPPConfig {
          ppShowConstTypes  = False       -- default: False
        , ppMinSharingDepth = Just 0      -- default: Just 0
        , ppLineLength      = 80          -- default: 80
        }

sbvToTerm :: FilePath -> SymbolicMonad (MonadTerm SymbolicMonad)
sbvToTerm path = do
  pgm <- liftIO $ SBV.loadSBV path
  let (argTys,_) = SBV.inferSBVFunctionType (const Nothing) pgm
  args <- mapM freshUninterpretedVar argTys
  SBV.parseSBV (const Nothing) (const (const Nothing)) pgm (V.fromList args)

test :: (MonadTerm SymbolicMonad -> String) -> FilePath -> IO ()
test prt f = putStrLn =<< runSymbolic (prt `fmap` sbvToTerm f)

main :: IO ()
main = mapM_ (test pp) [
           smallTest
         -- , largeTest
         ]

smallTest, largeTest :: FilePath
smallTest = "sbv/ppTest.sbv"
largeTest = "../ECC/sbv/sref_p384_mul.sbv"

_ignore_nouse :: a
_ignore_nouse = undefined largeTest
