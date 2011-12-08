{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix
-}

module Tests.SBVParser (sbvParserTests) where

import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import SBVParser
import Test.QuickCheck
import Test.QuickCheck.Monadic as MQC

import SBVModel.SBV

import Verinf.Symbolic

sbvParserTests :: [(Args, Property)]
sbvParserTests =
  [ ( stdArgs{ maxSuccess = 1 }
    , label "testCPlus" testCPlus
    )
  ]


fieldPrec :: OpPrec
fieldPrec = 100

testCPlus :: Property
testCPlus = monadicIO $ do
  _ <- run $ do
    oc <- mkOpCache
    let arrayType12 = SymArray (constantWidth 12) (SymInt (constantWidth 32))
    let abRecDef = getStructuralRecord oc (Set.fromList ["a", "b"])
        abSubst = emptySubst { shapeSubst =
                    Map.fromList [ ("a", arrayType12)
                                 , ("b", arrayType12) ] }
        abRec = SymRec abRecDef abSubst
    let cRecDef = getStructuralRecord oc (Set.fromList ["c"])
        cSubst = emptySubst { shapeSubst = Map.fromList [ ("c", arrayType12) ] }
        cRec = SymRec cRecDef cSubst
    cplusOpDef <- uninterpretedOp oc
                                  "c+"
                                  (V.fromList [abRec, arrayType12])
                                  cRec
    let uninterpFn "cplus" [_x,_y] = Just $ groundOp cplusOpDef
        uninterpFn "cplus" args = error $ "Unexpected arg count " ++ show (length args)
        uninterpFn name _ = error $ "Unexpected uninterpreted function " ++ name
    sbv <- liftIO $ loadSBV "test/src/support/cplus.sbv"
    (cPlusSbvOp, cPlusTerm) <- parseSBV oc uninterpFn "cplus" sbv
    de <- mkExactDagEngine
    args <- V.mapM (deFreshInput de) (opDefArgTypes cPlusSbvOp)
    let inputFn i _ = return (args V.! i)
    v <- evalDagTerm inputFn (deTermSemantics de) cPlusTerm
    v `seq` return ()
  assert True
