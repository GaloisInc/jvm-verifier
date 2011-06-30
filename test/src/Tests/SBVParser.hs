{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix
-}

module Tests.SBVParser (sbvParserTests) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import SBVParser
import Test.QuickCheck
import Test.QuickCheck.Monadic as MQC

import IO.Session

import SBVModel.SBV
import Symbolic

sbvParserTests :: [(Args, Property)]
sbvParserTests =
  [ ( stdArgs{ maxSuccess = 1 }
    , label "testInferRecordDef" testInferRecordDef
    )
  , ( stdArgs{ maxSuccess = 1 }
    , label "testCPlus" testCPlus
    )
  ]

testInferRecordDef :: Property
testInferRecordDef =  MQC.monadicIO $ MQC.run $ do
  sess <- makeSession
  let e = toIO sess
  let sref_ty = SymArray (constantWidth 12) (SymInt (constantWidth 32))
  (ab_ty,abRecordMap) <- e $
    extendRecords noRecords
                  (OI "abConstruct" "{0}" defaultPrec)
                  [ ("a", OI "aFldSel" "{0}.a" fieldPrec, sref_ty)
                  , ("b", OI "bFldSel" "{0}.b" fieldPrec, sref_ty)
                  ]
  (cRecord,abcRecordMap) <- e $ extendRecords abRecordMap
                                              (OI "cConstruct" "{0}" defaultPrec)
                                              [("c", OI "cFldSel" "{0}.c" fieldPrec, sref_ty)]
  (_,ufuns) <- e $ extendUfuns noUfuns
                               ("cplus", OI "c+" "{0} `c+` {1}" defaultPrec, [ab_ty,sref_ty])
                               cRecord
  sbv <- loadSBV "test/src/support/cplus.sbv"
  (cPlusOpDef, WEF applyCPlus) <- e $ liftOpSession $
    parseSBVOp abcRecordMap ufuns "cplus" sbv
  e $ applyCPlus =<< V.mapM freshUninterpretedVar (opDefArgTypes cPlusOpDef)

fieldPrec :: OpPrec
fieldPrec = 100

testCPlus :: Property
testCPlus = monadicIO $ do
  _ <- run $ runOpSession $ do
    let arrayType12 = SymArray (constantWidth 12) (SymInt (constantWidth 32))
    abRecDef <- getStructuralRecord (Set.fromList ["a", "b"])
    let abSubst = emptySubst { shapeSubst = 
                    Map.fromList [ ("a", arrayType12)
                                 , ("b", arrayType12) ] }
        abRec = SymRec abRecDef abSubst
    cRecDef <- getStructuralRecord (Set.fromList ["c"])
    let cSubst = emptySubst { shapeSubst = 
                   Map.fromList [ ("c", arrayType12) ] }
        cRec = SymRec cRecDef cSubst
    let recordFn [("a", _), ("b", _)] = Just abRec
        recordFn [("c", _)] = Just cRec
        recordFn _ =  Nothing
    cplusOpDef <- uninterpretedOp "c+" 
                                  (V.fromList [abRec, arrayType12])
                                  cRec
    let uninterpFn "cplus" [_x,_y] = Just $ groundOp cplusOpDef
        uninterpFn "cplus" args = error $ "Unexpected arg count " ++ show (length args)
        uninterpFn name _ = error $ "Unexpected uninterpreted function " ++ name
    sbv <- liftIO $ loadSBV "test/src/support/cplus.sbv"
    (cPlusSbvOp,WEF applyCPlus) <- parseSBVOp recordFn uninterpFn "cplus" sbv
    runSymSession $ do
      args <- V.mapM freshUninterpretedVar (opDefArgTypes cPlusSbvOp) :: SymbolicMonad (V.Vector Node)
      _ <- applyCPlus args
      return ()
  assert True
