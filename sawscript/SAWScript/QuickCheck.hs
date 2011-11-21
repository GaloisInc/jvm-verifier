module SAWScript.QuickCheck (pickRandom,minimizeCounterExample) where

import System.Random(randomIO, randomRIO)
import Verinf.Symbolic
import qualified Data.Vector as V
import Control.Monad(replicateM)



-- Or, perhaps, short, tall, grande, venti :-)
data RandomSpec = Least | Small | Medium | Large | Largest


pickRandomSize :: DagType -> RandomSpec -> IO CValue
pickRandomSize ty spec =
  case ty of

    SymBool -> CBool `fmap`
      case spec of
        Least   -> return False
        Small   -> return False
        Medium  -> randomIO
        Large   -> return True
        Largest -> return True

    SymInt w ->
      case widthConstant w of
         Just n  -> mkCInt n `fmap`
           do let least   = 0
                  largest = bitWidthSize n - 1
              case spec of
                Least   -> return least
                Small   -> randomRIO (least, min largest (least + 100))
                Medium  -> randomRIO (least, largest)
                Large   -> randomRIO (max least (largest - 100), largest)
                Largest -> return largest

         Nothing -> qcFail "integers of polymorphic size"

    SymArray els ty1 ->
      case widthConstant els of
        Just n    -> (CArray . V.fromList) `fmap`
                     replicateM (numBits n) (pickRandomSize ty1 spec)
        Nothing   -> qcFail "arrays of non-constant size"

    SymRec def su ->
      CRec def su `fmap` V.mapM (`pickRandomSize` spec) (recFieldTypes def su)

    SymShapeVar _ -> qcFail "polymorphic values"
  where
  qcFail x = fail $
                "QuickCheck: Generating random " ++ x ++ " is not supported."

-- Distribution of tests.  The choice is somewhat arbitrary.
pickRandom :: DagType -> IO CValue
pickRandom ty = pickRandomSize ty =<< ((`pick` distr) `fmap` randomRIO (0,99))
  where
  pick n ((x,s) : ds) = if n < x then s else pick (n-x) ds
  pick _ _            = Medium

  distr :: [(Int, RandomSpec)]
  distr = [ (5,  Least)
          , (30, Small)
          , (30, Medium)
          , (30, Large)
          , (5,  Largest)
          ]


-- TODO: Given a function that will tell us if a collection of inputs
-- violates some goal (i.e., return "Just violated_goal"), try to find
-- a set of smaller values that do the job.  This should help present
-- nicer examples.
minimizeCounterExample :: Monad m => ([CValue] -> m (Maybe goal))
                       -> [CValue] -> goal -> m ([CValue], goal)
minimizeCounterExample stillBad vs g = return (vs,g)

















