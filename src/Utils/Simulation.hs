{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jstanley
-}

module Utils.Simulation where

import Execution
import Simulation

{-
withValidResult :: Show term => Either (FinalResult term) a -> (a -> m b) -> m b
withValidResult (Left (Exc ref)) _ = error $ "wvr: obtained exception: ref = " ++ show ref
withValidResult (Left _) _         = error $ "internal: unexpected FinalResult"
withValidResult (Right rslt) f     = f rslt
-}

withoutExceptions :: [(PathDescriptor, FinalResult term)]
                  -> [(PathDescriptor, FinalResult term)]
withoutExceptions = filter (\r -> case snd r of Exc{} -> False ; _ -> True)