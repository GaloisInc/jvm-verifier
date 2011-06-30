{- |
Module           : $Header$
Description      :
Stability        : provisional
Point-of-contact : jhendrix, jstanley
-}

module Utils.Common where

import Control.Monad.Trans

slashesToDots :: String -> String
slashesToDots = map (\c -> if c == '/' then '.' else c)

-- | Converts integral into bounded num class.
-- TODO: Revisit error handling when integer is out of range.
safeCast :: (Integral s, Bounded t, Integral t, Num t) => s -> t
safeCast = impl minBound maxBound . toInteger
  where impl :: Integral t => t -> t -> Integer -> t
        impl minb maxb s
          | toInteger minb <= s && s <= toInteger maxb = fromInteger s
          | otherwise = error "internal: safeCast argument out of range"

dbugM :: MonadIO m => String -> m ()
dbugM = liftIO . putStrLn
