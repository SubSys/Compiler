{-# LANGUAGE NoImplicitPrelude #-}
module Core.Utils (
    mapPair
  , mapPairs
) where



-- ~
import qualified Prelude as Pre
-- ~


mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

mapPairs :: (a -> b) -> [(a, a)] -> [(b, b)]
mapPairs f = Pre.map (mapPair f)

