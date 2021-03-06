{-# LANGUAGE NoImplicitPrelude #-}
module Core.Utils (
    mapPair
  , mapPairs
  , mapPairM
  , mapPairsM
  , applyMaybe
) where



-- ~
import Prelude (return, Maybe(..), ($))
import qualified Control.Monad as M
import qualified Prelude as Pre
-- ~


mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

mapPairs :: (a -> b) -> [(a, a)] -> [(b, b)]
mapPairs f = Pre.map (mapPair f)


mapPairM :: (M.Monad m) => (a -> m b) -> (a, a) -> m (b, b)
mapPairM f (x, y) = do
    x' <- f x
    y' <- f y
    
    return (x', y')

mapPairsM :: (M.Monad m) => (a -> m b) -> [(a, a)] -> m [(b, b)]
mapPairsM f = M.mapM (mapPairM f)


applyMaybe :: (a -> b) -> Maybe a -> Maybe b
applyMaybe _ Nothing = Nothing
applyMaybe f (Just x) = Just $ f x

