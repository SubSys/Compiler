{-# LANGUAGE NoImplicitPrelude #-}
module Core.List.Util (flatten, singleton) where


import qualified Data.List as List



flatten :: [[a]] -> [a]         
flatten xs = (\z n -> List.foldr (\x y -> List.foldr z y x) n xs) (:) []



singleton :: a -> [a]
singleton a = [a]
