{-# LANGUAGE NoImplicitPrelude #-}
module Core.Either.Util (
      fromLeft
    , fromRight
) where


import qualified Data.Either as E



fromLeft :: a -> E.Either a b -> a
fromLeft _ (E.Left a) = a
fromLeft a (E.Right _) = a


fromRight :: b -> E.Either a b -> b
fromRight _ (E.Right b) = b
fromRight b _         = b


