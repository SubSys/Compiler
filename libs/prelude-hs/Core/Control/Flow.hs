{-# LANGUAGE NoImplicitPrelude #-}
module Core.Control.Flow
    ( (|>)
    , (<|)
    , (!>)
    , (<!)
    , apply
    , apply'
    ) where


import Prelude (seq)


infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = apply x f

apply :: a -> (a -> b) -> b
apply x f = f x


infixr 0 <|

(<|) :: (a -> b) -> a -> b
f <| x = apply x f



infixl 0 !>
(!>) :: a -> (a -> b) -> b
x !> f = apply' x f

infixr 0 <!
(<!) :: (a -> b) -> a -> b
f <! x = apply' x f

--- | Strict function application.
--- The different between this and 'apply' is that this evaluates its argument
--- before passing it to the function.
apply' :: a -> (a -> b) -> b
apply' x f = seq x (apply x f)