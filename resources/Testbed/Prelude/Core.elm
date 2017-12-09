module Core exposing (..)


import Native.Core
import Native.Core.List


infixl 6 +
infixl 6 -
infixl 6 *
infixl 6 /

infix 4 ==
infix 4 /=
infix 4 <
infix 4 >
infix 4 <=
infix 4 >=

infixr 3 &&
infixr 2 ||

{-|-}
(+) : number -> number -> number
(+) =
  Native.Core.add

{-|-}
(-) : number -> number -> number
(-) =
  Native.Core.sub

{-|-}
(*) : number -> number -> number
(*) =
  Native.Core.mul


{-| Floating point division. -}
(/) : Float -> Float -> Float
(/) =
  Native.Core.floatDiv



(++) : appendable -> appendable -> appendable
(++) =
  Native.Core.append


(==) : a -> a -> Bool
(==) =
  Native.Core.equal

(/=) : a -> a -> Bool
(/=) =
  Native.Core.notEqual


{-|-}
(<) : comparable -> comparable -> Bool
(<) =
  Native.Core.lt


{-|-}
(>) : comparable -> comparable -> Bool
(>) =
  Native.Core.gt


{-|-}
(<=) : comparable -> comparable -> Bool
(<=) =
  Native.Core.le


{-|-}
(>=) : comparable -> comparable -> Bool
(>=) =
  Native.Core.ge


(&&) : Bool -> Bool -> Bool
(&&) =
  Native.Core.and

(||) : Bool -> Bool -> Bool
(||) =
  Native.Core.or



(::) : a -> List a -> List a
(::) =
    Native.Core.List.cons


infixr 5 ::




type Color = Red | Green | Blue


type ColorWrapper a = ColorPayload a


alpha : Color -> ColorWrapper Color
alpha x =
    case x of
        Red -> ColorPayload Red
        Green -> ColorPayload Green
        Blue -> ColorPayload Blue




type Name = String


beta x = x + x


gamma x =
    let
        x1 x = 1 + x
    in
        x1 x




fibonacci : Int -> Int
fibonacci n =
    case n of
        1 -> 1
        2 -> 1
        _ -> fibonacci (n-1) + fibonacci (n-2)



repeat : Int -> a -> List a
repeat n x =
    if n <= 0 then
        []
    else 
        x :: repeat (n-1) x