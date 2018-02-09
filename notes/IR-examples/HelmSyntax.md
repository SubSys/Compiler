# Input File:
```elm
module One exposing (..)


import Helm.Compiler.Sudo.Native



type Maybe a
    = Nothing
    | Just a

type Either a b
    = Left a
    | Right b

type User = Anonymous | Named String

type Visibility = All | Active | Completed





(+) : Int -> Int -> Int
(+) leftA rightA =
    Sudo.Helm.Native.plus leftA rightA


(+) : Float -> Float -> Float
(+) leftA rightA =
    Sudo.Helm.Native.plus leftA rightA


(-) : Int -> Int -> Int
(-) leftA rightA =
    Sudo.Helm.Native.sub leftA rightA


(-) : Float -> Float -> Float
(-) leftA rightA =
    Sudo.Helm.Native.sub leftA rightA



(==) : Int -> Int -> Bool
(==) leftA rightA =
    Sudo.Helm.Native.isEqInt leftA rightA

(==) : Float -> Float -> Bool
(==) leftA rightA =
    Sudo.Helm.Native.isEqFloat leftA rightA

(==) : String -> String -> Bool
(==) leftA rightA =
    Sudo.Helm.Native.isEqString leftA rightA

(==) : Char -> Char -> Bool
(==) leftA rightA =
    Sudo.Helm.Native.isEqChar leftA rightA



plusA x y =
    x + y

alpha1 x =
    plusA x 1

alpha2 x =
    x + x

alpha3 x =
    alpha2 1


alpha4 x y =
    x + y

alpha5 x =
    alpha4 x 1


alpha6 x =
    x - 1.0

testEqualA x y =
    x == y


testEqualB x =
    testEqualA x 1

testEqualC x =
    testEqualA 1 x

testEqualD x =
    testEqualA 1 x

testEqualE x =
    1 == x


testEqualF x =
    x == 1

testEqualG =
    1 == 1


plusInt : Int -> Int -> Int
plusInt x y =
    x + y


fib n =
    if n == 0 then
        0
    else if n == 1 then
        1
    else
        (fib (n - 1)) + (fib (n - 2))


beta : Int
beta =
    fib 0

id x = x

delta1 z =
    let
        passA x y = x
        
        x : Int
        x = passA (0 + z) 0
        
        y : Int
        y = 1
    in
        x + y + z


delta2 z =
    let
        passB x y = x
        
        x : Int
        x = id 0
        
        y : Int
        y = 1
        
        passX x y = x
    in
        x + y


delta3 =
    let
        x = 1 + 1
        y = 2
    in
        x + y

typeTest =
    if True then
        Left "True"
    else
        Right "False"

x1 = True
x2 = True
x3 = True
x4 = True

gamma =
    if True then
        1
    else if False then
        2
    else if False then
        3
    else
        4


omega x y z =
    x y z

phi f x =
    f x


test = phi id

test2 = test 0
```


# Parsed Source Code
## Pretty printed (Functions and Unions):
```
type Maybe a
    = Nothing
    | Just a

type Either a b
    = Left a
    | Right b

type User
    = Anonymous
    | Named String

type Visibility
    = All
    | Active
    | Completed


(+) : Int -> Int -> Int
(+) leftA rightA =
    ((Sudo.Helm.Native.plus leftA) rightA)

(+) : Float -> Float -> Float
(+) leftA rightA =
    ((Sudo.Helm.Native.plus leftA) rightA)

(-) : Int -> Int -> Int
(-) leftA rightA =
    ((Sudo.Helm.Native.sub leftA) rightA)

(-) : Float -> Float -> Float
(-) leftA rightA =
    ((Sudo.Helm.Native.sub leftA) rightA)

(==) : Int -> Int -> Bool
(==) leftA rightA =
    ((Sudo.Helm.Native.isEqInt leftA) rightA)

(==) : Float -> Float -> Bool
(==) leftA rightA =
    ((Sudo.Helm.Native.isEqFloat leftA) rightA)

(==) : String -> String -> Bool
(==) leftA rightA =
    ((Sudo.Helm.Native.isEqString leftA) rightA)

(==) : Char -> Char -> Bool
(==) leftA rightA =
    ((Sudo.Helm.Native.isEqChar leftA) rightA)

plusA x y =
    x + y

alpha1 x =
    ((plusA x) 1)

alpha2 x =
    x + x

alpha3 x =
    (alpha2 1)

alpha4 x y =
    x + y

alpha5 x =
    ((alpha4 x) 1)

alpha6 x =
    x - 1.0

testEqualA x y =
    x == y

testEqualB x =
    ((testEqualA x) 1)

testEqualC x =
    ((testEqualA 1) x)

testEqualD x =
    ((testEqualA 1) x)

testEqualE x =
    1 == x

testEqualF x =
    x == 1

testEqualG =
    1 == 1

plusInt : Int -> Int -> Int
plusInt x y =
    x + y

fib n =
    if n == 0 then
        0
    if n == 1 then
        1
    else
        ((fib (n - 1))) + ((fib (n - 2)))

beta : Int
beta =
    (fib 0)

id x =
    x

delta1 z =
    let passA x y =
            x


        x : Int
        x =
            ((passA (0 + z)) 0)


        y : Int
        y =
            1

    in
        x + y + z

delta2 z =
    let passB x y =
            x


        x : Int
        x =
            (id 0)


        y : Int
        y =
            1


        passX x y =
            x

    in
        x + y

delta3 =
    let x =
            1 + 1


        y =
            2

    in
        x + y

typeTest =
    if True then
        (Left "True")
    else
        (Right "False")

x1 =
    True

x2 =
    True

x3 =
    True

x4 =
    True

gamma =
    if True then
        1
    if False then
        2
    if False then
        3
    else
        4

omega x y z =
    ((x y) z)

phi f x =
    (f x)

test =
    (phi id)

test2 =
    (test 0)

```


## Raw:
> Note that, currently: the `Meta` constructor implements an empty string for Show.

```haskell
Union (Ident "Maybe" Nothing)
      [Ident "a" Nothing]
      [Constructor (Ident "Nothing" Nothing)
                   []
      ,Constructor (Ident "Just" Nothing)
                   [VarType (Ident "a" Nothing)]]
Union (Ident "Either" Nothing)
      [Ident "a" Nothing
      ,Ident "b" Nothing]
      [Constructor (Ident "Left" Nothing)
                   [VarType (Ident "a" Nothing)]
      ,Constructor (Ident "Right" Nothing)
                   [VarType (Ident "b" Nothing)]]
Union (Ident "User" Nothing)
      []
      [Constructor (Ident "Anonymous" Nothing)
                   []
      ,Constructor (Ident "Named" Nothing)
                   [StringType]]
Union (Ident "Visibility" Nothing)
      []
      [Constructor (Ident "All" Nothing)
                   []
      ,Constructor (Ident "Active" Nothing)
                   []
      ,Constructor (Ident "Completed" Nothing)
                   []]
Function (Binder (Ident "+" Nothing)
                 Nothing)
         [Binder (Ident "leftA" Nothing)
                 Nothing
         ,Binder (Ident "rightA" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "plus" (Just (Namespace ["Sudo"
                                                                   ,"Helm"
                                                                   ,"Native"]))))
                           (VarExpr (Ident "leftA" Nothing)))
                  (VarExpr (Ident "rightA" Nothing)))
         (Unresolved (ArrType (IntType)
                              (ArrType (IntType) (IntType))))
Function (Binder (Ident "+" Nothing)
                 Nothing)
         [Binder (Ident "leftA" Nothing)
                 Nothing
         ,Binder (Ident "rightA" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "plus" (Just (Namespace ["Sudo"
                                                                   ,"Helm"
                                                                   ,"Native"]))))
                           (VarExpr (Ident "leftA" Nothing)))
                  (VarExpr (Ident "rightA" Nothing)))
         (Unresolved (ArrType (FloatType)
                              (ArrType (FloatType)
                                       (FloatType))))
Function (Binder (Ident "-" Nothing)
                 Nothing)
         [Binder (Ident "leftA" Nothing)
                 Nothing
         ,Binder (Ident "rightA" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "sub" (Just (Namespace ["Sudo"
                                                                  ,"Helm"
                                                                  ,"Native"]))))
                           (VarExpr (Ident "leftA" Nothing)))
                  (VarExpr (Ident "rightA" Nothing)))
         (Unresolved (ArrType (IntType)
                              (ArrType (IntType) (IntType))))
Function (Binder (Ident "-" Nothing)
                 Nothing)
         [Binder (Ident "leftA" Nothing)
                 Nothing
         ,Binder (Ident "rightA" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "sub" (Just (Namespace ["Sudo"
                                                                  ,"Helm"
                                                                  ,"Native"]))))
                           (VarExpr (Ident "leftA" Nothing)))
                  (VarExpr (Ident "rightA" Nothing)))
         (Unresolved (ArrType (FloatType)
                              (ArrType (FloatType)
                                       (FloatType))))
Function (Binder (Ident "==" Nothing)
                 Nothing)
         [Binder (Ident "leftA" Nothing)
                 Nothing
         ,Binder (Ident "rightA" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "isEqInt" (Just (Namespace ["Sudo"
                                                                      ,"Helm"
                                                                      ,"Native"]))))
                           (VarExpr (Ident "leftA" Nothing)))
                  (VarExpr (Ident "rightA" Nothing)))
         (Unresolved (ArrType (IntType)
                              (ArrType (IntType) (BoolType))))
Function (Binder (Ident "==" Nothing)
                 Nothing)
         [Binder (Ident "leftA" Nothing)
                 Nothing
         ,Binder (Ident "rightA" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "isEqFloat" (Just (Namespace ["Sudo"
                                                                        ,"Helm"
                                                                        ,"Native"]))))
                           (VarExpr (Ident "leftA" Nothing)))
                  (VarExpr (Ident "rightA" Nothing)))
         (Unresolved (ArrType (FloatType)
                              (ArrType (FloatType)
                                       (BoolType))))
Function (Binder (Ident "==" Nothing)
                 Nothing)
         [Binder (Ident "leftA" Nothing)
                 Nothing
         ,Binder (Ident "rightA" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "isEqString" (Just (Namespace ["Sudo"
                                                                         ,"Helm"
                                                                         ,"Native"]))))
                           (VarExpr (Ident "leftA" Nothing)))
                  (VarExpr (Ident "rightA" Nothing)))
         (Unresolved (ArrType (StringType)
                              (ArrType (StringType)
                                       (BoolType))))
Function (Binder (Ident "==" Nothing)
                 Nothing)
         [Binder (Ident "leftA" Nothing)
                 Nothing
         ,Binder (Ident "rightA" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "isEqChar" (Just (Namespace ["Sudo"
                                                                       ,"Helm"
                                                                       ,"Native"]))))
                           (VarExpr (Ident "leftA" Nothing)))
                  (VarExpr (Ident "rightA" Nothing)))
         (Unresolved (ArrType (CharType)
                              (ArrType (CharType)
                                       (BoolType))))
Function (Binder (Ident "plusA" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing
         ,Binder (Ident "y" Nothing)
                 Nothing]
         (InfixAppExpr (Ident "+" Nothing)
                       (VarExpr (Ident "x" Nothing))
                       (VarExpr (Ident "y" Nothing)))
         Unknown
Function (Binder (Ident "alpha1" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "plusA" Nothing))
                           (VarExpr (Ident "x" Nothing)))
                  (LitExpr (IntLit 1)))
         Unknown
Function (Binder (Ident "alpha2" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (InfixAppExpr (Ident "+" Nothing)
                       (VarExpr (Ident "x" Nothing))
                       (VarExpr (Ident "x" Nothing)))
         Unknown
Function (Binder (Ident "alpha3" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (AppExpr (VarExpr (Ident "alpha2" Nothing))
                  (LitExpr (IntLit 1)))
         Unknown
Function (Binder (Ident "alpha4" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing
         ,Binder (Ident "y" Nothing)
                 Nothing]
         (InfixAppExpr (Ident "+" Nothing)
                       (VarExpr (Ident "x" Nothing))
                       (VarExpr (Ident "y" Nothing)))
         Unknown
Function (Binder (Ident "alpha5" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "alpha4" Nothing))
                           (VarExpr (Ident "x" Nothing)))
                  (LitExpr (IntLit 1)))
         Unknown
Function (Binder (Ident "alpha6" Nothing ) Nothing) [Binder (Ident "x" Nothing ) Nothing] (InfixAppExpr (Ident "-" Nothing ) (VarExpr (Ident "x" Nothing ) ) (LitExpr (FloatLit 1.0 ) ) ) Unknown
Function (Binder (Ident "testEqualA" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing
         ,Binder (Ident "y" Nothing)
                 Nothing]
         (InfixAppExpr (Ident "==" Nothing)
                       (VarExpr (Ident "x" Nothing))
                       (VarExpr (Ident "y" Nothing)))
         Unknown
Function (Binder (Ident "testEqualB" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "testEqualA" Nothing))
                           (VarExpr (Ident "x" Nothing)))
                  (LitExpr (IntLit 1)))
         Unknown
Function (Binder (Ident "testEqualC" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "testEqualA" Nothing))
                           (LitExpr (IntLit 1)))
                  (VarExpr (Ident "x" Nothing)))
         Unknown
Function (Binder (Ident "testEqualD" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "testEqualA" Nothing))
                           (LitExpr (IntLit 1)))
                  (VarExpr (Ident "x" Nothing)))
         Unknown
Function (Binder (Ident "testEqualE" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (InfixAppExpr (Ident "==" Nothing)
                       (LitExpr (IntLit 1))
                       (VarExpr (Ident "x" Nothing)))
         Unknown
Function (Binder (Ident "testEqualF" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (InfixAppExpr (Ident "==" Nothing)
                       (VarExpr (Ident "x" Nothing))
                       (LitExpr (IntLit 1)))
         Unknown
Function (Binder (Ident "testEqualG" Nothing)
                 Nothing)
         []
         (InfixAppExpr (Ident "==" Nothing)
                       (LitExpr (IntLit 1))
                       (LitExpr (IntLit 1)))
         Unknown
Function (Binder (Ident "plusInt" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing
         ,Binder (Ident "y" Nothing)
                 Nothing]
         (InfixAppExpr (Ident "+" Nothing)
                       (VarExpr (Ident "x" Nothing))
                       (VarExpr (Ident "y" Nothing)))
         (Unresolved (ArrType (IntType)
                              (ArrType (IntType) (IntType))))
Function (Binder (Ident "fib" Nothing)
                 Nothing)
         [Binder (Ident "n" Nothing)
                 Nothing]
         (IfExpr [(InfixAppExpr (Ident "==" Nothing)
                                (VarExpr (Ident "n" Nothing))
                                (LitExpr (IntLit 0))
                  ,LitExpr (IntLit 0))
                 ,(InfixAppExpr (Ident "==" Nothing)
                                (VarExpr (Ident "n" Nothing))
                                (LitExpr (IntLit 1))
                  ,LitExpr (IntLit 1))]
                 (InfixAppExpr (Ident "+" Nothing)
                               (ParensExpr (AppExpr (VarExpr (Ident "fib" Nothing))
                                                    (ParensExpr (InfixAppExpr (Ident "-" Nothing)
                                                                              (VarExpr (Ident "n" Nothing))
                                                                              (LitExpr (IntLit 1))))))
                               (ParensExpr (AppExpr (VarExpr (Ident "fib" Nothing))
                                                    (ParensExpr (InfixAppExpr (Ident "-" Nothing)
                                                                              (VarExpr (Ident "n" Nothing))
                                                                              (LitExpr (IntLit 2))))))))
         Unknown
Function (Binder (Ident "beta" Nothing)
                 Nothing)
         []
         (AppExpr (VarExpr (Ident "fib" Nothing))
                  (LitExpr (IntLit 0)))
         (Unresolved (IntType))
Function (Binder (Ident "id" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing]
         (VarExpr (Ident "x" Nothing))
         Unknown
Function (Binder (Ident "delta1" Nothing)
                 Nothing)
         [Binder (Ident "z" Nothing)
                 Nothing]
         (LetExpr [Function (Binder (Ident "passA" Nothing)
                                    Nothing)
                            [Binder (Ident "x" Nothing)
                                    Nothing
                            ,Binder (Ident "y" Nothing)
                                    Nothing]
                            (VarExpr (Ident "x" Nothing))
                            Unknown
                  ,Function (Binder (Ident "x" Nothing)
                                    Nothing)
                            []
                            (AppExpr (AppExpr (VarExpr (Ident "passA" Nothing))
                                              (ParensExpr (InfixAppExpr (Ident "+" Nothing)
                                                                        (LitExpr (IntLit 0))
                                                                        (VarExpr (Ident "z" Nothing)))))
                                     (LitExpr (IntLit 0)))
                            (Unresolved (IntType))
                  ,Function (Binder (Ident "y" Nothing)
                                    Nothing)
                            []
                            (LitExpr (IntLit 1))
                            (Unresolved (IntType))]
                  (InfixAppExpr (Ident "+" Nothing)
                                (InfixAppExpr (Ident "+" Nothing)
                                              (VarExpr (Ident "x" Nothing))
                                              (VarExpr (Ident "y" Nothing)))
                                (VarExpr (Ident "z" Nothing))))
         Unknown
Function (Binder (Ident "delta2" Nothing)
                 Nothing)
         [Binder (Ident "z" Nothing)
                 Nothing]
         (LetExpr [Function (Binder (Ident "passB" Nothing)
                                    Nothing)
                            [Binder (Ident "x" Nothing)
                                    Nothing
                            ,Binder (Ident "y" Nothing)
                                    Nothing]
                            (VarExpr (Ident "x" Nothing))
                            Unknown
                  ,Function (Binder (Ident "x" Nothing)
                                    Nothing)
                            []
                            (AppExpr (VarExpr (Ident "id" Nothing))
                                     (LitExpr (IntLit 0)))
                            (Unresolved (IntType))
                  ,Function (Binder (Ident "y" Nothing)
                                    Nothing)
                            []
                            (LitExpr (IntLit 1))
                            (Unresolved (IntType))
                  ,Function (Binder (Ident "passX" Nothing)
                                    Nothing)
                            [Binder (Ident "x" Nothing)
                                    Nothing
                            ,Binder (Ident "y" Nothing)
                                    Nothing]
                            (VarExpr (Ident "x" Nothing))
                            Unknown]
                  (InfixAppExpr (Ident "+" Nothing)
                                (VarExpr (Ident "x" Nothing))
                                (VarExpr (Ident "y" Nothing))))
         Unknown
Function (Binder (Ident "delta3" Nothing)
                 Nothing)
         []
         (LetExpr [Function (Binder (Ident "x" Nothing)
                                    Nothing)
                            []
                            (InfixAppExpr (Ident "+" Nothing)
                                          (LitExpr (IntLit 1))
                                          (LitExpr (IntLit 1)))
                            Unknown
                  ,Function (Binder (Ident "y" Nothing)
                                    Nothing)
                            []
                            (LitExpr (IntLit 2))
                            Unknown]
                  (InfixAppExpr (Ident "+" Nothing)
                                (VarExpr (Ident "x" Nothing))
                                (VarExpr (Ident "y" Nothing))))
         Unknown
Function (Binder (Ident "typeTest" Nothing)
                 Nothing)
         []
         (IfExpr [(LitExpr (BoolLit True)
                  ,AppExpr (ConstrExpr (Ident "Left" Nothing))
                           (LitExpr (StringLit "True")))]
                 (AppExpr (ConstrExpr (Ident "Right" Nothing))
                          (LitExpr (StringLit "False"))))
         Unknown
Function (Binder (Ident "x1" Nothing)
                 Nothing)
         []
         (LitExpr (BoolLit True))
         Unknown
Function (Binder (Ident "x2" Nothing)
                 Nothing)
         []
         (LitExpr (BoolLit True))
         Unknown
Function (Binder (Ident "x3" Nothing)
                 Nothing)
         []
         (LitExpr (BoolLit True))
         Unknown
Function (Binder (Ident "x4" Nothing)
                 Nothing)
         []
         (LitExpr (BoolLit True))
         Unknown
Function (Binder (Ident "gamma" Nothing)
                 Nothing)
         []
         (IfExpr [(LitExpr (BoolLit True)
                  ,LitExpr (IntLit 1))
                 ,(LitExpr (BoolLit False)
                  ,LitExpr (IntLit 2))
                 ,(LitExpr (BoolLit False)
                  ,LitExpr (IntLit 3))]
                 (LitExpr (IntLit 4)))
         Unknown
Function (Binder (Ident "omega" Nothing)
                 Nothing)
         [Binder (Ident "x" Nothing)
                 Nothing
         ,Binder (Ident "y" Nothing)
                 Nothing
         ,Binder (Ident "z" Nothing)
                 Nothing]
         (AppExpr (AppExpr (VarExpr (Ident "x" Nothing))
                           (VarExpr (Ident "y" Nothing)))
                  (VarExpr (Ident "z" Nothing)))
         Unknown
Function (Binder (Ident "phi" Nothing)
                 Nothing)
         [Binder (Ident "f" Nothing)
                 Nothing
         ,Binder (Ident "x" Nothing)
                 Nothing]
         (AppExpr (VarExpr (Ident "f" Nothing))
                  (VarExpr (Ident "x" Nothing)))
         Unknown
Function (Binder (Ident "test" Nothing)
                 Nothing)
         []
         (AppExpr (VarExpr (Ident "phi" Nothing))
                  (VarExpr (Ident "id" Nothing)))
         Unknown
Function (Binder (Ident "test2" Nothing)
                 Nothing)
         []
         (AppExpr (VarExpr (Ident "test" Nothing))
                  (LitExpr (IntLit 0)))
         Unknown
```


## After desugaring:
```
º0 : Int -> Int -> Int
º0 º1 º2 =
    Sudo.Helm.Native.plus｟ º1,º2 ｠

º3 : Int -> Int -> Int
º3 º4 º5 =
    Sudo.Helm.Native.sub｟ º4,º5 ｠

º6 : Int -> Int -> Bool
º6 º7 º8 =
    Sudo.Helm.Native.isEqInt｟ º7,º8 ｠

º9 : Float -> Float -> Float
º9 º10 º11 =
    Sudo.Helm.Native.plus｟ º10,º11 ｠

º12 : Float -> Float -> Float
º12 º13 º14 =
    Sudo.Helm.Native.sub｟ º13,º14 ｠

º15 : Float -> Float -> Bool
º15 º16 º17 =
    Sudo.Helm.Native.isEqFloat｟ º16,º17 ｠

º18 : String -> String -> Bool
º18 º19 º20 =
    Sudo.Helm.Native.isEqString｟ º19,º20 ｠

º21 : Char -> Char -> Bool
º21 º22 º23 =
    Sudo.Helm.Native.isEqChar｟ º22,º23 ｠

º24 : Int -> Int
º24 º25 : Int =
    (((λº26 : Int. (λº27 : Int. º0｟ º26,º27 ｠ : Int)) º25) 1)

º28 : Float -> Float
º28 º29 : Float =
    º12｟ º29,1.0 ｠ : Float

º30 : Int -> Bool
º30 º31 : Int =
    (((λº32 : Int. (λº33 : Int. º6｟ º32,º33 ｠ : Bool)) º31) 1)

º34 : Int -> Bool
º34 º35 : Int =
    (((λº36 : Int. (λº37 : Int. º6｟ º36,º37 ｠ : Bool)) 1) º35)

º38 : Int -> Bool
º38 º39 : Int =
    (((λº40 : Int. (λº41 : Int. º6｟ º40,º41 ｠ : Bool)) 1) º39)

º42 : Int -> Bool
º42 º43 : Int =
    º6｟ 1,º43 ｠ : Bool

º44 : Int -> Bool
º44 º45 : Int =
    º6｟ º45,1 ｠ : Bool

º46 : Bool
º46 =
    º6｟ 1,1 ｠ : Bool

º47 : Int -> Int -> Int
º47 º48 : Int º49 : Int =
    º0｟ º48,º49 ｠ : Int

º50 : Int -> Int
º50 º51 : Int =
    if º6｟ º51,0 ｠ : Bool then
        0
    if º6｟ º51,1 ｠ : Bool then
        1
    else
        º0｟ (º50｟ (º3｟ º51,1 ｠ : Int) ｠ : Int),(º50｟ (º3｟ º51,2 ｠ : Int) ｠ : Int) ｠ : Int

º52 : Int
º52 =
    º50｟ 0 ｠ : Int

º53 : forall !c. !c -> !c
º53 º54 : !c =
    º54

º55 : Int -> Int
º55 º56 : Int =
    let º57 : forall !d !e. !d -> !e -> !d
        º57 º58 : !d º59 : !e =
            º58


        º60 : Int
        º60 =
            º57｟ (º0｟ 0,º56 ｠ : Int),0 ｠ : Int


        º61 : Int
        º61 =
            1

    in
        º0｟ º0｟ º60,º61 ｠ : Int,º56 ｠ : Int

º62 : forall !h. !h -> Int
º62 º63 : !h =
    ((λº64 : Int. º0｟ º64,º64 ｠ : Int) 1)

º65 : forall !t. !t -> Int
º65 º66 : !t =
    let º67 : forall !d !e. !d -> !e -> !d
        º67 º68 : !d º69 : !e =
            º68


        º70 : Int
        º70 =
            º53｟ 0 ｠ : Int


        º71 : Int
        º71 =
            1


        º72 : forall !d !e. !d -> !e -> !d
        º72 º73 : !d º74 : !e =
            º73

    in
        º0｟ º70,º71 ｠ : Int

º75 : Int
º75 =
    let º76 : Int
        º76 =
            º0｟ 1,1 ｠ : Int


        º77 : Int
        º77 =
            2

    in
        º0｟ º76,º77 ｠ : Int

º78 : Either String String
º78 =
    if True then
        Left｟ "True" ｠ : Either String String
    else
        Right｟ "False" ｠ : Either String String

º79 : Bool
º79 =
    True

º80 : Bool
º80 =
    True

º81 : Bool
º81 =
    True

º82 : Bool
º82 =
    True

º83 : Int
º83 =
    if True then
        1
    if False then
        2
    if False then
        3
    else
        4

º84 : forall !g !h !i. (!g -> (!h -> !i)) -> !g -> !h -> !i
º84 º85 : (!g -> (!h -> !i)) º86 : !g º87 : !h =
    º85｟ º86,º87 ｠ : !i

º88 : forall !f !g. (!f -> !g) -> !f -> !g
º88 º89 : (!f -> !g) º90 : !f =
    º89｟ º90 ｠ : !g

º91 : Int -> Int
º91 º92 : Int =
    (((λº93 : Int. (λº94 : Int. º0｟ º93,º94 ｠ : Int)) º92) 1)

º95 : forall !h. !h -> !h
º95 º96 : !h =
    º88｟ º53,º96 ｠ : !h

º97 : Int
º97 =
    º95｟ 0 ｠ : Int
```




