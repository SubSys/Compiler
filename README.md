# The Helm Compiler

# Example dataset
> (To showcase current progress)

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
    Helm.Compiler.Sudo.Native.plusInt leftA rightA


(+) : Float -> Float -> Float
(+) leftA rightA =
    Helm.Compiler.Sudo.Native.plusFloat leftA rightA


(-) : Int -> Int -> Int
(-) leftA rightA =
    Helm.Compiler.Sudo.Native.subInt leftA rightA


(-) : Float -> Float -> Float
(-) leftA rightA =
    Helm.Compiler.Sudo.Native.subFloat leftA rightA



(==) : Int -> Int -> Bool
(==) leftA rightA =
    Helm.Compiler.Sudo.Native.isEqInt leftA rightA

(==) : Float -> Float -> Bool
(==) leftA rightA =
    Helm.Compiler.Sudo.Native.isEqFloat leftA rightA

(==) : String -> String -> Bool
(==) leftA rightA =
    Helm.Compiler.Sudo.Native.isEqString leftA rightA

(==) : Char -> Char -> Bool
(==) leftA rightA =
    Helm.Compiler.Sudo.Native.isEqChar leftA rightA



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


## The above is compiled into:

```rust
enum Maybe <A> {
    Nothing (),
    Just (A)
}

enum Either <A, B> {
    Left (A),
    Right (B)
}

enum User {
    Anonymous (),
    Named (&'static str)
}

enum Visibility {
    All (),
    Active (),
    Completed ()
}




fn f0 (f1: u32, f2: u32) -> u32 {
    Helm::Compiler::Sudo::Native::plusInt(f1, f2)
}

fn f3 (f4: u32, f5: u32) -> u32 {
    f0(f5, f4)
}

fn f6 (f7: u32) -> u32 {
    f0(f7, f7)
}

fn f8 (f9: u32, f10: u32) -> u32 {
    f0(f10, f9)
}

fn f11 (f12: f32, f13: f32) -> f32 {
    Helm::Compiler::Sudo::Native::subFloat(f12, f13)
}

fn f14 (f15: f32, f16: f32) -> bool {
    Helm::Compiler::Sudo::Native::isEqFloat(f15, f16)
}

fn f17 (f18: &'static str, f19: &'static str) -> bool {
    Helm::Compiler::Sudo::Native::isEqString(f18, f19)
}

fn f20 (f21: char, f22: char) -> bool {
    Helm::Compiler::Sudo::Native::isEqChar(f21, f22)
}

fn f23 (f24: u32) -> u32 {
    f3(f24, 1)
}

fn f25 (f26: u32, f27: u32) -> u32 {
    f0(f26, f27)
}

fn f28 (f29: u32, f30: u32) -> u32 {
    Helm::Compiler::Sudo::Native::subInt(f29, f30)
}

fn f31 (f32: u32, f33: u32) -> bool {
    Helm::Compiler::Sudo::Native::isEqInt(f32, f33)
}

fn f34 (f35: u32) -> u32 {
    match f31(f35, 0) {
        true =>
            0,
        false =>
            match f31(f35, 1) {
                true =>
                    1,
                false =>
                    f0(f34(f28(f35, 1)), f34(f28(f35, 2)))
            }
    }
}

fn f36 () -> u32 {
    f34(0)
}

fn f37 <AM> (f39: AM) -> AM {
    f39
}

fn f40 <AP, AQ> (f43: AP, f44: AQ) -> AP {
    f43
}

fn f45 (f46: u32) -> u32 {
    f40(f0(0, f46), 0)
}

fn f47 () -> u32 {
    1
}

fn f48 (f49: u32) -> u32 {
    f0(f0(f45(f49), f47()), f49)
}

fn f50 () -> u32 {
    f37(0)
}

fn f51 () -> u32 {
    1
}

fn f52 <BB> (f54: BB) -> u32 {
    f0(f50(), f51())
}

fn f55 <BE, BF> (f58: BE, f59: BF) -> BE {
    f58
}

fn f60 <BJ, BK> (f63: BJ, f64: BK) -> BJ {
    f63
}

fn f65 () -> u32 {
    f0(1, 1)
}

fn f66 () -> u32 {
    2
}

fn f67 () -> u32 {
    f0(f65(), f66())
}

fn f68 () -> Either <&'static str, &'static str> {
    match true {
        true =>
            Either::Left("True"),
        false =>
            Either::Right("False")
    }
}

fn f69 () -> bool {
    true
}

fn f70 () -> bool {
    true
}

fn f71 () -> bool {
    true
}

fn f72 () -> bool {
    true
}

fn f73 () -> u32 {
    match true {
        true =>
            1,
        false =>
            match false {
                true =>
                    2,
                false =>
                    match false {
                        true =>
                            3,
                        false =>
                            4
                    }
            }
    }
}

fn f74 <BX, BY, BZ> (f78: &Fn(BX, BY) -> BZ, f79: BX, f80: BY) -> BZ {
    f78(f79, f80)
}

fn f81 <CE, CF> (f84: &Fn(CE) -> CF, f85: CE) -> CF {
    f84(f85)
}

fn f86 <CJ> (f88: CJ) -> u32 {
    f6(1)
}

fn f89 <CM> (f91: CM) -> CM {
    f81(&f37, f91)
}

fn f92 () -> u32 {
    f89(0)
}

fn f93 (f94: u32) -> u32 {
    f8(f94, 1)
}

fn f95 (f96: f32, f97: f32) -> f32 {
    Helm::Compiler::Sudo::Native::plusFloat(f96, f97)
}

```

