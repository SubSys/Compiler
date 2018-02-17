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

Note that, the above representation may be significantly outdated.


## Not yet supported
> (That immediately comes to mind.)

- Syntax:
    - Records: Values, types, and syntactic sugar.
- General compiler:
    - Friendly error messages, especially including type inference errors.
    - Operator associativity resolution, and the like (though lots of parens work).
- Rust Codegen:
    - recursive data types (easy)
    - recursive functions I.e. Tail Call Optimization (not so easy)
    - lists?…

Note that, at this stage, there are a multitude of missing features.




## General new ideas regarding development
- Lambda lifting at the end of the frontend IR (HelmSyntax). The thinking is that this will yet further simplify the subsequent IR, and therefore compiler passes.

- So for context, I’ve been wondering: Regarding an AST implementation of lambda calculus. Why are nested sequences of expression applications so prevalent over conveying the equivalent information in one constructor?

    E.g. `(App (App (Var +) 1) 2)` Vs. `(Call + 1 2)`

    Since, while the former would be convent for parser output. Transforming such into the latter would seem to be a simpler medium for AST traversals, given the extra context we get. E.g. `f (Call + [1, 2]) = …`, over `f (App x y) = …`.

    Such that I’m considering dropping the former, in favor of the latter, for the first *core* IR.


- Regarding the frontend, before desugaring. After type inference resolving, don’t just propagate the types of function declarations, but for all binders, and therefore, every value can easily have it’s type looked up.
    - Essential for:
        - Replacing the syntax directed desugarer (called SDD) component, with a simpler, and saner alternative. I.e. For resolving overloaded, and therefore superposed*(need to document) functions.
        - One day perhaps, editor/development tooling information! Oddly, last time I checked, the Elm compiler doesn’t do this.
            - For instance, select an expression and know, not just it’s general type, but it’s inferred type for a given context. For instance, polymorphic type variables can be replaced with a perhaps more helpful, specific type for a given context.


## Long term, and vague aspirations:
- Explore the idea of a multi-language compilation toolchain, and runtime. Except, instead of thinking in terms of function FFI/interoperability for instance. Standardize on an Erlang style ‘processes’ model, and messaging medium. So that for instance, a language that exceptionally efficient with various regards for data processing may be utilized; without the as of yet, inconveniences of our current software norms, and practices. As in, extend the idea of reusable libraries to reusable processes.
- Treat processes as capable data stores, don’t use a standard filesystem interface. 

- A little while ago I was wondering if I could define batch style data processing pipelines for *massive datasets* without getting bogged down with performance considerations, such as paralleling work when feasible.

    Specifically, if I could streamline the development of compilers with a sort of dedicated language based on something other than manual and tedious recursion… With various accommodating syntactic conveniences (e.g. specifying dependent passes), against potentially mutually dependent schemas. So that common functionality such as lambda lifting, or defunctionalization schemes, may be more simply adapted and reused for new languages. The thinking is a medium, with two subsystems, one for defining transformations and analyses against datasets, the other, defining runtime characteristics without adapting the actual algorithms to accommodate such ideas, like ‘nested data parallelism’, memorization/caching, or resumable computation. The other dimension, is for exposing a medium for library developers to implement application specific, compile-time optimizations, or validations; while mitigating increased compilation times with the aforementioned, transparent runtime facilities.


