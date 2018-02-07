# NOTE: New development on the `mvp-redesign` branch
> This is the old codebase, left here temporarily...

I’ve learned a lot since I began this project. To such an extent that I’ve realized, everything is too messy, complicated, and hopefully can be vastly simplified, and or improved.

# The Helm Compiler
> Although eventually I would like to support multiple languages, with some conveniences for interoperability.



* Initial compiler infrastructure for Helm -> Rust -> LLVM -> Wasm.
* Very messy & still figuring things out!

* Eventually I would like this to evolve into a CPU/GPU compiler toolchain, using Elm’s api conventions/patterns for the Helm CPU variant.


## NOTE: Recent update:

The syntax, and semantics of Helm are supposed to mimic the Elm language. Although I’ve updated type inference with a notion of function overloading. E.g:
```
(+) : Float -> Float -> Float
(+) left right =
    Sudo.Helm.Native.plusFloat left right


(+) : Int -> Int -> Int
(+) left right =
    Sudo.Helm.Native.plusInt left right
```

I’m considering allowing such functionality in user code… Although perhaps with some rules, to prevent such functions as:

```
(+) : Float -> Int -> Float
(+) left right =
    Sudo.Helm.Native.plus left right
```

This may be, simply:
1.  All overloaded functions must have a type signature (already required for obvious reasons).
    - E.g. `(+) : Int -> Int -> Int`
2. All overloaded functions must be of the same type *(although kinda ambiguous, not sure how else to describe the pattern)
    - E.g.
        - `(+) : Float -> Float -> Float`
        - `(+) : Int -> Int -> Int`
3. Unless, in the context of #2
    1. All overloaded occurrences of an identifier reduce to the same type.
        - E.g.
            - `(==) : Int -> Int -> Bool`
            - `(==) : Float -> Float -> Bool`


Lastly, regarding function overloading, perhaps this can include some interesting error/feedback reporting facilities (something to explore one day)…



## Currently takes input like this:
```

type Either a b
    = Left a
    | Right b

type Maybe a
    = Nothing
    | Just a

type User = Anonymous | Named String

type Visibility = All | Active | Completed


(+) : Int -> Int -> Int
(+) left right =
    Sudo.Helm.Native.plus left right

(-) : Int -> Int -> Int
(-) left right =
    Sudo.Helm.Native.sub left right

(==) : Int -> Int -> Bool
(==) left right =
    Sudo.Helm.Native.isEq left right


alpha x =
    case x of
        Nothing -> 1
        Just 1  -> 0
        Just x  -> x


test1 =
    if True then
        Left "Lorem"
    else
        Right "Lorem"

id x = x

delta1 z =
    let
        id x y = x
        
        x : Int
        x = id (0 + z) 0
        
        y : Int
        y = 1
    in
        x + y + z

delta2 z =
    let
        id x y = x
        
        x : Int
        x = id (0 + z) 0
        
        y : Int
        y = 1
    in
        x + y


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


## And turns such into this:
> NOTE: May be outdated by a lot…

```
enum Either <a, b> {
    Left (a),
    Right (b)
}

enum Maybe <a> {
    Nothing,
    Just (a)
}

enum User {
    Anonymous,
    Named (&'static str)
}

enum Visibility {
    All,
    Active,
    Completed
}


fn g0 (g1: u32, g2: u32) -> u32 {
    Sudo::Helm::Native::plus(g1, g2)
}

fn g3 (g4: u32, g5: u32) -> u32 {
    Sudo::Helm::Native::sub(g4, g5)
}

fn g6 (g7: u32, g8: u32) -> bool {
    Sudo::Helm::Native::isEq(g7, g8)
}

fn g9 (g10: Maybe <u32>) -> u32 {
    match g10 {
        Maybe::Nothing =>
            1,
        Maybe::Just (1) =>
            0,
        Maybe::Just (g11) =>
            g11
    }
}

fn g12 () -> Either <&'static str, &'static str> {
    match true {
        true =>
            Either::Left("Lorem"),
        false =>
            Either::Right("Lorem")
    }
}

fn g13 <b> (g14: b) -> b {
    g14
}

fn g15 <b, c> (g16: b, g17: c) -> b {
    g16
}

fn g18 <e> (g19: u32, g20: &Fn(u32, u32) -> e) -> e {
    g20(g0(0, g19), 0)
}

fn g21 () -> u32 {
    1
}

fn g22 (g23: u32) -> u32 {
    g0(g0(g18(g23, &g15), g21()), g23)
}

fn g24 <b, c> (g25: b, g26: c) -> b {
    g25
}

fn g27 <e> (g28: u32, g29: &Fn(u32, u32) -> e) -> e {
    g29(g0(0, g28), 0)
}

fn g30 () -> u32 {
    1
}

fn g31 (g32: u32) -> u32 {
    g0(g27(g32, &g24), g30())
}

fn g33 () -> bool {
    true
}

fn g34 () -> bool {
    true
}

fn g35 () -> bool {
    true
}

fn g36 () -> bool {
    true
}

fn g37 () -> u32 {
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

fn g38 <c, d, e> (g39: &Fn(c, d) -> e, g40: c, g41: d) -> e {
    g39(g40, g41)
}

fn g42 <c, d> (g43: &Fn(c) -> d, g44: c) -> d {
    g43(g44)
}

fn g45 <f> (g46: f) -> f {
    g42(&g13, g46)
}

fn g47 () -> u32 {
    g45(0)
}
```




# TODO (Soonish):
- (CGIR) Rust
    - Tail call elimination 
    - Wrap recursive types with Box.

NOTE: Eventually I would like to add more primitive types such as:

```
data ScalarType
    = Char
    | String
    | Bool
    | I8
    | I16
    | I32
    | I64
    | I128
    | U8
    | U16
    | U32
    | U64
    | U128
    | F32
    | F64
```

# Not yet supported (that immediately comes to mind)
- Records (at some point in the compilation pipeline)
- Type aliases
- Friendly, or helpful error messages. I.e. Type inferences errors will currently say something like ‘unification fail’, or whatnot…
- Whatever feature doesn’t compile ;)
- Module System: At this stage, just to keep things simple while figuring out/implementing core features, comping single files for now…

Note: theres lots of unused dependancies, something I need to clean up…

# The Compilation Pipeline Terminology and Organization

- `SLIR` = ‘Syntax Level’ Intermediate Representation
- `HLIR` = ‘High Level’ Intermediate Representation
- `LLIR` = ‘Low Level’ Intermediate Representation
- `CGIR` = 'Code-gen' Intermediate Representation


## (`SLIR`) ‘Syntax Level’ Intermediate Representation

* Intended to resemble the parsed source code, and so convey much more information than would otherwise be necessary for, e.g. optimization passes. Intended for analysis/validation passes of the clients source code.

* (Once I actually finish, or implement such aforementioned passes) I’m thinking that, to keep things simple, and organized, after this stage, downstream components should expect a syntactically valid AST.


## (`HLIR`) ‘High Level’ Intermediate Representation
- Desugared IR implementations.

- The intended utility is to be relatively simplistic, lacking any redundant syntactical sugar, and etc… For high level optimization transformations, and etc...



## (`LLIR`) ‘Low Level’ Intermediate Representation
- Intended to be closer to actual computing norms. I.e. structured or imperative paradigms, and transformations thereof.


## (`CGIR`) 'Code-gen' Intermediate Representation

- ‘Compiler target’ implementations are organized under this namespace.






