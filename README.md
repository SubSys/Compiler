# The Helm Compiler
> Note: Old codebase on the ‘master’ branch.


## Not yet supported
> (That immediately comes to mind.)

- Syntax:
    - Records: Values, types, and syntactic sugar.
- General compiler:
    - Multi-module compilation:
        -  Almost finished *(I think)*; for an initial, functional version.
    - Friendly error messages, especially including type inference errors.



Note that, at this stage, there are a multitude of missing features.

## Development

The initial compiler frontend (HelmSyntax IR) infrastructure is pretty much finished. To perform a dry-run of the initial compilation pipeline, open `SLIR.HelmSyntax.Dev.DryRun` in `stack ghci` and type `run`. You should see the parsed, processed and rendered syntax printed.


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


## Long term, vague but sincere aspirations:
- Explore the idea of a multi-language compilation toolchain, and runtime. Except, instead of thinking in terms of function FFI/interoperability for instance. Standardize on an Erlang style ‘processes’ model, and messaging medium. So that for instance, a language that exceptionally efficient with various regards for data processing may be utilized; without the as of yet, inconveniences of our current software norms, and practices. As in, extend the idea of reusable libraries to reusable processes.
    - As if to say, given the strange utility of the Unix pipe for std-out/in batch style operations. Extend such a model with the notion of a query, or inquiry about a long running service between mutually untrusting parties.
- Resumable computation on geographically distributed nodes.
- Treat processes as capable data stores, don’t use a standard filesystem interface. 
- Automated management of computer/network infrastructure and software services.
- Runtimes that can tolerate the presence of malicious processes. Likewise, package systems that don’t trust third party libraries/processes.


- A little while ago I was wondering if I could define batch style data processing pipelines for *massive datasets* without getting bogged down with performance considerations, such as paralleling work when feasible.

    Specifically, if I could streamline the development of compilers with a sort of dedicated language based on something other than manual and tedious recursion… With various accommodating syntactic conveniences (e.g. specifying dependent passes), against potentially mutually dependent schemas. So that common functionality such as lambda lifting, or defunctionalization schemes, may be more simply adapted and reused for new languages. The thinking is a medium, with two subsystems, one for defining transformations and analyses against datasets, the other, defining runtime characteristics without adapting the actual algorithms to accommodate such ideas, like ‘nested data parallelism’, memorization/caching, or resumable computation. The other dimension, is for exposing a medium for library developers to implement application specific, compile-time optimizations, or validations; while mitigating increased compilation times with the aforementioned, transparent runtime facilities.


