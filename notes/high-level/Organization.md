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
