{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Pass.Formalize (
    input
) where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)

import Prelude (mapM_, IO, String, return)

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Set  as Set


import qualified Data.Generics.Uniplate.Data as Uni

--- Framework(s)
import qualified Framework.Pipeline.Data    as Pipe
import qualified Framework.IR.Standard.Data as StdIR


--- Local
import qualified CGIR.Rust.Pass.Formalize.CS.Functions as CS.Functions


-- ~ RedRust Payload
import qualified CGIR.Rust.Data.Payload.Input as IR
-- ~ RedRust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Etc    as Etc
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Types  as T
-- ~~ BlockLevel
import qualified CGIR.Rust.AST.BlockLevel.Patterns as P
import qualified CGIR.Rust.AST.BlockLevel.Stmts    as S
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.CallSites  as CS
import qualified CGIR.Rust.AST.TopLevel.TypeSystem as TS
-- ~ RedRust AST Utils
import qualified CGIR.Rust.AST.BlockLevel.Stmt.Utils as SU
-- ~ Misc. Builder Utils
import qualified CGIR.Rust.Prefabs.Builder.Etc.Utils as Util

-- ~ Render Utils
import qualified CGIR.Rust.Render.Utils as Util
-- *





input :: IR.Payload -> IR.Payload
input =
    CS.Functions.input
