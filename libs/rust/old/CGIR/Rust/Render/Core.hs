{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Render.Core where


-- *
import Core
import Core.Control.Flow

import qualified Data.Maybe as Maybe
import qualified Data.List as List

--- Framework(s)
-- ~ Renderer
import Framework.Render'
import qualified Framework.Render'.Utils as Util

-- ~ Pipeline & Related
import qualified Framework.Pipeline.Data    as Pipe
import qualified Framework.IR.Standard.Data as StdIR


--- Local
import CGIR.Rust.Render.Instances ()

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
import qualified CGIR.Rust.AST.BlockLevel.Stmt.Aux as StmtAux
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.CallSites  as CS
import qualified CGIR.Rust.AST.TopLevel.TypeSystem as TS
-- *







