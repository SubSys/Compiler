{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Prefabs.Builder.Generate.Impl.Init where


-- *
import Core
import Core.Control.Flow

import qualified Data.Text as Text


--- Local
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
-- *
