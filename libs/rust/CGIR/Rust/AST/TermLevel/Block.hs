{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.TermLevel.Block (
      IR.Block
    , pattern Block
) where


-- *
import Core

--- Local
import qualified CGIR.Rust.Internal.AST as IR
-- *


pattern Block :: [IR.Stmt] -> IR.Block
pattern Block stmts = IR.Block stmts





