{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.Rust.AST.BlockLevel.Stmt.Utils (
      pattern Panic
    , pattern Vec
) where


-- *
import Core
import Data.Data (Data, Typeable)

--- Local
import qualified CGIR.Rust.Internal.AST    as IR
-- *




-- *
-- | Error Utils
-- *
pattern Panic :: IR.Stmt
pattern Panic = IR.MacroCall (IR.Low "panic") [IR.ScalarStmt (IR.StringValue "Error")]




-- *
-- | Sequences
-- *
pattern Vec :: [IR.Stmt] -> IR.Stmt
pattern Vec xs =
    IR.MacroCall (IR.Low "vec") xs






