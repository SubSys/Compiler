{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.LightRoast.AST.TermLevel.Block (
      IR.Block
    , pattern Block
) where


-- *
import Core

--- Local
import qualified LLIR.LightRoast.Internal.AST as IR
-- *


pattern Block :: [IR.Stmt] -> IR.Block
pattern Block stmts = IR.Block stmts





