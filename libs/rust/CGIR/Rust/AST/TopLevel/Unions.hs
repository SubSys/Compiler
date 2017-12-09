{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.TopLevel.Unions (
      IR.Union
    , IR.Constructor

    , pattern Union
    , pattern Constructor
) where


-- *
import Core

--- Local
import qualified CGIR.Rust.Internal.AST as IR
-- *



pattern Union :: IR.Big -> [IR.Low] -> [IR.Constructor] -> IR.Union
pattern Union name gs cons = IR.Union name gs cons



pattern Constructor :: IR.Big -> [IR.Type] -> IR.Constructor
pattern Constructor name args = IR.Constructor name args






