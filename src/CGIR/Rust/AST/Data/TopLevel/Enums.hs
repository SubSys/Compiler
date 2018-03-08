{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.Data.TopLevel.Enums (
    IR.Enum
  , pattern Enum
) where


-- *
import Core

--- Local
import qualified CGIR.Rust.Internal.AST as IR
-- *


pattern Enum :: IR.Ident -> [IR.Generic] -> [IR.Variant] -> IR.Enum
pattern Enum name generics variants = IR.Enum name generics variants



