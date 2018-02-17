{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module GCIR.RustCG.AST.Data.Semantic.DeclLevel.Enums (
    IR.Enum
  , pattern Enum
) where


-- *
import Core

--- Local
import qualified GCIR.RustCG.Internal.AST as IR
-- *


pattern Enum :: IR.Ident -> [IR.Generic] -> [IR.Variant] -> IR.Enum
pattern Enum name generics variants = IR.Enum name generics variants



