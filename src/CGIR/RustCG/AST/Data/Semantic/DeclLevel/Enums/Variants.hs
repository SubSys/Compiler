{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.RustCG.AST.Data.Semantic.DeclLevel.Enums.Variants (
    IR.Variant
  , pattern TupleVariant
  , pattern UnitVariant
) where

-- *
import Core

--- Local
import qualified CGIR.RustCG.Internal.AST as IR
-- *


pattern TupleVariant :: IR.Ident -> [IR.Type] -> IR.Variant
pattern TupleVariant name args = IR.TupleVariant name args


pattern UnitVariant :: IR.Ident -> IR.Variant
pattern UnitVariant name = IR.UnitVariant name

