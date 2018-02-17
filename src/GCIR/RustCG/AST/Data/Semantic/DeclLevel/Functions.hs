{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module GCIR.RustCG.AST.Data.Semantic.DeclLevel.Functions (
    IR.Function
  , pattern Function
) where


-- *
import Core

--- Local
import qualified GCIR.RustCG.Internal.AST as IR
-- *



pattern Function :: IR.Ident -> [IR.Generic] -> [IR.Input] -> IR.Output -> IR.Block -> IR.Function
pattern Function name generics inputs outType body =
    IR.Function name generics inputs outType body


