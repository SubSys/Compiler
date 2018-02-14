{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module GCIR.RustCG.AST.Data.Semantic.DeclLevel.Functions.Header (
    IR.Input
  , IR.Output
  , IR.Generic
  
  , pattern Input
  , pattern Output
  , pattern Generic
) where


-- *
import Core

--- Local
import qualified GCIR.RustCG.Internal.AST as IR
-- *



pattern Input :: IR.Ident -> IR.Type -> IR.Input
pattern Input ident ty = IR.Input ident ty

pattern Output :: IR.Type -> IR.Output
pattern Output ty = IR.Output ty

pattern Generic :: IR.Ident -> IR.Generic
pattern Generic ident = IR.Generic ident


