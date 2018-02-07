{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc (
    IR.Binder
  , pattern Binder
  , pattern Binder_
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *


pattern Binder :: IR.Ident -> Maybe IR.Type -> IR.Binder
pattern Binder ident optType = IR.Binder ident optType


pattern Binder_ :: IR.Ident -> IR.Binder
pattern Binder_ ident = IR.Binder ident Nothing


