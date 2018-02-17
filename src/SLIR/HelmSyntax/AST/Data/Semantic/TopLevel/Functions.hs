{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions (
    IR.Function
  , IR.Signature(..)
  , pattern Function
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *



pattern Function :: IR.Binder
               -> [IR.Binder]
               -> IR.Expr
               -> IR.Signature
               -> IR.Meta
               -> IR.Function

pattern Function name args expr sig meta = IR.Function name args expr sig meta





