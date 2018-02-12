{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions (
    IR.Function
  , IR.Signature(..)
  , pattern Function
) where


-- *
import Core

--- Local
import qualified HLIR.HelmFlat.Internal.AST as IR
-- *



pattern Function :: IR.Binder
               -> [IR.Binder]
               -> IR.Expr
               -> IR.Signature
               -> IR.Meta
               -> IR.Function

pattern Function name args expr sig meta = IR.Function name args expr sig meta





