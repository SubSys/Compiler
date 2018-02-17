{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions (
    IR.Function
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
               -> Maybe IR.Scheme
               -> IR.Function

pattern Function name args expr scheme = IR.Function name args expr scheme





