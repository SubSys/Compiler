{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmOutro.AST.TopLevel.Functions (
      IR.Function
    , pattern Function
) where


-- *
import Core

--- Local
import qualified HLIR.HelmOutro.Internal.AST as IR
-- *


pattern Function :: IR.Binder -> [IR.Arg] -> IR.Expr -> Maybe IR.Scheme -> IR.Function
pattern Function name args expr scheme = IR.Function name args expr scheme


