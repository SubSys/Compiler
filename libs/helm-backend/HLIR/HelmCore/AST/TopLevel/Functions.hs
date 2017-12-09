{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmCore.AST.TopLevel.Functions (
      IR.Function
    , pattern Function
) where


-- *
import Core

--- Local
import qualified HLIR.HelmCore.Internal.AST as IR
-- *


pattern Function :: IR.Binder -> IR.Expr -> IR.Scheme -> IR.Function
pattern Function name expr scheme = IR.Function name expr scheme


