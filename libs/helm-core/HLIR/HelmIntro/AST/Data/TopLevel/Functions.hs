{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmIntro.AST.Data.TopLevel.Functions (
      IR.Function
    , pattern Function
) where


-- *
import Core
import Prelude (show)


--- Local
import qualified HLIR.HelmIntro.Internal.AST as IR
-- *










pattern Function :: IR.Binder
               -> [IR.Binder]
               -> IR.Expr
               -> Maybe IR.Scheme
               -> IR.Function

pattern Function name args expr optSig = IR.Function name args expr optSig
