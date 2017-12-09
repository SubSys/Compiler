{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.LightRoast.AST.TopLevel.Functions (
      IR.Function
    , pattern Function
) where


-- *
import Core

--- Local
import qualified LLIR.LightRoast.Internal.AST as IR
-- *




pattern Function :: IR.Low -> [IR.Generic] -> [IR.Input] -> IR.Output -> IR.Block -> IR.Function
pattern Function name gs args outTy block = IR.Function name gs args outTy block



