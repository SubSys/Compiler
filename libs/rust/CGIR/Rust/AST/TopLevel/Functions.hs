{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.TopLevel.Functions (
      IR.Function
    , pattern Function
) where


-- *
import Core

--- Local
import qualified CGIR.Rust.Internal.AST as IR
-- *




pattern Function :: IR.Low -> [IR.Generic] -> [IR.Input] -> IR.Output -> IR.Block -> IR.Function
pattern Function name gs args outTy block = IR.Function name gs args outTy block



