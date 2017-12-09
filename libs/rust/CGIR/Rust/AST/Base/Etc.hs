{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.Base.Etc (
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
import qualified CGIR.Rust.Internal.AST as IR
-- *



pattern Input :: IR.Low -> IR.Type -> IR.Input
pattern Input name ty = IR.Input name ty

pattern Output :: IR.Type -> IR.Output
pattern Output ty = IR.Output ty

pattern Generic :: IR.Low -> IR.Generic
pattern Generic name = IR.Generic name




