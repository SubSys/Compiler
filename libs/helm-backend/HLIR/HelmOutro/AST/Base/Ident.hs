{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmOutro.AST.Base.Ident (
      IR.Low
    , IR.Big
    , IR.Namespace(..)
    
    , pattern Low
    , pattern Big
    
    , IR.Binder(..)
    , IR.Ref(..)
) where


-- *
import Core

--- Local
import qualified HLIR.HelmOutro.Internal.AST as IR
-- *



pattern Low :: Text -> Maybe IR.Namespace -> IR.Low
pattern Low id' ns = IR.Low id' ns

pattern Big :: Text -> Maybe IR.Namespace -> IR.Big
pattern Big id' ns = IR.Big id' ns




