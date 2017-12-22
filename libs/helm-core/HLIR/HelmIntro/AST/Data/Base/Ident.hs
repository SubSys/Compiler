{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

module HLIR.HelmIntro.AST.Data.Base.Ident (
      IR.Low
    , IR.Big
    , IR.Ref
    , IR.Binder
    , IR.Namespace(..)
    
    , pattern Low
    , pattern Big
    
    , pattern Binder
    , pattern Ref
) where


-- *
import Core

--- Local
import qualified HLIR.HelmIntro.Internal.AST as IR
-- *



pattern Low :: Text -> Maybe IR.Namespace ->  IR.Low
pattern Low id' ns = IR.Low id' ns

pattern Big :: Text -> Maybe IR.Namespace ->  IR.Big
pattern Big id' ns = IR.Big id' ns



pattern Binder :: Text -> Maybe IR.Namespace ->  IR.Binder
pattern Binder id' ns = IR.Binder id' ns


pattern Ref :: Text -> Maybe IR.Namespace ->  IR.Ref
pattern Ref id' ns = IR.Ref id' ns

