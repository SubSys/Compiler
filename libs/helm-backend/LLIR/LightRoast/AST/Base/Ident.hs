{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.LightRoast.AST.Base.Ident (
      IR.Low
    , IR.Big
    , IR.Namespace

    , pattern Low
    , pattern Big
    , pattern Namespace
) where


-- *
import Core

--- Local
import qualified LLIR.LightRoast.Internal.AST as IR
-- *





pattern Low :: Text -> Maybe IR.Namespace -> IR.Low
pattern Low name ns = IR.Low name ns


pattern Big :: Text -> Maybe IR.Namespace -> IR.Big
pattern Big name ns = IR.Big name ns


pattern Namespace :: [Text] -> IR.Namespace
pattern Namespace segs = IR.Namespace segs






