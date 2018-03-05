{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.SPMD.AST.Data.Base.Ident (
    IR.Ident
  , IR.Namespace
  
  , pattern Ident
  , pattern Namespace
  
  , pattern Ident_
) where


-- ~
import Core
import qualified LLIR.SPMD.Internal.AST as IR
-- ~



pattern Ident :: Text -> Maybe IR.Namespace -> IR.Ident
pattern Ident name ns = IR.Ident name ns

pattern Namespace :: [Text] -> IR.Namespace
pattern Namespace segs = IR.Namespace segs


-- | Alternate variations - convenience helpers
--

pattern Ident_ :: Text -> IR.Ident
pattern Ident_ name = IR.Ident name Nothing

















