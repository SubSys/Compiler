{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.GLSL.AST.Data.Base.Ident (
    IR.Ident
  , IR.Namespace
  
  , pattern Ident
  , pattern Namespace
  
  , pattern Ident_
  , pattern Ident'
) where


-- ~
import Core
import Prelude (String)
import qualified Data.Text as Text
import qualified CGIR.GLSL.Internal.AST as IR
-- ~



pattern Ident :: Text -> Maybe IR.Namespace -> IR.Ident
pattern Ident name ns = IR.Ident name ns

pattern Namespace :: [Text] -> IR.Namespace
pattern Namespace segs = IR.Namespace segs


-- | Alternate variations - convenience helpers
--

pattern Ident' :: String -> IR.Ident
pattern Ident' name <- IR.Ident (Text.unpack -> name) Nothing
    where
        Ident' name = IR.Ident (Text.pack name) Nothing

pattern Ident_ :: Text -> IR.Ident
pattern Ident_ name = IR.Ident name Nothing

















