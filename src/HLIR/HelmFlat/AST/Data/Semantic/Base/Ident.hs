{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmFlat.AST.Data.Semantic.Base.Ident (
    IR.Ident
  , IR.Namespace(..)
  , pattern Ident
  
  , pattern Ident'
  , pattern Ident''
  , pattern Ident_
) where


-- *
import Core

import qualified Prelude as Pre

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Char as Char

--- Local
import qualified HLIR.HelmFlat.Internal.AST as IR
-- *


pattern Ident :: Text -> Maybe IR.Namespace -> IR.Ident
pattern Ident name ns = IR.Ident name ns



-- | Alternative variations & misc. helpers.
--


pattern Ident' :: Pre.String -> IR.Ident
pattern Ident' name <- IR.Ident (Text.unpack -> name) _
    where
        Ident' name = IR.Ident (Text.pack name) Nothing

pattern Ident'' :: Pre.String -> Maybe IR.Namespace -> IR.Ident
pattern Ident'' name ns <- IR.Ident (Text.unpack -> name) ns
    where
        Ident'' name ns = IR.Ident (Text.pack name) ns

pattern Ident_ :: Text -> IR.Ident
pattern Ident_ name <- IR.Ident name _
    where
        Ident_ name = IR.Ident name Nothing









