{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident (
    IR.Ident
  , IR.Namespace(..)
  , pattern Ident
  , pattern Ident'
  , pattern Ident_
  , pattern OnSym
) where


-- *
import Core

import qualified Prelude as Pre

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Char as Char

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *


pattern Ident :: Text -> Maybe IR.Namespace -> IR.Meta -> IR.Ident
pattern Ident name ns meta = IR.Ident name ns meta



-- | Alternative variations & misc. helpers.
--


pattern Ident' :: Text -> Maybe IR.Namespace -> IR.Ident
pattern Ident' name ns <- IR.Ident name ns _
    where
        Ident' name ns = IR.Ident name ns IR.Empty

pattern Ident_ :: Text -> IR.Ident
pattern Ident_ name <- IR.Ident name _ _
    where
        Ident_ name = IR.Ident name Nothing IR.Empty




pattern OnSym :: Text -> Maybe IR.Namespace -> IR.Meta -> IR.Ident
pattern OnSym name ns meta <- (opCharPred -> Just (Ident name ns meta))



opCharPred :: IR.Ident -> Maybe IR.Ident
opCharPred ident@(Ident x _ _) =
    if pred x then
        Just ident
    else
        Nothing
    
    where
        pred = Text.all (`List.elem` range)
        
        range :: Pre.String
        range = "+-/*=.<>:&|^?%~!"












