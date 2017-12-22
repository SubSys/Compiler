{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Syntax.TopLevel.Fixities (
    renderInfix
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import qualified Data.Maybe as Maybe
import qualified Data.List as List

--- Framework(s)
import Framework.Render
import qualified Framework.Render.Utils as Util

--- Local
-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload as Payload

-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~ Renderers
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Ident         as ID
-- *



-- *
-- | ## Fixities
-- *



renderInfix :: Decl.Infix -> Doc
renderInfix (Decl.InfixL sym opPrecedence meta) =
        "infixl"
    <+> renderOpPrecedence opPrecedence
    <+> ID.renderSym sym

renderInfix (Decl.InfixR sym opPrecedence meta) =
        "infixr"
    <+> renderOpPrecedence opPrecedence
    <+> ID.renderSym sym

renderInfix (Decl.InfixN sym opPrecedence meta) =
        "infix"
    <+> renderOpPrecedence opPrecedence
    <+> ID.renderSym sym



renderOpPrecedence :: Decl.OpPrecedence -> Doc
renderOpPrecedence (Decl.OpPrecedence i meta) = render i





