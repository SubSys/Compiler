{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Syntax.TopLevel.Functions (
    renderFunction
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
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Types         as T
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Values        as V
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Etc           as Etc
import qualified SLIR.HelmSyntax.Render.Syntax.TermLevel.Patterns as P
import qualified SLIR.HelmSyntax.Render.Syntax.TermLevel.Expr     as E
-- *





-- *
-- | ## Functions
-- *


renderFunction (Decl.FnDecl id' args expr sig meta) =
    renderFunction' (ID.renderLow id') args expr sig

renderFunction (Decl.OpDecl (ID.Sym txt Nothing _) args expr sig meta) =
    renderFunction' (Util.parens $ render txt) args expr sig

renderFunction (Decl.OpDecl (ID.Sym txt (Just ns) _) args expr sig meta) =
    let
        name = ID.renderNamespace ns <> "." <> Util.parens (render txt)
    in
        renderFunction' name args expr sig


renderFunction' :: Doc -> [ID.Low] -> E.Expr -> Maybe Etc.Signature -> Doc
renderFunction' name args expr (Just sig) =
    let args' = map ID.renderLow args
          |> Util.punctuate Util.space
          |> Util.hcat
        expr' = E.renderExpr renderFunction expr
        sig' = renderSig sig

        typeDecl = name <+> ":" <+> sig'
        exprDecl = name <+> args' <+> "=" <$$> Util.indent 4 expr'
    in
        typeDecl <$$> exprDecl <$$> Util.softline
    
    where
        renderSig sig = Etc.renderSig T.renderScheme (Just sig)

renderFunction' name args expr Nothing =
    let args' = map ID.renderLow args
          |> Util.punctuate Util.space
          |> Util.hcat
        expr' = E.renderExpr renderFunction expr
    in
        name <+> args' <+> "=" <$$> Util.indent 4 expr' <$$> Util.softline
