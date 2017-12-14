{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Syntax.TermLevel.Expr (
    renderExpr
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
import qualified SLIR.HelmSyntax.Data.Payload as Payload

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
-- *


-- *
-- | ## Expressions
-- *



renderExpr :: (Decl.Function -> Doc) -> E.Expr -> Doc
renderExpr f (E.Var id' meta) = ID.renderLow id'
renderExpr f (E.Lit val meta) = V.renderValue val


renderExpr f (E.Tuple items meta) =
    map (renderExpr f) items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens


renderExpr f (E.List xs meta) =
    map (renderExpr f) xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets


renderExpr f (E.Con name meta) =
    ID.renderBig name


renderExpr f (E.BinOp sym e1 e2 _) =
    let sym' = ID.renderSym sym
        e1'  = renderExpr f e1
        e2'  = renderExpr f e2
    in 
        e1' <+> sym' <+> e2'



renderExpr f (E.If intros outro meta) =
    let ifBranches = map intro intros
            |> Util.vcat

        elseBranch = "else" <$$> Util.indent 4 (renderExpr f outro)
    in 
        ifBranches <$$> elseBranch
    where
        intro (con, expr) =
            let con' = renderExpr f con
                expr' = renderExpr f expr
            in
                "if" <+> con' <+> "then" <$$> Util.indent 4 expr'


renderExpr f (E.Let fns expr meta) =
    let fns' = map f fns
          |> Util.punctuate Util.linebreak
          |> Util.vcat
        expr' = renderExpr f expr
    in
        "let" <> Util.indent 1 fns' <$$> "in" <$$> Util.indent 4 expr'


renderExpr f (E.Case expr caseAlts meta) =
    let expr'     = renderExpr f expr
        caseAlts' = map renderAlt caseAlts
            |> Util.punctuate Util.linebreak
            |> Util.hcat
            |> Util.indent 4
    in
        "case" <+> expr' <+> "of" <$$> caseAlts'
    
    where
        renderAlt = P.renderCaseAlt (renderExpr f)


-- renderExpr f (E.RecordUpdate low fields meta) =
-- renderExpr f (E.RecordAccess low optExpr meta) =


renderExpr f (E.Parens expr meta) =
    Util.parens $ renderExpr f expr

renderExpr f (E.App e1 e2 meta) =
    Util.parens
        $ renderExpr f e1 <+> renderExpr f e2

renderExpr f (E.Abs var expr _) =
    let var' = ID.renderLow var
        expr' = renderExpr f expr
    in
        Util.parens
            $ "λ" <> var' <> "." <+> expr'


renderExpr f (E.Record fields meta) =
    map field fields
        |> Util.punctuate ","
        |> Util.punctuate Util.softline
        |> Util.vcat
        |> Util.braces
    where
        field :: (ID.Low, E.Expr) -> Doc
        field (name, val) = ID.renderLow name <+> ":" <+> renderExpr f val



-- *
-- | Internal Helpers
-- *






