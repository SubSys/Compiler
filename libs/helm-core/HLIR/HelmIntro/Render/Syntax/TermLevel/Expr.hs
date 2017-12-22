{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmIntro.Render.Syntax.TermLevel.Expr (
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
-- ~ HelmIntro AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

-- ~ Renderers
import qualified HLIR.HelmIntro.Render.Syntax.Base.Ident         as ID
import qualified HLIR.HelmIntro.Render.Syntax.Base.Types         as T
import qualified HLIR.HelmIntro.Render.Syntax.Base.Values        as V
import qualified HLIR.HelmIntro.Render.Syntax.TermLevel.Patterns as P
-- *


-- *
-- | ## Expressions
-- *



renderExpr :: (Decl.Function -> Doc) -> E.Expr -> Doc
renderExpr f (E.Var ref) = ID.renderRef ref
renderExpr f (E.Lit val) = V.renderValue val


renderExpr f (E.Tuple items) =
    map (renderExpr f) items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens


renderExpr f (E.List xs) =
    map (renderExpr f) xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets


renderExpr f (E.Con name) =
    ID.renderBig name


renderExpr f (E.BinApp ref e1 e2) =
    let ref' = ID.renderRef ref
        e1'  = renderExpr f e1
        e2'  = renderExpr f e2
    in 
        e1' <+> ref' <+> e2'



renderExpr f (E.If intros outro) =
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


renderExpr f (E.Let fns expr) =
    let fns' = map f fns
          |> Util.punctuate Util.linebreak
          |> Util.vcat
        expr' = renderExpr f expr
    in
        "let" <> Util.indent 1 fns' <$$> "in" <$$> Util.indent 4 expr'


renderExpr f (E.Case expr caseAlts) =
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


renderExpr f (E.Parens expr) =
    Util.parens $ renderExpr f expr

renderExpr f (E.App e1 e2) =
    Util.parens
        $ renderExpr f e1 <+> renderExpr f e2

renderExpr f (E.Abs binder expr) =
    let binder' = ID.renderBinder binder
        expr' = renderExpr f expr
    in
        Util.parens
            $ "λ" <> binder' <> "." <+> expr'


-- renderExpr f (E.Record fields) =
--     map field fields
--         |> Util.punctuate ","
--         |> Util.punctuate Util.softline
--         |> Util.vcat
--         |> Util.braces
--     where
--         field :: (ID.Low, E.Expr) -> Doc
--         field (name, val) = ID.renderLow name <+> ":" <+> renderExpr f val
-- 


-- *
-- | Internal Helpers
-- *






