{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module LLIR.LightRoast.Render.Syntax.TermLevel.Stmt (
      renderStmt
    , renderBlock
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import Prelude (show)

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text

--- Framework(s)
import Framework.Render
import qualified Framework.Render.Utils as Util

--- Local Deps
-- ~ LightRoast AST
-- ~~ Base
import qualified LLIR.LightRoast.AST.Base.Ident  as ID
import qualified LLIR.LightRoast.AST.Base.Types  as T
import qualified LLIR.LightRoast.AST.Base.Values as V
import qualified LLIR.LightRoast.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified LLIR.LightRoast.AST.TermLevel.Stmt        as S
import qualified LLIR.LightRoast.AST.TermLevel.Patterns    as P
import qualified LLIR.LightRoast.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified LLIR.LightRoast.AST.TopLevel.Functions as Decl
import qualified LLIR.LightRoast.AST.TopLevel.Unions    as Decl

--- Local
import qualified LLIR.LightRoast.Render.Syntax.Base.Ident         as ID
import qualified LLIR.LightRoast.Render.Syntax.TermLevel.Patterns as P
import qualified LLIR.LightRoast.Render.Syntax.Base.Values        as V
-- *







renderStmt (S.Ref name) = ID.renderLow name
renderStmt (S.Lit val) = V.renderValue val


renderStmt (S.Tuple items) =
    map renderStmt items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens


renderStmt (S.List xs) =
    map renderStmt xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets


renderStmt (S.Case expr caseAlts) =
    let expr'     = renderStmt expr
        caseAlts' = map (P.renderCaseAlt renderStmt) caseAlts
            |> Util.punctuate Util.linebreak
            |> Util.hcat
            |> Util.indent 4
    in
        "case" <+> expr' <+> "of" <$$> caseAlts'
        



renderStmt (S.FunCall ref args) =
    let ref'  = ID.renderLow ref
        args' = map renderStmt args
            |> Util.punctuate ","
            |> Util.punctuate Util.softline
            |> Util.hcat
            |> Util.parens
    in
        ref' <> args'

renderStmt (S.ConCall con args) =
    let con'  = ID.renderBig con
        args' = map renderStmt args
            |> Util.punctuate ","
            |> Util.punctuate Util.softline
            |> Util.hcat
            |> Util.parens
    in
        con' <> args'





renderBlock :: Decl.Block -> Doc
renderBlock (Decl.Block stmts) =
    let stmts' = map renderStmt stmts
            |> Util.punctuate ";"
            |> Util.vcat
    in
        "{" <$$> Util.indent 4 stmts' <$$> "}"





