{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.Rust.Render.Syntax.TermLevel.Patterns where


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
-- ~ (CGIR) Rust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Types  as T
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified CGIR.Rust.AST.TermLevel.Stmt        as S
import qualified CGIR.Rust.AST.TermLevel.Patterns    as P
import qualified CGIR.Rust.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.Functions as Decl
import qualified CGIR.Rust.AST.TopLevel.Unions    as Decl

--- Local
import qualified CGIR.Rust.Render.Syntax.Base.Ident  as ID
import qualified CGIR.Rust.Render.Syntax.Base.Values as V
-- *




renderCaseAlt :: (S.Stmt -> Doc) -> P.CaseAlt -> Doc
renderCaseAlt renderStmt (P.CaseAlt p (Decl.Block b)) =
    let p' = renderPattern p
        b' = renderBlock b
    in
        p' <+> b'
    
    where
        renderBlock :: [S.Stmt] -> Doc
        renderBlock [s]   = "=>" <$$> Util.indent 4 (renderStmt s)
        renderBlock stmts =
            let stmts' = map renderStmt stmts
                    |> Util.punctuate ";"
                    |> Util.vcat
            in
                "=>" <+> "{" <$$> Util.indent 4 stmts' <$$> "}"





renderPattern :: P.Pattern -> Doc

renderPattern (P.Var name) = ID.renderLow name
renderPattern (P.Lit val)  = V.renderValue val
renderPattern P.Wildcard   = "_"

renderPattern (P.Record fields) =
    let fields' = map ID.renderLow fields
          |> Util.punctuate ","
          |> Util.hcat
    in
        Util.parens fields'


renderPattern (P.List xs) =
    map renderPattern xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets

renderPattern (P.Cons xs Nothing) =
    let xs' = map renderPattern xs
            |> Util.punctuate "::"
            |> Util.hcat
        rest = "::" <> "[]"
    in
        Util.parens $ xs' <> rest

renderPattern (P.Cons xs (Just rest)) =
    let xs' = map renderPattern xs
            |> Util.punctuate "::"
            |> Util.hcat
        rest' = "::" <> renderPattern rest
    in
        Util.parens $ xs' <> rest'


renderPattern (P.Tuple items) =
    map renderPattern items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens




renderPattern (P.Con name []) =
    let name' = ID.renderBig name
    in
        name'

renderPattern (P.Con name args) =
    let name' = ID.renderBig name
        args' = map renderPattern args
          |> Util.punctuate ","
          |> Util.punctuate Util.space
          |> Util.hcat
          |> Util.parens
    in
        name' <+> args'








