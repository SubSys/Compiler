{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.Rust.Render.Syntax.Base.Types (
    renderType
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
import qualified CGIR.Rust.Render.Syntax.Base.Ident as ID
-- *






renderType :: T.Type -> Doc
renderType T.String = "&'static str"
renderType T.Char   = "char"
renderType T.Int    = "u32"
renderType T.Float  = "f32"
renderType T.Bool   = "bool"

renderType (T.Record fields) =
    let field (name, ty) = ID.renderLow name <+> ":" <+> renderType ty
    in map field fields
        |> Util.punctuate ","
        |> Util.vcat
        |> Util.braces


renderType (T.Tuple ts) =
    map renderType ts
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens

renderType (T.List ty) =
    "List" <+> renderType ty

renderType (T.Union name args) =
    let args' = map renderType args
         |> Util.punctuate ","
         |> Util.punctuate Util.space
         |> Util.hcat
         |> Util.angles
    in
        ID.renderBig name <+> args'

renderType (T.Generic (Etc.Generic name)) =
    ID.renderLow name

renderType (T.Fn inTs (Etc.Output outTy)) =
    let inTs' = map renderType inTs
            |> Util.punctuate ","
            |> Util.punctuate Util.space
            |> Util.hcat
            |> Util.parens
        
        outTy' = renderType outTy
    in
        "&Fn" <> inTs' <+> "->" <+> outTy'









