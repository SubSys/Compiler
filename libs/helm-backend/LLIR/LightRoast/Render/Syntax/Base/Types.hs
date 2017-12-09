{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module LLIR.LightRoast.Render.Syntax.Base.Types (
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
import qualified LLIR.LightRoast.Render.Syntax.Base.Ident as ID
-- *






renderType :: T.Type -> Doc
renderType T.String = "String"
renderType T.Char   = "Char"
renderType T.Int    = "Int"
renderType T.Float  = "Float"
renderType T.Bool   = "Bool"

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
         |> Util.punctuate Util.space
         |> Util.hcat
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
        "Fn" <> inTs' <+> "->" <+> outTy'









