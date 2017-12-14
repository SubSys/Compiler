{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Syntax.Base.Types (
      renderType
    , renderScheme
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
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Ident as ID
-- *




-- *
-- | # Types
-- *

renderType :: T.Type -> Doc

renderType (T.String meta) = "String"
renderType (T.Char   meta) = "Char"
renderType (T.Int    meta) = "Int"
renderType (T.Float  meta) = "Float"
renderType (T.Bool   meta) = "Bool"

renderType (T.Record fields meta) =
    let field (name, ty) = ID.renderLow name <+> ":" <+> renderType ty
    in map field fields
        |> Util.punctuate ","
        |> Util.vcat
        |> Util.braces


renderType (T.Tuple ts meta) =
    map renderType ts
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens

renderType (T.List ty meta) =
    "List" <+> renderType ty

renderType (T.Union name args meta) =
    let args' = map renderType args
         |> Util.punctuate Util.space
         |> Util.hcat
    in
        ID.renderBig name <+> args'

renderType (T.Var name meta) =
    ID.renderLow name

renderType (T.Arr t1 t2 meta) =
    let t1' = renderType t1
        t2' = renderType t2
    in
        Util.parens (t1' <+> "->" <+> t2')

renderType (T.Parens ty meta) =
    let ty' = renderType ty
    in
        Util.parens ty'


renderType (T.Superposed con ts) =
    let
        con' = renderType con
        ts' = map renderType ts
            |> Util.punctuate "/"
            |> Util.hcat
    in
        Util.angles
            $ con' <+> ":" <+> ts'



-- *
-- | # Type Schemes
-- *
renderScheme :: T.Scheme -> Doc
renderScheme (T.Forall [] ty) = renderToplevelType ty
renderScheme (T.Forall vars ty) =
    let ty'   = renderToplevelType ty
        vars' = map ID.renderLow vars
          |> Util.punctuate Util.space
          |> Util.hcat
    in
        "forall" <+> vars' <> "." <+> ty'


renderToplevelType :: T.Type -> Doc
renderToplevelType (T.Arr t1 t2 meta) =
    renderType t1 <+> "->" <+> renderToplevelType t2

renderToplevelType x = renderType x



