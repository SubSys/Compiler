{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmIntro.Render.Syntax.Base.Types (
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
import qualified HLIR.HelmIntro.Render.Syntax.Base.Ident as ID
-- *




-- *
-- | # Types
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

renderType (T.Var name) =
    ID.renderLow name

renderType (T.Arr t1 t2) =
    let t1' = renderType t1
        t2' = renderType t2
    in
        Util.parens (t1' <+> "->" <+> t2')

renderType (T.Parens ty) =
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
renderToplevelType (T.Arr t1 t2) =
    renderType t1 <+> "->" <+> renderToplevelType t2

renderToplevelType x = renderType x



