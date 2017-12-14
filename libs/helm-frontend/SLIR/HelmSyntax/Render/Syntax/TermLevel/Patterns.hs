{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Syntax.TermLevel.Patterns (
    renderCaseAlt
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
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Ident  as ID
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Types  as T
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Values as V
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Etc    as Etc
-- *



-- *
-- | ## Patterns
-- *

renderCaseAlt :: (E.Expr -> Doc) -> P.CaseAlt -> Doc
renderCaseAlt f (P.CaseAlt p e meta) =
    let p' = renderPattern p
        e' = f e
    in
        p' <+> "->" <$$> Util.indent 4 e'



renderPattern :: P.Pattern -> Doc
renderPattern (P.Var name meta) = ID.renderLow name
renderPattern (P.Lit val meta)  = V.renderValue val
renderPattern (P.Wildcard meta) = "_"

renderPattern (P.Record fields meta) =
    let fields' = map ID.renderLow fields
          |> Util.punctuate ","
          |> Util.hcat
    in
        Util.parens fields'

renderPattern (P.List xs meta) =
    map renderPattern xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets

renderPattern (P.Cons xs Nothing meta) =
    let xs' = map renderPattern xs
            |> Util.punctuate "::"
            |> Util.hcat
        rest = "::" <> "[]"
    in
        Util.parens $ xs' <> rest

renderPattern (P.Cons xs (Just rest) meta) =
    let xs' = map renderPattern xs
            |> Util.punctuate "::"
            |> Util.hcat
        rest' = "::" <> renderPattern rest
    in
        Util.parens $ xs' <> rest'


renderPattern (P.Tuple items meta) =
    map renderPattern items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens

renderPattern (P.Con name args meta) =
    let name' = ID.renderBig name
        args' = map renderPattern args
          |> Util.punctuate ","
          |> Util.punctuate Util.space
          |> Util.hcat
    in
        name' <+> args'




