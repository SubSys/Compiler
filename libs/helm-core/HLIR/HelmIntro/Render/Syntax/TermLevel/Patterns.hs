{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmIntro.Render.Syntax.TermLevel.Patterns (
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
import qualified HLIR.HelmIntro.Render.Syntax.Base.Ident  as ID
import qualified HLIR.HelmIntro.Render.Syntax.Base.Types  as T
import qualified HLIR.HelmIntro.Render.Syntax.Base.Values as V
-- *



-- *
-- | ## Patterns
-- *

renderCaseAlt :: (E.Expr -> Doc) -> P.CaseAlt -> Doc
renderCaseAlt f (P.CaseAlt p e) =
    let p' = renderPattern p
        e' = f e
    in
        p' <+> "->" <$$> Util.indent 4 e'



renderPattern :: P.Pattern -> Doc
renderPattern (P.Var name) = ID.renderBinder name
renderPattern (P.Lit val)  = V.renderValue val
renderPattern P.Wildcard = "_"

-- renderPattern (P.Record fields) =
--     let fields' = map ID.renderLow fields
--           |> Util.punctuate ","
--           |> Util.hcat
--     in
--         Util.parens fields'

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

renderPattern (P.Con name args) =
    let name' = ID.renderBig name
        args' = map renderPattern args
          |> Util.punctuate ","
          |> Util.punctuate Util.space
          |> Util.hcat
    in
        name' <+> args'




