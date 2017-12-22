{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmIntro.Render.Syntax.TopLevel.Functions (
    renderFunction
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
import qualified HLIR.HelmIntro.Render.Syntax.TermLevel.Expr     as E
-- *





-- *
-- | ## Functions
-- *


renderFunction (Decl.Function id' args expr sig) =
    renderFunction' (ID.renderBinder id') args expr sig


renderFunction' :: Doc -> [ID.Binder] -> E.Expr -> Maybe T.Scheme -> Doc
renderFunction' name args expr (Just scheme) =
    let args' = map ID.renderBinder args
          |> Util.punctuate Util.space
          |> Util.hcat
        expr' = E.renderExpr renderFunction expr
        scheme' = T.renderScheme scheme

        typeDecl = name <+> ":" <+> scheme'
        exprDecl = name <+> args' <+> "=" <$$> Util.indent 4 expr'
    in
        typeDecl <$$> exprDecl <$$> Util.softline


renderFunction' name args expr Nothing =
    let args' = map ID.renderBinder args
          |> Util.punctuate Util.space
          |> Util.hcat
        expr' = E.renderExpr renderFunction expr
    in
        name <+> args' <+> "=" <$$> Util.indent 4 expr' <$$> Util.softline
