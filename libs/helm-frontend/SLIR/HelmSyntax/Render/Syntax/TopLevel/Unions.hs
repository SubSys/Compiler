{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Syntax.TopLevel.Unions (
    renderUnion
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
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload as Payload

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
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Ident         as ID
import qualified SLIR.HelmSyntax.Render.Syntax.Base.Types         as T
-- *





-- *
-- | ## Unions
-- *
renderUnion :: Decl.Union -> Doc
renderUnion (Decl.Union name vars cons meta) =
    let name' = ID.renderBig name
        cons' = renderConstructors cons
        vars' = map ID.renderLow vars
            |> Util.punctuate Util.space
            |> Util.hcat
    in
        "type" <+> name' <+> vars' <$$> cons' <$$> Util.softline


renderConstructor :: Decl.Constructor -> Doc
renderConstructor (Decl.Constructor name args meta) =
    let name' = ID.renderBig name
        args' = map T.renderType args
          |> Util.punctuate Util.space
          |> Util.hcat
    in
        name' <+> args'




-- *
-- | Internal Helpers
-- *

renderConstructors :: [Decl.Constructor] -> Doc
renderConstructors [x] = Util.indent 4 $ "=" <+> renderConstructor x
renderConstructors (x:xs) =
    let rest = map constr xs
          |> Util.vcat
    in
        Util.indent 4 $ "=" <+> renderConstructor x <$$> rest

    where
        constr con =
            "|" <+> renderConstructor con







