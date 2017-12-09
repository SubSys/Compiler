{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module LLIR.LightRoast.Render.Syntax.TopLevel.Unions (
    renderUnion
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
import qualified LLIR.LightRoast.Render.Syntax.Base.Types as T
-- *




renderUnion :: Decl.Union -> Doc
renderUnion (Decl.Union name vars cons) =
    let name' = ID.renderBig name
        cons' = renderConstructors cons
        vars' = map ID.renderLow vars
            |> Util.punctuate Util.space
            |> Util.hcat
    in
        "type" <+> name' <+> vars' <$$> cons' <$$> Util.softline


renderConstructor :: Decl.Constructor -> Doc
renderConstructor (Decl.Constructor name args) =
    let name' = ID.renderBig name
        args' = map T.renderType args
          |> Util.punctuate Util.space
          |> Util.hcat
    in
        name' <+> args'



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






