{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.Rust.Render.Syntax.TopLevel.Unions (
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
import qualified CGIR.Rust.Render.Syntax.Base.Types as T
-- *




renderUnion :: Decl.Union -> Doc
renderUnion (Decl.Union name vars cons) =
    let name' = ID.renderBig name
        cons' = renderConstructors cons
        vars' = renderGenerics vars
    in
        "enum" <+> name' <+> vars' <+> cons' <$$> Util.softline
    
    where
        renderGenerics [] = Util.empty
        renderGenerics gs =
            map ID.renderLow gs
                |> Util.punctuate ","
                |> Util.punctuate Util.space
                |> Util.hcat
                |> Util.angles


renderConstructor :: Decl.Constructor -> Doc
renderConstructor (Decl.Constructor name []) = ID.renderBig name

renderConstructor (Decl.Constructor name args) =
    let name' = ID.renderBig name
        args' = map T.renderType args
          |> Util.punctuate ","
          |> Util.punctuate Util.space
          |> Util.hcat
          |> Util.parens
    in
        name' <+> args'



renderConstructors :: [Decl.Constructor] -> Doc
renderConstructors xs =
    let xs' = map renderConstructor xs
            |> Util.punctuate ","
            |> Util.vcat
    in
        "{" <$$> Util.indent 4 xs' <$$> "}"





