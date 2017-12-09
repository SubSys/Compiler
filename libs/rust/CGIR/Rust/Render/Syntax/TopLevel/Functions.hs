{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.Rust.Render.Syntax.TopLevel.Functions (
    renderFunction
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
import qualified CGIR.Rust.Render.Syntax.Base.Ident         as ID
import qualified CGIR.Rust.Render.Syntax.Base.Values        as V
import qualified CGIR.Rust.Render.Syntax.TermLevel.Stmt     as S
import qualified CGIR.Rust.Render.Syntax.Base.Types         as T
-- *


renderFunction :: Decl.Function -> Doc

renderFunction (Decl.Function name gs args out block) =
    let name'  = ID.renderLow name
        gs'    = renderGenerics gs
        args'  = map renderInput args
            |> Util.punctuate ","
            |> Util.punctuate Util.space
            |> Util.hcat
            |> Util.parens
        out'   = renderOutput out
        block' = S.renderBlock block
    in
            "fn"
        <+> name'
        <+> gs'
        <+> args'
        <+> out'

        <+> block'
        <> Util.linebreak




renderGenerics :: [Etc.Generic] -> Doc
renderGenerics [] = Util.empty
renderGenerics gs =
    map renderGeneric gs
        |> Util.punctuate ","
        |> Util.punctuate Util.space
        |> Util.hcat
        |> Util.angles




renderInput :: Etc.Input -> Doc
renderInput (Etc.Input name ty) =
    ID.renderLow name <> ":" <+> T.renderType ty

renderGeneric :: Etc.Generic -> Doc
renderGeneric (Etc.Generic name) =
    ID.renderLow name


renderOutput :: Etc.Output -> Doc
renderOutput (Etc.Output ty) =
    let ty' = T.renderType ty
    in
        "->" <+> ty'



