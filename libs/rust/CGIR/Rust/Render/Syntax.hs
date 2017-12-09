{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.Rust.Render.Syntax (
      renderFunctions
    , renderUnions
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import Prelude (show)

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Text.PrettyPrint.Leijen.Text as P

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
import qualified CGIR.Rust.Render.Syntax.TopLevel.Unions    as Decl
import qualified CGIR.Rust.Render.Syntax.TopLevel.Functions as Decl
-- *



renderFunctions :: [Decl.Function] -> Text
renderFunctions xs =
    map (packDoc Decl.renderFunction) xs
        |> Text.unlines


renderUnions :: [Decl.Union] -> Text
renderUnions xs =
    map (packDoc Decl.renderUnion) xs
        |> Text.unlines



packDoc :: (a -> Doc) -> a -> Text
packDoc f doc =
    P.displayTStrict toSimpleDoc
    where
        toSimpleDoc = P.renderPretty 0.4 400 (f doc)






