{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module LLIR.LightRoast.Render.Syntax.Base.Ident (
      renderLow
    , renderBig
    , renderNamespace
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
-- *



renderLow :: ID.Low -> Doc
renderLow (ID.Low name Nothing)   = render name
renderLow (ID.Low name (Just ns)) =
    renderNamespace ns <> "." <> render name

renderBig :: ID.Big -> Doc
renderBig (ID.Big name Nothing)   = render name
renderBig (ID.Big name (Just ns)) =
    renderNamespace ns <> "." <> render name


renderNamespace :: ID.Namespace -> Doc
renderNamespace (ID.Namespace segs) =
    map render segs
        |> Util.punctuate "."
        |> Util.hcat





