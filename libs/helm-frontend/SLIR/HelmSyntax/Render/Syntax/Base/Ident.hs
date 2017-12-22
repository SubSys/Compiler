{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Syntax.Base.Ident (
      renderLow
    , renderBig
    , renderSym
    , renderNamespace
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
-- *



renderLow :: ID.Low -> Doc
renderLow (ID.Low name Nothing   meta) = render name
renderLow (ID.Low name (Just ns) meta) =
    renderNamespace ns <> "." <> render name



renderBig :: ID.Big -> Doc
renderBig (ID.Big name Nothing   meta) = render name
renderBig (ID.Big name (Just ns) meta) =
    renderNamespace ns <> "." <> render name


renderSym :: ID.Sym -> Doc
renderSym (ID.Sym name Nothing   meta) = render name
renderSym (ID.Sym name (Just ns) meta) =
        renderNamespace ns <> "." <> Util.parens (render name)


renderNamespace :: ID.Namespace -> Doc
renderNamespace (ID.Namespace segs) =
    map render segs
        |> Util.punctuate "."
        |> Util.hcat







