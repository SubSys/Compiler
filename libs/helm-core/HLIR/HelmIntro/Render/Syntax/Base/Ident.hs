{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmIntro.Render.Syntax.Base.Ident (
      renderLow
    , renderBig
    , renderNamespace
    , renderRef
    , renderBinder
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
-- *



renderLow :: ID.Low -> Doc
renderLow (ID.Low name Nothing  ) = render name
renderLow (ID.Low name (Just ns)) =
    renderNamespace ns <> "." <> render name


renderBig :: ID.Big -> Doc
renderBig (ID.Big name Nothing  ) = render name
renderBig (ID.Big name (Just ns)) =
    renderNamespace ns <> "." <> render name


renderNamespace :: ID.Namespace -> Doc
renderNamespace (ID.Namespace segs) =
    map render segs
        |> Util.punctuate "."
        |> Util.hcat



renderRef :: ID.Ref -> Doc
renderRef (ID.Ref name Nothing  ) = render name
renderRef (ID.Ref name (Just ns)) =
    renderNamespace ns <> "." <> render name


renderBinder :: ID.Binder -> Doc
renderBinder (ID.Binder name Nothing  ) = render name
renderBinder (ID.Binder name (Just ns)) =
    renderNamespace ns <> "." <> render name


