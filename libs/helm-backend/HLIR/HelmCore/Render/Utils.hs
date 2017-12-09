{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Render.Utils (
      renderFunctions
    , renderUnions
    , Display.packDoc
) where



-- *
import Core
import Core.Control.Flow

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text



--- Framework(s)
-- ~ Renderer
import Framework.Render
import qualified Framework.Render.Utils as Util
import qualified Framework.Render.Display as Display

-- ~ Pipeline & Related
import qualified Framework.Pipeline.Data    as Pipe
import qualified Framework.IR.Standard.Data as StdIR


--- Local
import HLIR.HelmCore.Render.Instances ()

--- Local
-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as ID
import qualified HLIR.HelmCore.AST.Base.Types  as T
import qualified HLIR.HelmCore.AST.Base.Values as V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as Decl
-- *




renderFunctions :: [Decl.Function] -> Text
renderFunctions xs =
    map Display.packDoc xs
        |> Text.unlines


renderUnions :: [Decl.Union] -> Text
renderUnions xs =
    map Display.packDoc xs
        |> Text.unlines


