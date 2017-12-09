{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Render.Utils (
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
import HLIR.HelmOutro.Render.Instances ()

--- Local
-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl
-- *




renderFunctions :: [Decl.Function] -> Text
renderFunctions xs =
    map Display.packDoc xs
        |> Text.unlines


renderUnions :: [Decl.Union] -> Text
renderUnions xs =
    map Display.packDoc xs
        |> Text.unlines


