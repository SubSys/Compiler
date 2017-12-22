{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Render.Utils (
      renderFunctions
    , renderUnions
    , displayProgram
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
-- ~ HelmIntro Interfaces
import qualified HLIR.HelmIntro.Data.Payload as Payload

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

-- ~ Renderers
import qualified HLIR.HelmIntro.Render.Syntax.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.Render.Syntax.TopLevel.Unions    as Decl
import qualified HLIR.HelmIntro.Render.Syntax                    as Syntax
-- *




renderFunctions :: [Decl.Function] -> Text
renderFunctions xs =
    map (Display.packDoc' . Decl.renderFunction) xs
        |> Text.unlines


renderUnions :: [Decl.Union] -> Text
renderUnions xs =
    map (Display.packDoc' . Decl.renderUnion) xs
        |> Text.unlines


displayProgram :: Payload.Program -> Text
displayProgram x =
    Syntax.renderProgram x
        |> Display.packDoc'


