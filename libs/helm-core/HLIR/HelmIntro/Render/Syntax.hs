{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Render.Syntax (
    renderProgram
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Maybe as Maybe
import qualified Data.List as List

--- Framework(s)
import Framework.Render
import qualified Framework.Render.Utils as Util

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

-- ~ Sub Renderers
import qualified HLIR.HelmIntro.Render.Syntax.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.Render.Syntax.TopLevel.Unions    as Decl
-- *



renderProgram :: Payload.Program -> Doc
renderProgram payload =
    let
        fns = Payload.getFunctions payload
            |> map Decl.renderFunction
            |> Util.punctuate Util.linebreak
            |> Util.vcat
        
        uns = Payload.getUnions payload
            |> map Decl.renderUnion
            |> Util.punctuate Util.linebreak
            |> Util.vcat
    in
        uns <$$> fns







