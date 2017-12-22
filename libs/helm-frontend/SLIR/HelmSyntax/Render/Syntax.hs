{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Render.Syntax (
    renderModule
  , renderProgram
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
-- ~ HelmSyntax Interfaces
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload   as Module
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Program
import qualified SLIR.HelmSyntax.Data.Interface.Utils              as IUtil

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

-- ~ Sub Renderers
import qualified SLIR.HelmSyntax.Render.Syntax.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.Render.Syntax.TopLevel.Unions    as Decl
-- *




renderModule :: Module.Module -> Doc
renderModule payload =
    let
        fns = Module.getFunctions payload
            |> map Decl.renderFunction
            |> Util.punctuate Util.linebreak
            |> Util.vcat
        
        uns = Module.getUnions payload
            |> map Decl.renderUnion
            |> Util.punctuate Util.linebreak
            |> Util.vcat
    in
        uns <$$> fns




renderProgram :: Program.Program -> Doc
renderProgram payload =
    let
        fns = Program.getFunctions payload
            |> map Decl.renderFunction
            |> Util.punctuate Util.linebreak
            |> Util.vcat
        
        uns = Program.getUnions payload
            |> map Decl.renderUnion
            |> Util.punctuate Util.linebreak
            |> Util.vcat
    in
        uns <$$> fns







