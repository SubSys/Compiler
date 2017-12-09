{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Render.Utils (
      renderFunctions
    , renderUnions
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
import SLIR.HelmSyntax.Render.Instances ()

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Payload as Payload

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

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Base      as Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Exporting as Export
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Importing as Import
-- *




renderFunctions :: [Decl.Function] -> Text
renderFunctions xs =
    map Display.packDoc xs
        |> Text.unlines


renderUnions :: [Decl.Union] -> Text
renderUnions xs =
    map Display.packDoc xs
        |> Text.unlines


