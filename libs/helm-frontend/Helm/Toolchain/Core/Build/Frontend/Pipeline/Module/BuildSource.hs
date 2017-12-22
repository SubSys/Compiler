{-# LANGUAGE NoImplicitPrelude #-}
module Helm.Toolchain.Core.Build.Frontend.Pipeline.Module.BuildSource (
    buildSource
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Core.Text.Util    (punctuate)

import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>))
import System.FilePath (FilePath)
import Data.List.Index  (imap)

import qualified Control.Monad.State         as M
import qualified Control.Monad.Except        as M
import qualified Control.Monad.RWS           as M
import qualified Control.Monad.Identity      as M
import qualified Control.Monad.Reader        as M
import qualified Data.List                   as List
import qualified Data.Char                   as Char
import qualified Data.Either                 as Either
import qualified Data.Maybe                  as Maybe
import qualified Data.Text                   as Text
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Foldable               as Fold
import qualified Data.Monoid                 as Monoid
import qualified Data.Text.IO                as TIO
import qualified Data.Hashable               as Hash
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint       as PP
import qualified System.IO                   as SIO
import qualified System.FilePath             as FP
import qualified System.Directory            as SD
import qualified Algebra.Graph               as AG
import qualified Algebra.Graph.Class         as AGC
import qualified Algebra.Graph.AdjacencyMap  as AM
import qualified Algebra.Graph.Export        as Export
import qualified Algebra.Graph.Export.Dot    as ExportDot



--- Local Deps
-- ~ HelmSyntax Misc.
import qualified SLIR.HelmSyntax.Render.Utils as Display

-- ~ HelmSyntax Interfaces
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload     as Module
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload    as Program
import qualified SLIR.HelmSyntax.Data.Interface.Utils              as IUtil

-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Base       as Base
import qualified SLIR.HelmSyntax.AST.Data.Header.ImportDecl as Decl

-- ~ Frontend Compiler
import qualified Helm.Toolchain.Core.Compiler.Frontend.Driver as Frontend


--- Local
import qualified Helm.Toolchain.System.Data.Report     as Report
import qualified Helm.Toolchain.System.Data.BuildLog   as Log
import qualified Helm.Toolchain.System.Data.BuildOrder as BuildOrder
-- *



buildSource :: [Module.Module]
      -> Log.BuildLog
      -> (BuildOrder.ModuleName, BuildOrder.ModuleImports)
      -> IO (Either Text Module.Module)

buildSource deps buildLog (moduleName, _)
    | Just header <- Map.lookup moduleName buildLog =
        let
            filePath = Module.modulePath header
        in
            Frontend.sourcePipeline deps (Text.unpack filePath)
    
    | otherwise =
        return
            $ Left
            $ Text.pack
            $ PP.prettyShow
            $ Report.ModuleNotFound moduleName



