{-# LANGUAGE NoImplicitPrelude #-}
module Helm.Toolchain.Core.Initialize.Header.Pipeline (
    processHeaders
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Core.Text.Util    (punctuate)

import Prelude (return, String, IO, show, error, (<$>))
import System.FilePath (FilePath)
import Data.List.Index  (imap)

import qualified Control.Monad.State         as M
import qualified Control.Monad.Except        as M
import qualified Control.Monad.RWS           as M
import qualified Control.Monad.Identity      as M
import qualified Control.Monad.Reader        as M
import qualified Data.List                   as List
import qualified Data.Either                 as Either
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

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Base       as Base
import qualified SLIR.HelmSyntax.AST.Data.Header.ImportDecl as Decl

--- Local
import qualified Helm.Toolchain.System.Data.Report     as Report
import qualified Helm.Toolchain.System.Data.BuildLog   as Log
import qualified Helm.Toolchain.System.Data.BuildOrder as BuildOrder

-- ~ Header Pipeline
import qualified Helm.Toolchain.Core.Initialize.Header.Pipeline.Parse   as Parse
import qualified Helm.Toolchain.Core.Initialize.Header.Pipeline.ToGraph as ToGraph
-- *




-- NOTE: an alternative could be simply: `M.mapM Header.processHeader files`…
-- Although, should I report the first error encountered, or report as many errors as possible?…
-- - Since perhaps the latter would overwhelm the developer.

processHeaders :: [FilePath] -> IO (Either Report.BuildError (Log.BuildLog, AG.Graph Text))
processHeaders [] = return $ Right (Map.empty, AG.empty)
processHeaders (x1:xs) = do
    result1 <- processHeader x1
    case result1 of
        Left err -> return $ Left err
        Right (env1, node1) -> do
            rest <- processHeaders xs
            case rest of
                Left err -> return $ Left err
                Right (restEnv, restNodes) ->
                    return
                        $ Right (Map.union env1 restEnv, AG.overlay node1 restNodes)




-- | Process Header
-- With the given file-path to a helm-module, will 
-- 1. Parse just the module header, to obtain a list of import declarations
-- 2. Converts import declarations to a graph representing import deps
--     - Via `Graph.edges` - `List.zip (List.repeat moduleName') imports’`

processHeader :: FilePath -> IO (Either Report.BuildError (Log.BuildLog, AG.Graph Text))
processHeader modulePath = do
    result <- Parse.parseHeader modulePath
    
    case result of
        Left err ->
            return $ Left err

        Right payload ->
            return
                $ ToGraph.header2Graph payload











