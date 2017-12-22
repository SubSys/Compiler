{-# LANGUAGE NoImplicitPrelude #-}
module Helm.Toolchain.Core.Initialize.FileSystem.Crawl (
    getHelmFilePaths
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
-- *





getHelmFilePaths :: FilePath -> IO [FilePath]
getHelmFilePaths rootDir = do
    dirs <- getSubDirs rootDir

    files1 <- flatten <$> M.mapM getHelmFilesFromDir [rootDir]
    files2 <- flatten <$> M.mapM getHelmFilesFromDir dirs
    
    return $ files1 ++ files2




-- *
-- | Internal Helpers
-- *




getHelmFilesFromDir :: FilePath -> IO [FilePath]
getHelmFilesFromDir dirPath = do
    paths0 <- SD.listDirectory dirPath
    paths1 <- M.filterM SD.doesFileExist $ map (FP.combine dirPath) paths0
    
    return $ List.filter isHelmFile paths1



isHelmFile :: FilePath -> Bool
isHelmFile path
    | FP.hasExtension path =
        FP.takeExtension path == ".helm"

    | otherwise =
        False


getSubDirs :: FilePath -> IO [FilePath]
getSubDirs path = do
    paths0 <- SD.listDirectory path
    paths1 <- M.filterM SD.doesDirectoryExist $ map (FP.combine path) paths0
    
    rest <- flatten <$> M.mapM getSubDirs paths1
    
    return $ paths1 ++ rest




