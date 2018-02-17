{-# LANGUAGE NoImplicitPrelude #-}
module Helm.Toolchain.Frontend.Compile.Pipeline.HelmSyntax (
    pipeline
  , pipeline'
  , toHelmFlat
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    (return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun
import qualified Data.String                  as String


import qualified System.IO as SIO

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


-- + OS APIS & Related
import qualified Path
import qualified Path.IO as PIO

-- + Graphing & Related
import qualified Algebra.Graph              as G
import qualified Algebra.Graph.Export.Dot   as Dot
import qualified Algebra.Graph.AdjacencyMap as AM

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP


-- + Helm Toolchain
import qualified Helm.Toolchain.Frontend.Data.Node   as Node
import qualified Helm.Toolchain.Frontend.Data.Report as Report

-- + IRs
import qualified SLIR.HelmSyntax.Pipeline as HelmSyntax
import qualified HLIR.HelmFlat.Pipeline   as HelmFlat

-- ++ HelmSyntax Interfaces
import qualified SLIR.HelmSyntax.Module.Data.Interface         as Module
import qualified SLIR.HelmSyntax.Program.Data.Interface        as Program
import qualified SLIR.HelmSyntax.Module.System.InitDeps.Driver as InitDeps
-- *

-- | Root HelmSyntax Pipeline
--
pipeline :: IO (Either Text [[Node.Node]]) -> IO (Either Text Program.Program)
pipeline upstream = do
    result <- upstream
    
    case result of
        Left err    -> return $ Left err
        Right nodes ->
            pipeline' nodes

pipeline' :: [[Node.Node]] -> IO (Either Text Program.Program)
pipeline' nodes =
    nodes
        |> modulePipeline
        |> programPipeline


-- | Root HelmSyntax Feeds
--

toHelmFlat :: IO (Either Text Program.Program) -> IO (Either Text HelmFlat.Program)
toHelmFlat upstream = do
    result <- upstream
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ Right $ HelmSyntax.toHelmFlat' payload


-- | Internal Wrappers & Whatnot
--

programPipeline :: IO (Either Text [Module.Module])
                -> IO (Either Text Program.Program)
programPipeline upstream = do
    result <- upstream
    case result of
        Left err      -> return $ Left err
        Right payload -> do

            let payload' = map (return . Right) payload :: [IO (Either Text Module.Module)]

            HelmSyntax.programPipeline payload'


modulePipeline :: [[Node.Node]] -> IO (Either Text [Module.Module])
modulePipeline = initModuleBuild


initModuleBuild :: [[Node.Node]] -> IO (Either Text [Module.Module])
initModuleBuild nodes =
    rootModuleBuild nodes []


-- init :: Either Report.BuildError [[Node.Node]] 
rootModuleBuild :: [[Node.Node]]
                -> [Module.Module]
                -> IO (Either Text [Module.Module])

rootModuleBuild [] deps = return $ Right deps
rootModuleBuild (stage1:rest) deps = do
    result <- buildModules deps stage1
    
    case result of
        Left err      -> return $ Left err
        Right stage1' ->
            rootModuleBuild rest (stage1' ++ deps)


buildModules :: [InitDeps.ForeignModule] -> [Node.Node] -> IO (Either Text [Module.Module])
buildModules deps nodes = do
    (errors, modules) <- Either.partitionEithers <$> M.mapM (buildModule deps) nodes
    
    if not (null errors) then
        return
            $ Left
            $ Text.unlines errors
    
    else
        return
            $ Right modules



buildModule :: [InitDeps.ForeignModule] ->  Node.Node -> IO (Either Text Module.Module)
buildModule deps node =
    let path = Node.path node
        name = Node.name node
    in do
        result <- HelmSyntax.modulePipeline deps (Path.toFilePath path)
        case result of
            Right payload -> return $ Right payload
            Left err      -> return $ Left $ formatError err
            -- Left err ->
            --     error $ String.unlines
            --         (map PP.prettyShow deps)
            -- Left err ->
            --     error $ PP.prettyShow node


formatError :: Text -> Text
formatError err =
    Text.unlines
        [ Text.pack "Compiler Error:"
        , err
        ]


