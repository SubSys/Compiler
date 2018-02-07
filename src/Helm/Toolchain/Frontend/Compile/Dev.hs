{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Helm.Toolchain.Frontend.Compile.Dev where


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

-- ++ Helm Toolchain - Drivers
import qualified Helm.Toolchain.Frontend.Extract.Driver  as Extract
import qualified Helm.Toolchain.Frontend.Crawl.Driver    as Crawl
import qualified Helm.Toolchain.Frontend.Ordering.Driver as Ordering


-- + Local
-- *



{-# ANN module ("HLint: ignore" :: String) #-}








rootDir = [Path.absdir|/Users/colbyn/SubSystems/Compiler/etc/resources/samples/test-project|]

debugPath = "/Users/colbyn/SubSystems/Compiler/etc/runtime-log"
debugDotfile = debugPath ++ "/modules.dot"


upstream = do
    helmFiles <- Crawl.collectHelmFiles [rootDir]
    (errors, nodes) <- Either.partitionEithers <$> M.mapM Extract.parseHeader helmFiles
    
    if not (null errors) then
        return $ Left (Report.BuildErrors errors)
    else
        return $ (Ordering.buildOrdering nodes)


run = do
    result <- upstream
    
    case result of
        Left errors -> PP.prettyPrint errors
        Right nodes ->
            run' nodes




run' nodes = do
    
    M.mapM_ (\xs -> do M.mapM_ PP.prettyPrint xs; bar) nodes
    
    where
        bar = putStrLn $ "\n" ++ List.replicate 100 '-' ++ "\n"












