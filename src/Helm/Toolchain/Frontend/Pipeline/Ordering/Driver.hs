{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module Helm.Toolchain.Frontend.Pipeline.Ordering.Driver (
    buildOrdering
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
import qualified Helm.Toolchain.Frontend.Pipeline.Data.Node   as Node
import qualified Helm.Toolchain.Frontend.Pipeline.Data.Report as Report

-- ++ Helm Toolchain - Drivers
import qualified Helm.Toolchain.Frontend.Pipeline.Extract.Driver as Extract
import qualified Helm.Toolchain.Frontend.Pipeline.Crawl.Driver   as Crawl

-- + Local
import qualified Helm.Toolchain.Frontend.Pipeline.Ordering.Validations as Validate
import qualified Helm.Toolchain.Frontend.Pipeline.Ordering.Debug.Utils as DebugUtil
-- *


-- | 
-- It’s based off of `Data.Graph` topological ordering, with the extended idea of using nested nodes to denote parallel compilations for each sequence…
buildOrdering :: [Node.Node] -> Either Report.BuildError [[Node.Node]]
buildOrdering input =
    case Validate.importsSelf input of
        Left err -> Left err
        _ -> case Validate.importCycles input of
            Left err -> Left err
            _        -> Right $ buildOrdering' input


buildOrdering' :: [Node.Node] -> [[Node.Node]]
buildOrdering' = parallelize . fst . serialize



serialize (init -> (graph, records)) =
    case AM.topSort graph of
        Nothing -> error "Invalid dependency ordering!"
        Just (List.reverse -> xs) -> (packOrder xs records, DebugUtil.graphSerialOrder xs)


parallelize :: [Node.Node] -> [[Node.Node]]
parallelize = parallelOrder


packOrder :: [Text] -> Map.Map Text Node.Node -> [Node.Node]
packOrder [] _ = []
packOrder ((Node.isSudoFFI' -> True):names) records =
    packOrder names records


packOrder (name:names) records
    | Just node <- Map.lookup name records =
        node : packOrder names records


-- TODO: Improve me! (This is a very limited, naive method…)
-- 
parallelOrder :: [Node.Node] -> [[Node.Node]]
parallelOrder = List.groupBy parallelPred


parallelPred :: Node.Node -> Node.Node -> Bool
parallelPred (Node.Node _ namespace1 [])
             (Node.Node _ namespace2 []) = True

parallelPred (Node.Node _ namespace1 imports1)
             (Node.Node _ namespace2 imports2) =
    imports1 == imports2









init :: [Node.Node] -> (AM.AdjacencyMap Text, Map.Map Text Node.Node)
init nodes =
    (AM.fromAdjacencyList points, Map.unions records)
    where
        (points, records) = List.unzip $ map initNode nodes


-- initNode :: Node.Node -> (G.Graph Text, Map.Map Text Node.Node)
initNode node@(Node.Node filePath (Node.toText -> moduleName) (Node.toTexts -> imports)) =
    let
        record = Map.singleton moduleName node
    in
        ((moduleName, imports), record)


-- node2Graph :: Node.Node -> G.Graph Text
-- node2Graph (Node.Node)





