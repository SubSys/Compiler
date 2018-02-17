{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module Helm.Toolchain.Frontend.Pipeline.Ordering.Debug.Utils (
    node2Graph
  , nodes2Graphs
  , graphParallelOrder
  , graphSerialOrder
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
-- *



defaultListStyle graph =
    let
        xs = Dot.defaultStyle (String.fromString . show)
    in
        xs




graphSerialOrder :: [Text] -> G.Graph Text
graphSerialOrder = G.path 






node2Graph :: Node.Node -> G.Graph Text
node2Graph (Node.Node filePath (Node.toText -> namespace) (Node.toTexts -> imports)) =
    G.edges $ List.zip (List.repeat namespace) imports


nodes2Graphs :: [Node.Node] -> [G.Graph Text]
nodes2Graphs = map node2Graph

group2Graph :: [Node.Node] -> G.Graph [Text]
group2Graph (nodes2Text -> nodes) =
    G.vertex nodes

group2Text :: [Node.Node] -> [Text]
group2Text = map node2Text


graphParallelOrder :: [[Node.Node]] -> G.Graph [Text]
graphParallelOrder [] = G.empty
graphParallelOrder (map group2Text -> nodes) =
    G.path nodes



-- | Misc. Internal Utils

node2Text :: Node.Node -> Text
node2Text (Node.Node _ ns _) = Node.toText ns

nodes2Text = map node2Text