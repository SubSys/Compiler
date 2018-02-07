{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module Helm.Toolchain.Frontend.Ordering.Validations (
    importsSelf
  , importCycles
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


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + Helm Toolchain
import qualified Helm.Toolchain.Frontend.Data.Node   as Node
import qualified Helm.Toolchain.Frontend.Data.Report as Report

-- ++ Helm Toolchain - Drivers
import qualified Helm.Toolchain.Frontend.Extract.Driver as Extract
import qualified Helm.Toolchain.Frontend.Crawl.Driver   as Crawl

-- + Local
import qualified Helm.Toolchain.Frontend.Ordering.Validations.Utils as Util
-- *





importsSelf :: [Node.Node] -> Either Report.BuildError [Node.Node]
importsSelf nodes
    | null errors = Right valids
    | otherwise =
        if List.length errors >= 2 then
            Left $ Report.BuildErrors errors
        else
            Left $ List.head errors
    where
        (errors, valids) = Either.partitionEithers $ map importsSelf' nodes


importsSelf' :: Node.Node -> Either Report.BuildError Node.Node
importsSelf' node@(Node.Node filePath namespace imports)
    | not (namespace `List.elem` imports) = Right node
    | otherwise =
        Left $ Report.ImportsSelf node





-- |
-- TODO:
-- * Produces redundant error messages for each module cycle pair
-- * Perhaps implement a more efficient implementation?
importCycles :: [Node.Node] -> Either Report.BuildError [Node.Node]
importCycles nodes@(importCycles' -> xs)
    | not (null xs) = Left (Report.BuildErrors xs)
    | otherwise     = Right nodes

importCycles' :: [Node.Node] -> [Report.BuildError]
importCycles' = flatten . Util.forEach checkImports




-- |
-- NOTE: Although, I like the conciseness of this…
checkImports :: Node.Node -> Node.Node -> Maybe Report.BuildError
checkImports node1@(Node.Node filePath1 namespace1 imports1)
             node2@(Node.Node filePath2 namespace2 imports2)

    |  namespace1 `List.elem` imports2
    && namespace2 `List.elem` imports1 = Just $ Report.ImportsCycle node1 node2

    | otherwise = Nothing



-- TODO:
-- * duplicate module names











