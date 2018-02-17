{-# LANGUAGE NoImplicitPrelude #-}
module Helm.Toolchain.Frontend.Pipeline.Compile.Pipeline (
    pipeline
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
import qualified Helm.Toolchain.Frontend.Pipeline.Extract.Driver  as Extract
import qualified Helm.Toolchain.Frontend.Pipeline.Crawl.Driver    as Crawl
import qualified Helm.Toolchain.Frontend.Pipeline.Ordering.Driver as Ordering


-- ++ HelmSyntax Interfaces
import qualified SLIR.HelmSyntax.Module.Data.Interface         as HelmSyntax.Module
import qualified SLIR.HelmSyntax.Module.System.InitDeps.Driver as InitDeps

-- + Local
import qualified Helm.Toolchain.Frontend.Pipeline.Compile.Pipeline.HelmSyntax  as HelmSyntax
import qualified Helm.Toolchain.Frontend.Pipeline.Compile.Pipeline.HelmFlat    as HelmFlat
import qualified Helm.Toolchain.Frontend.Pipeline.Compile.Pipeline.RustCG      as RustCG
-- *



pipeline :: [[Node.Node]] -> IO (Either Text Text)
pipeline nodes =
    nodes |> HelmSyntax.pipeline'
          |> HelmSyntax.toHelmFlat
          |> HelmFlat.pipeline
          |> HelmFlat.toRustCG
          |> RustCG.pipeline
          |> RustCG.toSyntax

