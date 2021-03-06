{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
module Helm.Toolchain.Frontend.Manifest.File.Write (
    writeManifestTemplate
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
import qualified Data.HashMap.Lazy            as HMap
import qualified Data.Data                    as Data

import qualified Data.Vector                  as Vector


import qualified System.IO as SIO

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


-- + OS APIS & Related
import Path ((</>))
import qualified Path
import qualified Path.IO as PIO


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP
import qualified Data.String           as String

-- + TOML Stuff
import qualified Text.Toml       as Toml
import qualified Text.Toml.Types as T

-- + Local
import qualified Helm.Toolchain.Frontend.Manifest.Data        as Manifest
import qualified Helm.Toolchain.Frontend.Manifest.Data.Report as Report
-- *



writeManifestTemplate :: Path.Path Path.Abs Path.Dir -> IO ()
writeManifestTemplate rootDir =
    let name = [Path.relfile|./helm-manifest.toml|]
        path = rootDir </>  name
    in
        TIO.writeFile (Path.toFilePath path) contents



contents :: Text
contents = Text.unlines
    [ Text.pack "[package]"
    , Text.pack "name = \"dev-project\""
    , Text.pack "version = \"0.0.1\""
    , Text.pack "sources = [\"app\", \"src\"]"
    ]





