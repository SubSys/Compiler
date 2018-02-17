{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE PatternGuards #-}
module Helm.Toolchain.Frontend.Manifest.File.Parse (
    parseManifest
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


parseManifest :: Path.Path Path.Abs Path.Dir -> IO (Either Report.ManifestParseError Manifest.Package)
parseManifest path = do
    result <- Toml.parseTomlDoc "helm-manifest" <$> TIO.readFile (Path.toFilePath path)
    
    case result of
        Left error ->
            return $ Left $ Report.ParseError $ Text.pack $ PP.prettyShow error
        Right doc  ->
            serialize path doc



serialize :: Path.Path Path.Abs Path.Dir -> T.Table -> IO (Either Report.ManifestParseError Manifest.Package)
serialize rootDir doc
    | Just (T.VTable package) <- HMap.lookup (Text.pack "package") doc =
        getPackageFields rootDir package


getPackageFields :: Path.Path Path.Abs Path.Dir -> T.Table -> IO (Either Report.ManifestParseError Manifest.Package)
getPackageFields rootDir package
    | Just (T.VString name) <- HMap.lookup (Text.pack "name") package
    , Just (T.VString version) <- HMap.lookup (Text.pack "version") package
    , Just (T.VArray sources) <- HMap.lookup (Text.pack "sources") package = do
        paths <- map (rootDir </>) <$> M.mapM Path.parseRelDir (extractArray sources)
        
        return $ Right Manifest.Package
            { Manifest.name = name
            , Manifest.sources = paths
            , Manifest.version = Just version
            }



extractArray :: Vector.Vector T.Node -> [Pre.FilePath]
extractArray (Vector.toList -> xs) = map extract' xs
    where
        extract' :: T.Node -> Pre.FilePath
        extract' (T.VString txt) = Text.unpack txt



