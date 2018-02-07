{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module Helm.Toolchain.Frontend.Crawl.Driver (
    collectHelmFiles
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
-- *




collectHelmFiles :: [Path.Path Path.Abs Path.Dir] -> IO [Path.Path Path.Abs Path.File]
collectHelmFiles dirs = do
    (_, filter -> files) <- List.unzip <$> M.mapM PIO.listDirRecur dirs
    
    
    return files
    
    where
        filter :: [[Path.Path Path.Abs Path.File]] -> [Path.Path Path.Abs Path.File]
        filter (flatten -> files) = filterHelmFiles files



filterHelmFiles :: [Path.Path Path.Abs Path.File] -> [Path.Path Path.Abs Path.File]
filterHelmFiles = List.filter isHelmFile


isHelmFile :: Path.Path Path.Abs Path.File -> Bool
isHelmFile (Path.fileExtension -> ext)
    | ext == ".helm" = True
    | otherwise      = False






