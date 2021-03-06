{-# LANGUAGE NoImplicitPrelude #-}
module DevKit.Sample.Loader.CPU (
    alphaFilePath
  , betaFilePath
  , gammaFilePath
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

import qualified System.IO      as SIO
import qualified System.Process as SP

-- + Graphing & Related
import qualified Algebra.Graph              as G
import qualified Algebra.Graph.Export.Dot   as Dot
import qualified Algebra.Graph.AdjacencyMap as AM


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP
-- *




{-# ANN module ("HLint: ignore" :: String) #-}




getProjectRoot :: IO String
getProjectRoot = do
    SP.readCreateProcess (SP.shell "stack path --project-root" ) ""


inputFilePath filePath = do
    projectDir <- getProjectRoot
    
    return $ trim projectDir ++ trim filePath

    where
        trim = List.filter (/= '\n')


alphaFilePath :: IO String
alphaFilePath =
    inputFilePath "/etc/resources/dev-samples/cpu/Alpha.helm"


betaFilePath :: IO String
betaFilePath =
    inputFilePath "/etc/resources/dev-samples/cpu/Beta.helm"


gammaFilePath :: IO String
gammaFilePath =
    inputFilePath "/etc/resources/dev-samples/cpu/Gamma.helm"










