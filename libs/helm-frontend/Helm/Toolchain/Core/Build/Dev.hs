{-# LANGUAGE NoImplicitPrelude #-}
module Helm.Toolchain.Core.Build.Dev where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Core.Text.Util    (punctuate)

import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>))
import System.FilePath (FilePath)
import Data.List.Index  (imap)

import qualified Control.Monad.State         as M
import qualified Control.Monad.Except        as M
import qualified Control.Monad.RWS           as M
import qualified Control.Monad.Identity      as M
import qualified Control.Monad.Reader        as M
import qualified Data.List                   as List
import qualified Data.Char                   as Char
import qualified Data.Either                 as Either
import qualified Data.Maybe                  as Maybe
import qualified Data.Text                   as Text
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Foldable               as Fold
import qualified Data.Monoid                 as Monoid
import qualified Data.Text.IO                as TIO
import qualified Data.Hashable               as Hash
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint       as PP
import qualified System.IO                   as SIO
import qualified System.FilePath             as FP
import qualified System.Directory            as SD
import qualified Algebra.Graph               as AG
import qualified Algebra.Graph.Class         as AGC
import qualified Algebra.Graph.AdjacencyMap  as AM
import qualified Algebra.Graph.Export        as Export
import qualified Algebra.Graph.Export.Dot    as ExportDot



--- Local Deps
-- ~ HelmIntro Misc.
import qualified HLIR.HelmIntro.Render.Utils as Display
import qualified HLIR.HelmIntro.Data.Payload as Payload

-- ~ HelmIntro AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified Helm.Toolchain.System.Data.Report     as Report

import qualified Helm.Toolchain.Core.Build.Frontend.Driver as Frontend
-- *




{-# ANN module ("HLint: ignore" :: String) #-}


testbedPath = "/Users/colbyn/SubSystems/Toolchain/Compiler/resources/Testbed"

appPath = "/Users/colbyn/SubSystems/Toolchain/Compiler/resources/Testbed/src/app"
libsPath = "/Users/colbyn/SubSystems/Toolchain/Compiler/resources/Testbed/src/libs"
preludePath = "/Users/colbyn/SubSystems/Toolchain/Compiler/resources/Testbed/src/prelude/"

packageFile = "helm-package.json"




run :: IO ()
run = do
    result <- Frontend.toHelmIntro $ Frontend.frontendMaster [appPath, libsPath, preludePath]
    
    case result of
        Left err -> PP.prettyPrint err
        Right program ->
            run' program


run' program = do
    putStrLn
        $ Text.unpack
        $ Display.displayProgram program


