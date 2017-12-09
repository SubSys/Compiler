{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Dev.Labs.DryRun where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP

import qualified CGIR.Rust.Render.Syntax as Display

-- ~ Upstream
import qualified SLIR.HelmSyntax.Core  as HelmSyntax
import qualified HLIR.HelmCore.Core    as HelmCore
import qualified HLIR.HelmOutro.Core   as HelmOutro

import qualified LLIR.LightRoast.Core as LightRoast



--- Local Deps
-- ~ LightRoast Payload
import qualified CGIR.Rust.Data.Payload as Payload

-- ~ (GCIR) - Rust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Types  as T
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified CGIR.Rust.AST.TermLevel.Stmt        as S
import qualified CGIR.Rust.AST.TermLevel.Patterns    as P
import qualified CGIR.Rust.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.Functions as Decl
import qualified CGIR.Rust.AST.TopLevel.Unions    as Decl


--- Local
import qualified CGIR.Rust.Core.Indexing.Driver as Driver

-- TODO...
import qualified CGIR.Rust.Core.Paths.Dev as InDevDriver
-- *


{-# ANN module "HLint: ignore" #-}

upstream =
    ParserSample.sampleOne
        |> HelmSyntax.frontend
        |> HelmSyntax.toHelmCore
        |> HelmCore.pipeline
        |> HelmCore.toHelmOutro
        |> HelmOutro.pipeline
        |> HelmOutro.toLightRoast
        |> LightRoast.toRust
        |> Driver.globalIndexer
        |> InDevDriver.updateConstrPaths



run = do
    input <- upstream
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload = do
    
    putStrLn $ Text.unpack $ Display.renderUnions unions
    putStrLn $ Text.unpack $ Display.renderFunctions functions
    
    
    where
        unions = Payload.getUnions payload
        functions  = Payload.getFunctions payload

