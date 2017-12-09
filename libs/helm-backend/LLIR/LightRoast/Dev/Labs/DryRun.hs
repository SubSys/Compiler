{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.LightRoast.Dev.Labs.DryRun where


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

import qualified LLIR.LightRoast.Render.Syntax as Display

-- ~ Upstream
import qualified SLIR.HelmSyntax.Core  as HelmSyntax
import qualified HLIR.HelmCore.Core    as HelmCore
import qualified HLIR.HelmOutro.Core   as HelmOutro

--- Local Deps
-- ~ LightRoast Payload
import qualified LLIR.LightRoast.Data.Payload as Payload

-- ~ LightRoast AST
-- ~~ Base
import qualified LLIR.LightRoast.AST.Base.Ident  as ID
import qualified LLIR.LightRoast.AST.Base.Types  as T
import qualified LLIR.LightRoast.AST.Base.Values as V
import qualified LLIR.LightRoast.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified LLIR.LightRoast.AST.TermLevel.Stmt        as S
import qualified LLIR.LightRoast.AST.TermLevel.Patterns    as P
import qualified LLIR.LightRoast.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified LLIR.LightRoast.AST.TopLevel.Functions as Decl
import qualified LLIR.LightRoast.AST.TopLevel.Unions    as Decl
--- Local
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



run = do
    input <- upstream
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload =
    -- M.mapM_ PP.prettyPrint functions
    
    putStrLn $ Text.unpack $ Display.renderFunctions functions
    
    
    where
        functions  = Payload.getFunctions payload

