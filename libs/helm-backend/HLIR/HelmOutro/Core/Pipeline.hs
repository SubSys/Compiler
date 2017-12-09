{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Pipeline (
    pipeline
) where


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
import qualified Text.Show.Prettyprint as PP


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified HLIR.HelmOutro.Render.Utils as Display


-- ~ Upstream
import qualified SLIR.HelmSyntax.Core  as HelmSyntax
import qualified HLIR.HelmCore.Core    as HelmCore


--- Local Deps
-- ~ HelmOutro Payload
import qualified HLIR.HelmOutro.Data.Payload as Payload

-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl

-- ~ HelmOutro Drivers
import qualified HLIR.HelmOutro.Core.Indexing.Driver  as Driver
import qualified HLIR.HelmOutro.Core.PreLift.Driver   as Driver
import qualified HLIR.HelmOutro.Core.TypeCheck.Driver as Driver
import qualified HLIR.HelmOutro.Core.Lift.Driver      as Driver
-- *





pipeline :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
pipeline input =
    input
        |> Driver.typeCheck
        |> Driver.globalIndexer
        |> Driver.prelift
        |> Driver.globalIndexer
        |> Driver.lift
        |> Driver.globalIndexer
        |> Driver.typeCheck
        |> Driver.globalIndexer

