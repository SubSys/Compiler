{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.Pipeline (
    pipeline
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M

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
import qualified HLIR.HelmCore.Render.Utils as Display


-- ~ Upstream
import qualified SLIR.HelmSyntax.Core as HelmSyntax


--- Local Deps
-- ~ HelmCore Drivers
import qualified HLIR.HelmCore.Core.Indexing.Driver  as Driver
import qualified HLIR.HelmCore.Core.TypeCheck.Driver as Driver

-- ~ HelmCore IR
import qualified HLIR.HelmCore.Data.Payload as Payload

-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as ID
import qualified HLIR.HelmCore.AST.Base.Types  as T
import qualified HLIR.HelmCore.AST.Base.Values as V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as Decl

--- Local
import qualified HLIR.HelmCore.Core.Indexing.Data.Subst            as Sub
import qualified HLIR.HelmCore.Core.Indexing.Data.System           as Sys
import qualified HLIR.HelmCore.Core.Indexing.Data.System.Bindable  as Bind
import qualified HLIR.HelmCore.Core.Indexing.Data.System.Referable as Ref
import qualified HLIR.HelmCore.Core.Indexing.Syntax                as Syntax
-- *


{-# ANN module "HLint: ignore" #-}



pipeline :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
pipeline upstream =
    upstream
        |> Driver.typeCheck
        |> Driver.stdIndexer


