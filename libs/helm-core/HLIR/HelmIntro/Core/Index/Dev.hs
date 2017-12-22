{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.Index.Dev where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)

import Prelude (return, String, IO, show, error, (<$>), (>>))

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


--- Local Deps
-- ~ HelmIntro Interfaces
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

-- ~~ Auxiliary Nodes
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident as CID

--- Local
import qualified HLIR.HelmIntro.Core.Index.Data.Subst            as Sub
import qualified HLIR.HelmIntro.Core.Index.Data.System           as Sys
import qualified HLIR.HelmIntro.Core.Index.Data.System.Bindable  as Bind
import qualified HLIR.HelmIntro.Core.Index.Data.System.Referable as Ref
import qualified HLIR.HelmIntro.Core.Index.Syntax                as Syntax
-- *


{-# ANN module "HLint: ignore" #-}



upstream =
    ParserSample.sampleOne
        |> HelmSyntax.frontend
        |> HelmSyntax.toHelmCore
        |> Driver.typeCheck
        |> Driver.stdIndexer



run = do
    input <- upstream
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload =
    
    putStrLn $ Text.unpack $ Display.renderFunctions fns

    where
        fns = Payload.getFunctions payload

