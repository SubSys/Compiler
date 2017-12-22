{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.TypeCheck.Driver.Program where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)

import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)

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


--- Local Deps
-- ~ HelmSyntax IR
import qualified HLIR.HelmIntro.Data.Interface.Module.Payload as Payload

-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Interface.TypesEnv     as TI
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident              as CID
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.System.Constraints     as Con
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.System.Scope           as Scope


-- ~ Infer Decls
import qualified HLIR.HelmIntro.Core.TypeCheck.Syntax.Decl as Decl

-- ~ Init Stuff
import qualified HLIR.HelmIntro.Core.TypeCheck.Init.Unions as Union
import qualified HLIR.HelmIntro.Core.TypeCheck.Init.Overloads as Overloads

-- ~ Finish
import qualified HLIR.HelmIntro.Core.TypeCheck.Resolve as Resolve
-- *