{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.TypeCheck.Driver.Module where


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
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload as Payload

-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Interface.TypesEnv     as TI
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Subst                  as Sub
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System                 as Sys
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.TypeSystem             as TS
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident              as CID
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System.Constraints     as Con
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System.Scope           as Scope


-- ~ Infer Decls
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Syntax.Decl as Decl

-- ~ Init Stuff
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Init.Unions as Union
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Init.Overloads as Overloads

-- ~ Finish
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Resolve as Resolve
-- *