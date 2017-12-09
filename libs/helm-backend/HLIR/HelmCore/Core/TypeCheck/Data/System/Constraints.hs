{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.TypeCheck.Data.System.Constraints (
      unify
    , app
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

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni


--- Local Deps
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
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Env                    as Env
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmCore.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmCore.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmCore.Core.TypeCheck.Data.System.Scope           as Scope
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Canonical.Ident        as CID
-- *


-- |
-- Simply generate a constraint on the two provided types.
--
unify :: T.Type -> T.Type -> Sys.Constrain
unify t1 t2 = M.tell [(t1, t2)]



app :: T.Type -> T.Type -> Sys.Constrain' T.Type
app t1 t2 = do
    tv <- TS.freshType
    
    unify t1 (t2 `T.Arr` tv)
    
    return tv












