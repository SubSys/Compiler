{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.TypeCheck.Data.Unification.Solver where


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
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Unification.System     as Sys
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Unification            as Unify
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Canonical.Ident        as CID
-- *



-- | Run the constraint solver
runSolve :: [Con.Constraint] -> Either Report.TypeError Sub.Subst
runSolve cs =
    let
        st = (Sub.emptySubst, cs)
    in
        M.runIdentity $ M.runExceptT $ solver st



-- Unification solver
solver :: Sys.Unifier -> Sys.Solve Sub.Subst
solver (su, cs) =
    case cs of
        [] -> return su
        ((t1, t2): cs0) -> do
            su1  <- Unify.unify t1 t2
            solver (su1 `Sub.compose` su, Sub.apply su1 cs0)







