{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Unification.Solver where


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
import qualified Text.Show.Prettyprint as PP

import qualified Data.Either as Either
import qualified Data.String as String


--- Local Deps
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
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Unification.Constraint as Con
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Unification.System     as Sys
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Unification            as Unify
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident              as CID

import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Subst                  as Sub
-- *





-- | Run the constraint solver
runSolve :: [Con.Constraint] -> Either Report.TypeError Sub.Subst
runSolve cs =
    let
        st = (Sub.emptySubst, cs)
    in
        fst $ M.runState (M.runExceptT $ solver st) Sys.initCounter



-- Unification solver
solver :: Sys.Unifier -> Sys.Solve Sub.Subst
solver (su, cs) =
    case cs of
        [] -> return su
        ((t1, t2): cs0) -> do
            su1  <- Unify.unify t1 t2
            solver (su1 `Sub.compose` su, Sub.apply su1 cs0)







