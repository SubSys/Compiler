{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.TypeCheck.Data.Unification.Solver (
    runSolve
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
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Unification.System     as Sys
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Unification            as Unify
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident        as CID

import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Subst                  as Sub
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Subst.Base             as Sub

-- ~ Pretty Printers
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Subst.Debug as Sub
-- *



-- | Run the constraint solver
runSolve :: [Con.Constraint] -> Either Report.TypeError Sub.RootSubst
runSolve cs =
    let
        st = (Sub.emptySubst, cs)
    in
        M.runIdentity $ M.runExceptT $ solver st


runSolve' :: [Con.Constraint] -> Sub.Subst -> Either Report.TypeError Sub.RootSubst
runSolve' cs sub =
    let
        st = (sub, cs)
    in
        M.runIdentity $ M.runExceptT $ solver st


-- Unification solver
solver :: Sys.Unifier -> Sys.Solve Sub.RootSubst
solver (su, cs) =
    case su of
        Sub.Base sub ->
    
    
    case cs of
        [] -> return su
        (Con.Constraint t1 t2: cs0) -> do
            su1  <- Unify.unify t1 t2
            solver (su1 `Sub.compose` su, Sub.apply su1 cs0)
        
        (Con.OverloadedConstraint ty ts:cs0) ->
            -- case solveOverloadedConstraint ty ts of
            -- 
            --     Left err -> M.throwError err
            --     Right su1 -> 
            --         solver (su1 `Sub.compose` su, Sub.apply su1 cs0)

            case solveOverloadedConstraint' ty ts of
                Left err -> M.throwError err
                Right subs ->
                    solverTrials subs cs0
        
        
        -- (Con.OverloadedConstraint ty ts:cs0) ->
        --     case solveOverloadedConstraint' ty ts of
        --         Left err -> M.throwError err
        --         Right subs ->
        --             case solverTrials subs cs0 of
        --                 Left err -> M.throwError err
        --                 Right s  ->
        --                     return s



solver :: Sys.Unifier -> Sys.Solve Sub.Subst
solver (su, cs) =
    case cs of
        [] -> return su
        ((t1, t2): cs0) -> do
            su1  <- Unify.unify t1 t2
            solver (su1 `Sub.compose` su, Sub.apply su1 cs0)





solverTrials :: [Sub.Subst] -> [Con.Constraint] -> Sys.Solve Sub.RootSubst
solverTrials subs cs0 =
    let result = map (runSolve' cs0) subs
        
        valids = Either.rights result
    
        -- debug =
        --     Either.rights result
        --         |> map Sub.debugSubst
        --         |> Text.unlines
        --         |> Text.unpack
        
    in
        return $ Sub.Super valids


-- *
-- | Internal Helpers
-- *

solveOverloadedConstraint' :: T.Type -> [T.Type] -> Either Report.TypeError [Sub.Subst]
solveOverloadedConstraint' ty ts =
    case foundValid' (ty, ts) result of
        Left err -> Left err
        Right subs -> Right subs

    where
        sets = map (\x -> [Con.Constraint ty x]) ts
        result = map runSolve sets


foundValid' :: (T.Type, [T.Type]) -> [Either Report.TypeError Sub.Subst] -> Either Report.TypeError [Sub.Subst]
foundValid' (t, ts) xs
    | null rights =
        Left $ Report.OverloadedTypeFail t ts
    
    | otherwise =
        Right rights
    
    where
        lefts = Either.lefts xs
        rights = Either.rights xs



