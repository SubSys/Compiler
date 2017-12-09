{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.TypeCheck.Data.Unification (
    unify
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
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Env                    as Env
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Subst                  as Sub
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.System                 as Sys
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Unification.System     as Sys
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Canonical.Ident        as CID
-- *




-- *
-- | Unification
-- *


unify :: T.Type -> T.Type -> Sys.Solve Sub.Subst
unify t1 t2 | t1 == t2 = return Sub.emptySubst


unify (T.Var v _) t = v `bind` t
unify t (T.Var v _) = v `bind` t


unify (T.List t1 _) (T.List t2 _) = unify t1 t2

unify (T.Parens t1 _) t2 = unify t1 t2
unify t1 (T.Parens t2 _) = unify t1 t2

unify (T.Arr t1 t2 _) (T.Arr t3 t4 _) =
    unifyMany [t1, t2] [t3, t4]

unify t1@(T.Union name1 args1 _) t2@(T.Union name2 args2 _)
    | name1 == name2 =
        unifyMany args1 args2
    
    
    | otherwise =
        M.throwError $ Report.UnificationFail t1 t2


unify t1 t2 = M.throwError $ Report.UnificationFail t1 t2



bind ::  ID.Low -> T.Type -> Sys.Solve Sub.Subst
bind a t | t == T.Var' a     = return Sub.emptySubst
         | occursCheck a t   = M.throwError $ Report.InfiniteType a' t
         | otherwise         = return (Sub.Subst $ Map.singleton a' t)
    where
        a' = CID.ident a





-- *
-- | Internal Utils
-- *


occursCheck :: ID.Low -> T.Type -> Bool
occursCheck a t = CID.ident a `Set.member` Sub.ftv t





unifyMany :: [T.Type] -> [T.Type] -> Sys.Solve Sub.Subst
unifyMany [] [] = return Sub.emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
    do
        su1 <- unify t1 t2
        su2 <- unifyMany (Sub.apply su1 ts1) (Sub.apply su1 ts2)
        return (su2 `Sub.compose` su1)

unifyMany t1 t2 = M.throwError $ Report.UnificationMismatch t1 t2



