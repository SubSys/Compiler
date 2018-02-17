{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE PatternGuards #-}
module SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Unifier.Types (
    unify
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope           as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident as ID

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl


-- + Local
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Data.System              as Sys
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Data.Report                     as Report
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.Env              as Env
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Utils.TypeSystem      as TS
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Utils.General         as Util
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Data.Constraint          as Con
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Subst.Types                     as TySub
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Unifier.Types.Superposed as Superposed
-- *



{-
    # Unification
-}




unify :: T.Type -> T.Type -> Sys.Solve TySub.Subst
unify t1 t2 | t1 == t2 = return TySub.emptySubst


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



-- unify t1 (T.Superposed (con:_) ts) = unifyOverloaded (T.Var' con) t1 ts
-- unify (T.Superposed (con:_) ts) t1 = unifyOverloaded (T.Var' con) t1 ts

unify t1 (T.Superposed cons ts) = resolveSuperposed (cons, ts) t1
unify (T.Superposed cons ts) t1 = resolveSuperposed (cons, ts) t1



unify t1 t2 = M.throwError $ Report.UnificationFail t1 t2



bind ::  ID.Ident -> T.Type -> Sys.Solve TySub.Subst
bind a t | t == T.Var' a     = return TySub.emptySubst
         | occursCheck a t   = M.throwError $ Report.InfiniteType a t
         | otherwise         = return (TySub.Subst $ Map.singleton a t)






-- *
-- | Internal Utils
-- *


occursCheck :: ID.Ident -> T.Type -> Bool
occursCheck a t = a `Set.member` TySub.fvs t






unifyMany :: [T.Type] -> [T.Type] -> Sys.Solve TySub.Subst
unifyMany [] [] = return TySub.emptySubst
unifyMany (t1 : ts1) (t2 : ts2) =
    do
        su1 <- unify t1 t2
        su2 <- unifyMany (TySub.apply su1 ts1) (TySub.apply su1 ts2)
        return (su2 `TySub.compose` su1)

unifyMany t1 t2 = M.throwError $ Report.UnificationMismatch t1 t2




resolveSuperposed :: ([ID.Ident], [T.Type]) -> T.Type -> Sys.Solve TySub.Subst
resolveSuperposed (cons@(con:_), ts) t2
    | isEmptySubst valids =
        return $ TySub.Subst $ Map.singleton con t2

    | not (null valids) =
        return
            $ TySub.Subst
            $ Map.unionsWith toSuper
            $ map extract valids

    | otherwise =
        M.throwError (Report.OverloadedTypeFail t2 ts)

    where
        extract (TySub.Subst s) = s

        result = map (M.runIdentity . M.runExceptT . unifyTrial) ts

        valids = Either.rights result
        invalids = Either.lefts result

        unifyTrial = unify t2

        toSuper :: T.Type -> T.Type -> T.Type
        toSuper t1 t2 = T.Superposed [con] [t1, t2]


isEmptySubst :: [TySub.Subst] -> Bool
isEmptySubst [TySub.Subst x]
    | Map.empty == x = True


isEmptySubst _ = False


