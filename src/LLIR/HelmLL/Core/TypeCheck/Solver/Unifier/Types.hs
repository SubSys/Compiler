{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE PatternGuards #-}
module LLIR.HelmLL.Core.TypeCheck.Solver.Unifier.Types (
    unify
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
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
import qualified Data.Data                    as Data
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP




-- + HelmLL Module Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fn
import qualified LLIR.HelmLL.AST.Utils.Generic.SudoFFI     as SudoFFI

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as T
import qualified LLIR.HelmLL.AST.Data.Base.Literals   as V

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as Decl

-- + Local
import qualified LLIR.HelmLL.Core.TypeCheck.Solver.Data.System              as Sys
import qualified LLIR.HelmLL.Core.TypeCheck.Data.Report                     as Report
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.Env              as Env
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Utils.TypeSystem      as TS
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Utils.General         as Util
import qualified LLIR.HelmLL.Core.TypeCheck.Solver.Data.Constraint          as Con
import qualified LLIR.HelmLL.Core.TypeCheck.Subst.Types                     as TySub
-- *



{-
    # Unification
-}




unify :: T.Type -> T.Type -> Sys.Solve TySub.Subst
unify t1 t2 | t1 == t2 = return TySub.emptySubst


unify (T.Var v) t = v `bind` t
unify t (T.Var v) = v `bind` t


unify (T.List t1) (T.List t2) = unify t1 t2

unify (T.Arr t1 t2) (T.Arr t3 t4) =
    unifyMany [t1, t2] [t3, t4]

unify t1@(T.Union name1 args1) t2@(T.Union name2 args2)
    | name1 == name2 =
        unifyMany args1 args2
    
    
    | otherwise =
        M.throwError $ Report.UnificationFail t1 t2




unify t1 t2 = M.throwError $ Report.UnificationFail t1 t2



bind ::  ID.Ident -> T.Type -> Sys.Solve TySub.Subst
bind a t | t == T.Var a     = return TySub.emptySubst
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



