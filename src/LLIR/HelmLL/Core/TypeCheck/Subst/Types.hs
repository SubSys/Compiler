{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module LLIR.HelmLL.Core.TypeCheck.Subst.Types (
    Subst(..)
  , emptySubst
  , compose
  , isEmpty
  , TypeSubstitutable(..)
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
-- ++ 
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.Env     as Env
import qualified LLIR.HelmLL.Core.TypeCheck.Solver.Data.Constraint as Con
-- *



newtype Subst = Subst (Map.Map ID.Ident T.Type)
    deriving (Eq, Ord, Show, Monoid.Monoid)






{-
    # Substitutable Utils
-}


-- |
-- The empty substitution
emptySubst :: Subst
emptySubst = Monoid.mempty


-- |
-- Compose substitutions

compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) =
    Subst
        $ Map.map (apply (Subst s1)) s2 `Map.union` s1

isEmpty :: Subst -> Bool
isEmpty (Subst s)
    | Map.empty == s = True
    | otherwise      = False


{-
    # Substitutable
-}


class TypeSubstitutable a where
    apply :: Subst -> a -> a
    fvs :: a -> Set.Set ID.Ident




{-
    ## Substitutable - Types
-}



instance TypeSubstitutable T.Type where
    apply = applyTypeSubst

    fvs = queryTypeVars


instance TypeSubstitutable T.Scheme where
  apply  = applySchemeSubst

  fvs = querySchemeVars


instance TypeSubstitutable a => TypeSubstitutable [a] where
    apply = map . apply
    fvs   = Fold.foldr (Set.union . fvs) Set.empty


instance TypeSubstitutable Con.Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    fvs (t1, t2) = fvs t1 `Set.union` fvs t2


instance TypeSubstitutable Env.Types where
    apply s = Map.map (apply s)
    fvs env = fvs $ Map.elems env


instance TypeSubstitutable Env.Env where
    apply s (ts, os) = (Map.map (apply s) ts, os)
    fvs (ts, os) = fvs $ Map.elems ts


-- | Internal Helpers
--

-- applyTypeSubst :: Subst -> T.Type -> T.Type
-- applyTypeSubst (Subst s) ty =
--     ty  |> Uni.transform f
--         |> Uni.transform cleanup
-- 
--     where
--         f (T.Superposed var@(T.Var x m) ts) = 
--             let name = x
--                 var' = Map.findWithDefault var name s
--             in
--                 case var' of
--                     T.Var{} ->
--                         T.Superposed var' ts
--                     _ ->
--                         error $ "How did this happen?\n" ++ PP.prettyShow var'
-- 
--         f (T.Superposed var ts) = 
--             var
-- 
--         f var@(T.Var x m) = 
--             let name =  x
--                 t'   = Map.findWithDefault var name s
--             in
--                 t'
--         f x = x
-- 
-- 
--         -- NOTE: Tmp hack...
--         -- * E.g. `<a : Bool/Bool>` = Bool
--         -- ** ^ (This should be resolved as a constraint)
--         -- ** I.e. emit `<Bool : Bool/Bool>` = Bool
--         --    maybe in `LLIR.HelmLL.Core.Module.TypeCheck.Data.System.Constraints`?
--         --    Then be simplified, in `LLIR.HelmLL.Core.Module.TypeCheck.Data.Unification`, or simmular?
--         -- * Although perhaps this is the best spot for such transformations...
-- 
--         cleanup :: T.Type -> T.Type
--         cleanup node@(T.Superposed _ (x:xs)) =
--             if List.all (== x) xs then
--                 x
--             else
--                 node
-- 
--         cleanup x = x




applyTypeSubst :: Subst -> T.Type -> T.Type
applyTypeSubst (Subst s) ty =
    ty  |> Uni.transform f

    where
        f var@(T.Var x) = 
            let name =  x
                t'   = Map.findWithDefault var name s
            in
                t'
        
        f x = x






queryTypeVars :: T.Type -> Set.Set ID.Ident
queryTypeVars ty =
    Set.fromList
        [x | (T.Var x) <- Uni.universe ty ]


applySchemeSubst :: Subst -> T.Scheme -> T.Scheme
applySchemeSubst (Subst s) (T.Forall as t) =
    T.Forall as $ apply s' t
    where
        s' = Subst $ Fold.foldr Map.delete s as



querySchemeVars :: T.Scheme -> Set.Set ID.Ident
querySchemeVars (T.Forall as t) =
    fvs t `Set.difference` Set.fromList as
