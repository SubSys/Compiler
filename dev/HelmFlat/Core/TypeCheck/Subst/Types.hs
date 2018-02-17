{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmFlat.Core.TypeCheck.Subst.Types (
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

-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Module.Data.Interface as I

-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Scope           as Scope
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Ident as ID

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
-- ++ 
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Data.Env     as Env
import qualified HLIR.HelmFlat.Core.TypeCheck.Solver.Data.Constraint as Con
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
--         --    maybe in `HLIR.HelmFlat.Core.Module.TypeCheck.Data.System.Constraints`?
--         --    Then be simplified, in `HLIR.HelmFlat.Core.Module.TypeCheck.Data.Unification`, or simmular?
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
        |> Uni.transform cleanup

    where
        f var@(T.Var x m) = 
            let name =  x
                t'   = Map.findWithDefault var name s
            in
                t'
        
        
        -- f super@(T.Superposed [con] ts) =
        --     Map.findWithDefault super con s
        
        -- TODO: Maybe?
        -- f (T.Superposed [] ts) = error ...
            
        f (T.Superposed (con:_) ts) =
            let
                init   = T.Superposed [con] (apply (Subst s) ts)
                finish = Map.findWithDefault init con s
            in
                finish
        
        f x = x
        
        
        -- NOTE: Tmp hack...
        -- * E.g. `<a : Bool/Bool>` = Bool
        -- ** ^ (This should be resolved as a constraint)
        -- ** I.e. emit `<Bool : Bool/Bool>` = Bool
        --    maybe in `HLIR.HelmFlat.Core.Module.TypeCheck.Data.System.Constraints`?
        --    Then be simplified, in `HLIR.HelmFlat.Core.Module.TypeCheck.Data.Unification`, or simmular?
        -- * Although perhaps this is the best spot for such transformations...

        cleanup :: T.Type -> T.Type
        cleanup node@(T.Superposed _ (x:xs)) =
            if List.all (== x) xs then
                x
            else
                node
        
        cleanup x = x





queryTypeVars :: T.Type -> Set.Set ID.Ident
queryTypeVars ty =
    Set.fromList
        [x | (T.Var x _) <- Uni.universe ty ]


applySchemeSubst :: Subst -> T.Scheme -> T.Scheme
applySchemeSubst (Subst s) (T.Forall as t) =
    T.Forall as $ apply s' t
    where
        s' = Subst $ Fold.foldr Map.delete s as



querySchemeVars :: T.Scheme -> Set.Set ID.Ident
querySchemeVars (T.Forall as t) =
    fvs t `Set.difference` Set.fromList as
