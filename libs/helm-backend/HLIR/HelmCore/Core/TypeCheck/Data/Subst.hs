{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmCore.Core.TypeCheck.Data.Subst (
      Subst(..)
    , Substitutable(..)
    , compose
    , emptySubst
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
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Env                    as Env
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Canonical.Ident        as CID
-- *




newtype Subst = Subst (Map.Map CID.Ident T.Type)
    deriving (Eq, Ord, Show, Monoid.Monoid)


class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set.Set CID.Ident

instance Substitutable T.Type where
    apply = applyTypeSubst

    ftv = queryTypeVars


instance Substitutable T.Scheme where
  apply  = applySchemeSubst

  ftv = querySchemeVars


instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv   = Fold.foldr (Set.union . ftv) Set.empty


instance Substitutable Con.Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)
    ftv (t1, t2) = ftv t1 `Set.union` ftv t2


instance Substitutable Env.Env where
    apply s (Env.Env env) = Env.Env $ Map.map (apply s) env
    ftv (Env.Env env) = ftv $ Map.elems env



-- *
-- | Subst Utils
-- *



-- | Compose substitutions
--
compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) =

    Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1


-- *
-- | Misc. Utils
-- *

-- | The empty substitution
emptySubst :: Subst
emptySubst = Monoid.mempty






-- *
-- | Internal Helpers
-- *


applyTypeSubst :: Subst -> T.Type -> T.Type
applyTypeSubst (Subst s) =
    Uni.transform f
    where
        f var@(T.Var x) = 
            let name = CID.ident x
                t'   = Map.findWithDefault var name s
            in
                t'
        f x = x

queryTypeVars :: T.Type -> Set.Set CID.Ident
queryTypeVars ty =
    Set.fromList
        $ CID.idents [x | (T.Var x) <- Uni.universe ty ]


applySchemeSubst :: Subst -> T.Scheme -> T.Scheme
applySchemeSubst (Subst s) (T.Forall as t) =
    T.Forall as $ apply s' t
    where
        s' = Subst $ Fold.foldr Map.delete s $ CID.idents as


querySchemeVars :: T.Scheme -> Set.Set CID.Ident
querySchemeVars (T.Forall as t) =
    let as' = CID.idents as
    in
        ftv t `Set.difference` Set.fromList as'


