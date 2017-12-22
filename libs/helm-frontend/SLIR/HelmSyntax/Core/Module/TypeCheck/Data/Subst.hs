{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Subst (
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
import qualified Text.Show.Prettyprint as PP


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
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Unification.Constraint as Con
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Interface.TypesEnv     as TI
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident              as CID

-- ~ Subst
-- *




newtype Subst  = Subst (Map.Map CID.Ident T.Type)
    deriving (Eq, Ord, Show, Monoid.Monoid)



-- *
-- | Type Utils
-- *




-- *
-- | Substitutable
-- *

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


instance Substitutable TI.Env where
    apply s (TI.Env env) = TI.Env $ Map.map (apply s) env
    ftv (TI.Env env) = ftv $ Map.elems env



-- *
-- | Subst Utils
-- *



-- | Compose substitutions
--
compose :: Subst -> Subst -> Subst
compose (Subst s1) (Subst s2) =
    Subst
        $ Map.map (apply (Subst s1)) s2 `Map.union` s1


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
applyTypeSubst (Subst s) ty =
    ty  |> Uni.transform f
        |> Uni.transform cleanup

    where
        f (T.Superposed var@(T.Var x m) ts) = 
            let name = CID.ident x
                var' = Map.findWithDefault var name s
            in
                case var' of
                    T.Var{} ->
                        T.Superposed var' ts
                    _ ->
                        error $ "How did this happen?\n" ++ PP.prettyShow var'
        
        f (T.Superposed var ts) = 
            var

        f var@(T.Var x m) = 
            let name = CID.ident x
                t'   = Map.findWithDefault var name s
            in
                t'
        f x = x
        
        
        -- NOTE: Tmp hack...
        -- * E.g. `<a : Bool/Bool>` = Bool
        -- ** ^ (This should be resolved as a constraint)
        -- ** I.e. emit `<Bool : Bool/Bool>` = Bool
        --    maybe in `SLIR.HelmSyntax.Core.Module.TypeCheck.Data.System.Constraints`?
        --    Then be simplified, in `SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Unification`, or simmular?
        -- * Although perhaps this is the best spot for such transformations...

        cleanup :: T.Type -> T.Type
        cleanup node@(T.Superposed _ (x:xs)) =
            if List.all (== x) xs then
                x
            else
                node
        
        cleanup x = x
        

queryTypeVars :: T.Type -> Set.Set CID.Ident
queryTypeVars ty =
    Set.fromList
        $ CID.idents [x | (T.Var x _) <- Uni.universe ty ]


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







