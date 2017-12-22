{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Type - Substitutions
--
module SLIR.HelmSyntax.Core.Program.SDD.Subst.Types (
      TySub(..)
    , TypeSubstitutable(..)
    , compose
    , emptySubst
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Auxiliary Nodes
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID

--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env     as Env
-- *



-- TODO: change `TySub` to simply `Subst`, then diff by namespace (I.e. `TySub.Subst`)



newtype TySub = TySub (Map.Map CID.Ident T.Type)
    deriving (Eq, Ord, Show, Monoid.Monoid)





-- *
-- | Substitutable Utils
-- *



-- |
-- The empty substitution
emptySubst :: TySub
emptySubst = Monoid.mempty


-- |
-- Compose substitutions
compose :: TySub -> TySub -> TySub
compose (TySub s1) (TySub s2) =
    TySub
        $ Map.map (apply (TySub s1)) s2 `Map.union` s1





-- *
-- | Substitutable Classes
-- *
class TypeSubstitutable a where
    apply :: TySub -> a -> a
    fvs :: a -> Set.Set CID.Ident












-- *
-- | Substitutable - Types
-- *






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
    apply s (Con.Inline name ty expr) =
        Con.Inline name (apply s ty) expr
    
    apply s (Con.Overloaded ident ty xs) =
        let
            ty' = apply s ty
            xs' = Map.mapKeys (apply s) xs
        in
            Con.Overloaded ident ty' xs'

    
    apply s (Con.TypeCon t1 t2) =
        Con.TypeCon (apply s t1) (apply s t2)
    
    fvs (Con.TypeCon t1 t2) =
        fvs t1 `Set.union` fvs t2
    fvs x = Set.empty



instance TypeSubstitutable Env.ExprEnv where
    apply s = Map.map (apply s)
    fvs env = fvs $ Map.elems env


instance TypeSubstitutable Env.Entry where
    apply s (Env.Normal scheme) =
        Env.Normal (apply s scheme)
    
    apply s (Env.Superposed ty expr) =
        Env.Superposed (apply s ty) expr
    
    
    apply s (Env.Overloaded xs) =
        Env.Overloaded $ Map.mapKeys (apply s) xs
    
    
    fvs (Env.Normal scheme) =
        fvs scheme
    fvs (Env.Superposed ty expr) =
        fvs ty
    
    -- fvs (Env.Overloaded xs) =
    --     fvs (Map.keys xs)

    -- TODO: ...? maybe use empty?
    -- fvs (Env.Overloaded xs) =










applyTypeSubst :: TySub -> T.Type -> T.Type
applyTypeSubst (TySub s) ty =
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


applySchemeSubst :: TySub -> T.Scheme -> T.Scheme
applySchemeSubst (TySub s) (T.Forall as t) =
    T.Forall as $ apply s' t
    where
        s' = TySub $ Fold.foldr Map.delete s $ CID.idents as



querySchemeVars :: T.Scheme -> Set.Set CID.Ident
querySchemeVars (T.Forall as t) =
    let as' = CID.idents as
    in
        fvs t `Set.difference` Set.fromList as'


