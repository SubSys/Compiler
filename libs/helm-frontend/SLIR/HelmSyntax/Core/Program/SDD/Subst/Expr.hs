{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
-- | Expression - Substitutions
--
module SLIR.HelmSyntax.Core.Program.SDD.Subst.Expr (
      Subst(..)
    , ExprSubstitutable(..)
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

-- ~~ AST Misc. Tools
import qualified SLIR.HelmSyntax.AST.Toolbox.FreeVars.Toolkit as FreeVars

--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Types            as TySub
-- *



newtype Subst = Subst (Map.Map CID.Ident E.Expr)
    deriving (Eq, Show, Monoid.Monoid)









-- *
-- | Substitutable Utils
-- *


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









-- *
-- | Substitutable Type Classes
-- *

class ExprSubstitutable a where
    apply :: Subst -> a -> a
    fvs :: a -> Set.Set CID.Ident







-- *
-- | Substitutable - Expr
-- *


instance ExprSubstitutable E.Expr where
    apply = applyExprSubst
    fvs expr  =  Set.fromList $ FreeVars.query expr


instance ExprSubstitutable Decl.Function where
    apply s (Decl.FnDecl name args expr sig meta) =
        Decl.FnDecl name args (apply s expr) sig meta
    
    apply s (Decl.OpDecl name args expr sig meta) =
        Decl.OpDecl name args (apply s expr) sig meta
    
    fvs decl  =  Set.fromList $ FreeVars.query decl



instance ExprSubstitutable Con.Constraint where
    apply s c@Con.TypeCon{} = c
    apply s c@Con.ExprCon{} = c
    
    apply s (Con.Overloaded name ty xs) =
        Con.Overloaded name ty xs
    
    apply s (Con.Inline  name ty expr) =
        Con.Inline name ty (apply s expr)


    fvs x = error "fvs ExprSubstitutable for Con.Constraint - Not Implemented"


instance ExprSubstitutable a => ExprSubstitutable [a] where
    apply = map . apply
    fvs   = Fold.foldr (Set.union . fvs) Set.empty


-- *
-- | Internal Helpers
-- *

applyExprSubst :: Subst -> E.Expr -> E.Expr
applyExprSubst subst expr =
    Uni.transform (applyExpr subst (FreeVars.query expr)) expr


applyExpr :: Subst -> [CID.Ident] -> E.Expr -> E.Expr
applyExpr (Subst subs) freeVars var@(E.Var name meta)
    | CID.ident name `List.elem` freeVars =
        Map.findWithDefault var (CID.ident name) subs


applyExpr (Subst subs) freeVars op@(E.BinOp sym e1 e2 meta)
    -- | CID.ident sym `List.elem` freeVars
    -- , Just node <- Map.lookup (CID.ident sym) subs =
    --     Map.findWithDefault op (CID.ident sym) subs
    
    | CID.ident sym `List.elem` freeVars
    , Just node <- Map.lookup (CID.ident sym) subs =
        E.App' (E.App' node e1) e2

applyExpr _ _ e = e



-- *
-- | Tmp. Fix.
-- *




