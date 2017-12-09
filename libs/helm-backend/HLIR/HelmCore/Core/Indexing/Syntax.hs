{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmCore.Core.Indexing.Syntax (
    indexer
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

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



--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP

import qualified HLIR.HelmCore.Render.Utils as Display


-- ~ Upstream
import qualified SLIR.HelmSyntax.Core as HelmSyntax

-- ~ HelmSyntax IR
import qualified HLIR.HelmCore.Data.Payload as Payload

--- Local Deps
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

-- ~ HelmCore Drivers
import qualified HLIR.HelmCore.Core.TypeCheck.Driver as Driver


--- Local
import qualified HLIR.HelmCore.Core.Indexing.Data.Subst            as Sub
import qualified HLIR.HelmCore.Core.Indexing.Data.System           as Sys
import qualified HLIR.HelmCore.Core.Indexing.Data.System.Bindable  as Bind
import qualified HLIR.HelmCore.Core.Indexing.Data.System.Referable as Ref
import qualified HLIR.HelmCore.Core.Indexing.Data.System.Scope     as Scope

-- ~ Indexable Stuff
import HLIR.HelmCore.Core.Indexing.Data.System (Index, Indexable(..), enter)
-- *



{-# ANN module "HLint: ignore" #-}



indexer :: [Decl.Function] -> Sys.Index [Decl.Function]
indexer fns = index fns


getBinderCount :: E.Expr -> Int
getBinderCount x =
    List.length [b | b@ID.Binder{} <- Uni.universeBi x]







-- *
-- | # Indexable Instances
-- *


instance Indexable Decl.Function where
    index (Decl.Function name expr scheme) = do
        mode <- Sys.getIndexingMode
        
        case mode of
            Sys.Globalizing -> global
            Sys.Localizing -> local
        
        where
            count = getBinderCount expr
            
            global = do
                (name', ss) <- index name
                (expr', _) <- Scope.withLocalSubst ss (index expr)
                
                enter (Decl.Function name' expr' scheme) ss

            local = do
                M.put (Sys.Counter count)
                
                (expr', _) <- index expr
                
                
                enter (Decl.Function name expr' scheme) Sub.empty




instance Indexable E.Expr where
    index (E.Var ref) = do
        (ref', _) <- index ref
        
        enter (E.Var ref') Sub.empty

    index (E.Lit val) = do
        enter (E.Lit val) Sub.empty

    index (E.Record fields) =
        error "TODO - Not yet supported: Indexing record expr fields…"

    index (E.Tuple items) = do
        (items', _) <- index items
        
        enter (E.Tuple items') Sub.empty

    index (E.List xs) = do
        (xs', _) <- index xs
        
        enter (E.List xs') Sub.empty

    index (E.Con id') = do
        enter (E.Con id') Sub.empty

    index (E.Let fns expr) = do
        (fns', ss) <- index fns
        (expr', _) <- Scope.withLocalSubst ss (index expr)
        
        enter (E.Let fns' expr') Sub.empty

    index (E.Case con alts) = do
        (con', _) <- index con
        (alts', _) <- index alts
        
        enter (E.Case con' alts') Sub.empty

    index (E.App e1 e2) = do
        (e1', _) <- index e1
        (e2', _) <- index e2
        
        enter (E.App e1' e2') Sub.empty

    index (E.Abs arg expr) = do
        (arg', ss) <- index arg
        (expr', _) <- Scope.withLocalSubst ss (index expr)
        
        enter (E.Abs arg' expr') Sub.empty


-- *
-- | # Patterns
-- *
instance Indexable P.CaseAlt where
    index (P.CaseAlt patrn expr) = do
        (patrn', ss) <- index patrn
        (expr', _) <- Scope.withLocalSubst ss (index expr)
        
        enter (P.CaseAlt patrn' expr') Sub.empty



instance Indexable (Maybe P.Pattern) where
    index Nothing = enter Nothing Sub.empty
    index (Just p) = do
        (p', subs) <- index p
        enter (Just p') subs


instance Indexable P.Pattern where
    index (P.Lit lit) = enter (P.Lit lit) Sub.empty
    index (P.Record vars) = error "TODO - Not yet supported: Indexing record patterns…"

    index (P.List xs) = do
        (xs', ss) <- index xs
        
        enter (P.List xs') ss

    index (P.Cons xs rest) = do
        (xs', s1) <- index xs
        (rest', s2) <- index rest
        
        enter (P.Cons xs' rest') (Sub.merge s1 s2)

    index (P.Tuple items) = do
        (items', subs) <- index items
        
        enter (P.Tuple items') subs

    index (P.Con name args) = do
        (args', subs) <- index args
    
        enter (P.Con name args') subs

    index (P.Var ref) = do
        (ref', subst) <- index ref
    
        enter (P.Var ref') subst

    index P.Wildcard =
        enter P.Wildcard Sub.empty


instance Indexable ID.Binder where
    index = Bind.bindable


instance Indexable ID.Ref where
    index = Ref.referable




-- *
-- | Misc. - Aux. Instances
-- *


instance Indexable a => Indexable [a] where
    index []     = enter [] Sub.empty
    index (x:xs) = do
        (x', s1) <- index x
        (xs', s2) <- Scope.withLocalSubst s1 (index xs)
        
        enter (x' : xs') (Sub.merge s1 s2)


