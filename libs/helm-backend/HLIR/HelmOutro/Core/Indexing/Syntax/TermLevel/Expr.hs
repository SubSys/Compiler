{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Indexing.Syntax.TermLevel.Expr (
    indexExpr
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


import qualified HLIR.HelmOutro.Render.Utils as Display


--- Local Deps
-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl


--- Local
import qualified HLIR.HelmOutro.Core.Indexing.Data.Subst            as Sub
import qualified HLIR.HelmOutro.Core.Indexing.Data.System           as Sys
import qualified HLIR.HelmOutro.Core.Indexing.Data.System.Bindable  as Bind
import qualified HLIR.HelmOutro.Core.Indexing.Data.System.Referable as Ref
import qualified HLIR.HelmOutro.Core.Indexing.Data.System.Scope     as Scope

-- ~ Indexable Stuff
import HLIR.HelmOutro.Core.Indexing.Data.System (Index, Indexable(..), enter)

-- ~ Misc. Utils
import HLIR.HelmOutro.Core.Indexing.Auxiliary.Utils (indexList, indexMaybe, indexArgs)

-- ~ Sub Indexers
import qualified HLIR.HelmOutro.Core.Indexing.Syntax.Base.Ident         as ID
import qualified HLIR.HelmOutro.Core.Indexing.Syntax.TermLevel.Patterns as P
-- *



indexExpr :: (Decl.Function -> Sys.Index Decl.Function)
          -> E.Expr
          -> Sys.Index E.Expr


indexExpr f (E.Var ref) = do
        (ref', _) <- ID.indexRef ref

        enter (E.Var ref') Sub.empty

indexExpr f (E.Lit val) =
        enter (E.Lit val) Sub.empty

indexExpr f (E.Record fields) =
        error "TODO - Not yet supported: Indexing record expr fields…"

indexExpr f (E.Tuple items) = do
        (items', _) <- List.unzip <$> M.mapM (indexExpr f) items

        enter (E.Tuple items') Sub.empty

indexExpr f (E.List xs) = do
        (xs', _) <- List.unzip <$> M.mapM (indexExpr f) xs

        enter (E.List xs') Sub.empty

indexExpr f (E.Con id') =
        enter (E.Con id') Sub.empty

indexExpr f (E.Let fns expr) = do
        (fns', ss) <- indexList f fns
        (expr', _) <- Scope.withLocalSubst ss (indexExpr f expr)

        enter (E.Let fns' expr') Sub.empty

indexExpr f (E.Case con alts) = do
        (con', _) <- indexExpr f con
        (alts', _) <- List.unzip <$> M.mapM (P.indexCaseAlt (indexExpr f)) alts
        
        enter (E.Case con' alts') Sub.empty


indexExpr f (E.App e1 e2) = do
        (e1', _) <- indexExpr f e1
        (e2', _) <- indexExpr f e2
        
        enter (E.App e1' e2') Sub.empty

indexExpr f (E.Abs args expr) = do
        (args', ss) <- indexArgs args
        (expr', _) <- Scope.withLocalSubst ss (indexExpr f expr)
        
        enter (E.Abs args' expr') Sub.empty



indexExpr f (E.FunCall ref params) = do
    (ref', _) <- ID.indexRef ref
    (params', _) <- List.unzip <$> M.mapM (indexExpr f) params
    
    enter (E.FunCall ref' params') Sub.empty


indexExpr f (E.ConCall con params) = do
    (params', _) <- List.unzip <$> M.mapM (indexExpr f) params
    
    enter (E.ConCall con params') Sub.empty


