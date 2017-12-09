{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Indexing.Syntax.TermLevel.Patterns (
    indexCaseAlt
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
import HLIR.HelmOutro.Core.Indexing.Auxiliary.Utils (indexList, indexMaybe)

-- ~ Sub Indexers
import qualified HLIR.HelmOutro.Core.Indexing.Syntax.Base.Ident as ID
-- *




indexCaseAlt :: (E.Expr -> Sys.Index E.Expr) -> P.CaseAlt -> Sys.Index P.CaseAlt
indexCaseAlt f (P.CaseAlt patrn expr) = do
        (patrn', ss) <- indexPattern patrn
        (expr', _) <- Scope.withLocalSubst ss (f expr)

        enter (P.CaseAlt patrn' expr') Sub.empty



indexPattern :: P.Pattern -> Sys.Index P.Pattern

indexPattern (P.Lit lit) = enter (P.Lit lit) Sub.empty
indexPattern (P.Record vars) = error "TODO - Not yet supported: Indexing record patterns…"

indexPattern (P.List xs) = do
    (xs', ss) <- indexList indexPattern xs
    
    enter (P.List xs') ss

indexPattern (P.Cons xs rest) = do
    (xs', s1) <- indexList indexPattern xs
    (rest', s2) <- indexMaybe indexPattern rest
    
    enter (P.Cons xs' rest') (Sub.merge s1 s2)

indexPattern (P.Tuple items) = do
    (items', subs) <- indexList indexPattern items
    
    enter (P.Tuple items') subs

indexPattern (P.Con name args) = do
    (args', subs) <- indexList indexPattern args

    enter (P.Con name args') subs

indexPattern (P.Var var) = do
    (var', subst) <- ID.indexBinder var

    enter (P.Var var') subst

indexPattern P.Wildcard =
    enter P.Wildcard Sub.empty





