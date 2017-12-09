{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Indexing.Syntax.TopLevel.Functions (
    indexFunction
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
import HLIR.HelmOutro.Core.Indexing.Auxiliary.Utils (indexList, indexMaybe, indexNameArgs, indexArgs)

-- ~ Sub Indexers
import qualified HLIR.HelmOutro.Core.Indexing.Syntax.Base.Etc       as Etc
import qualified HLIR.HelmOutro.Core.Indexing.Syntax.Base.Ident     as ID
import qualified HLIR.HelmOutro.Core.Indexing.Syntax.TermLevel.Expr as E
-- *






indexFunction (Decl.Function name args expr scheme) = do
    mode <- Sys.getIndexingMode
    
    case mode of
        Sys.Globalizing -> global
        Sys.Localizing -> local
    
    where
        count = List.length args + getBinderCount expr
        
        global = do
            (name', s1) <- ID.indexBinder name
            (args', s2) <- indexArgs args
            -- *
            
            -- *
            (expr', _) <- Scope.withLocalSubst
                            (Sub.merge s1 s2)
                            (E.indexExpr indexFunction expr)
            -- *
            
            -- *
            enter (Decl.Function name' args' expr' scheme) s1

        local = do
            M.put (Sys.Counter count)
            -- *
            
            -- *
            (args', ss) <- indexArgs args
            (expr', _) <- Scope.withLocalSubst ss (E.indexExpr indexFunction expr)
            -- *
            
            -- *
            enter (Decl.Function name args' expr' scheme) Sub.empty







-- *
-- | Internal Helpers
-- *

getBinderCount :: E.Expr -> Int
getBinderCount x =
    List.length [b | b@ID.Binder{} <- Uni.universeBi x]




