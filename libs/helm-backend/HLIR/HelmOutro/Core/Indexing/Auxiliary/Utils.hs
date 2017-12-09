{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Indexing.Auxiliary.Utils (
      indexList
    , indexMaybe
    , indexNameArgs
    , indexArgs
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

-- ~ Sub Indexers
import qualified HLIR.HelmOutro.Core.Indexing.Syntax.Base.Ident as ID
import  qualified HLIR.HelmOutro.Core.Indexing.Syntax.Base.Etc  as Etc
-- *




-- | Index Binder Name & Args
--
indexNameArgs :: ID.Binder -> [Etc.Arg] -> Sys.Index (ID.Binder, [Etc.Arg])
indexNameArgs name args = do
    (name', subs1) <- ID.indexBinder name
    (args', subs2) <- indexArgs args

    enter (name', args') (Sub.merge subs1 subs2)


indexArgs :: [Etc.Arg] -> Sys.Index [Etc.Arg]
indexArgs args = do
    (args', subs) <- List.unzip <$> M.mapM Etc.indexArg args
    
    enter args' (Sub.mergeSubs subs)


indexList :: (a -> Sys.Index a) -> [a] -> Sys.State ([a], Sub.Subst)
indexList f []     = enter [] Sub.empty
indexList f (x:xs) = do
    (x', s1) <- f x
    (xs', s2) <- Scope.withLocalSubst s1 (indexList f xs)
    
    enter (x' : xs') (Sub.merge s1 s2)



indexMaybe :: (a -> Sys.Index a) -> Maybe a -> Sys.Index (Maybe a)
indexMaybe f Nothing = enter Nothing Sub.empty
indexMaybe f (Just x) = do
    (x', subs) <- f x
    enter (Just x') subs


