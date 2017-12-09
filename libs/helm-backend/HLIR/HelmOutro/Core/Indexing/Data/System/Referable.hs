{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Indexing.Data.System.Referable where


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
import qualified HLIR.HelmOutro.Core.Indexing.Data.Subst  as Sub
import qualified HLIR.HelmOutro.Core.Indexing.Data.System as Sys
-- *



-- indexRefs :: E.Expr -> Subst -> E.Expr
-- indexRefs expr subs =
--     Uni.transformBi f expr
--     where
--         f :: ID.Ref -> ID.Ref
--         f ref@(ID.Ref ident _) =
--             case Map.lookup ident subs of
--                 Nothing -> ref
--                 Just i' -> ID.Ref i' Nothing



referable :: ID.Ref -> Sys.State (ID.Ref, Sub.Subst)
referable (ID.Ref txt ns) = do
    (_, s) <- M.ask

    case Sub.lookup txt s of
        Nothing   -> return (ID.Ref txt  ns, Sub.empty)
        Just txt' -> return (ID.Ref txt' ns, Sub.empty)



