{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Indexing.Data.System.Bindable where


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




localPrefix :: Text
localPrefix =
    Text.pack "ª"

localLabel :: Int -> Text
localLabel i =
    let idx = Text.pack $ show i
    in
        localPrefix `Text.append` idx


globalPrefix :: Text
globalPrefix = Text.pack "º"

globalLabel :: Int -> Text
globalLabel i =
    let idx = Text.pack $ show i
    in
        globalPrefix `Text.append` idx


newGlobalSubst :: Text -> Int -> (ID.Binder, Sub.Subst)
newGlobalSubst old idx =
    let
        -- Init Data
        newLabel = globalLabel idx
        
        -- Finish
        newBinder = ID.Binder newLabel Nothing
        subs'     = Sub.singleton old newLabel

    in
        (newBinder, subs')


newLocalSubst :: Text -> Int -> (ID.Binder, Sub.Subst)
newLocalSubst old idx =
    let
        -- Init Data
        newLabel = localLabel idx
        
        -- Finish
        newBinder = ID.Binder newLabel Nothing
        subs'     = Sub.singleton old newLabel

    in
        (newBinder, subs')



bindable :: ID.Binder -> Sys.State (ID.Binder, Sub.Subst)
bindable (ID.Binder txt ns) = do
    mode <- Sys.getIndexingMode
    
    case mode of
        Sys.Globalizing -> global
        Sys.Localizing -> local
    
    
    where
        global = do
            idx <- Sys.incCounter
            -- *

            -- *
            let (binder', subs) = newGlobalSubst txt idx
            -- *

            -- *
            return (binder', subs)
        
        local = do
            idx <- Sys.decCounter
            -- *

            -- *
            let (binder', subs) = newLocalSubst txt idx
            -- *

            -- *
            return (binder', subs)
            




