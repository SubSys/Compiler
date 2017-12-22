{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.Index.Data.System.Bindable where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)

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


--- Local Deps
-- ~ HelmIntro Interfaces
import qualified HLIR.HelmIntro.Data.Payload as Payload

-- ~ HelmIntro AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

-- ~~ Auxiliary Nodes
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident as CID


--- Local
import qualified HLIR.HelmIntro.Core.Index.Data.Subst  as Sub
import qualified HLIR.HelmIntro.Core.Index.Data.System as Sys
-- *





bindable :: ID.Binder -> Sys.State (ID.Binder, Sub.Subst)
bindable binder = do
    mode <- Sys.getIndexingMode
    
    case mode of
        Sys.Globalizing -> global Sys.Globalizing
        Sys.Localizing -> local Sys.Localizing
    
    
    where
        global mode = do
            idx <- Sys.incCounter
            -- *

            -- *
            let (binder', subs) = newSubst mode binder idx
            -- *

            -- *
            return (binder', subs)
        
        local mode = do
            idx <- Sys.decCounter
            -- *

            -- *
            let (binder', subs) = newSubst mode binder idx
            -- *

            -- *
            return (binder', subs)



-- *
-- |  Internal
-- *


localPrefix :: Text
localPrefix =
    Text.pack "ª"

globalPrefix :: Text
globalPrefix = Text.pack "º"


newSubst :: Sys.IndexingMode -> ID.Binder -> Int -> (ID.Binder, Sub.Subst)
newSubst mode old idx =
    let
        -- Finish
        newBinder = freshIdent mode idx
        subs'     = Map.singleton (CID.ident old) newBinder

    in
        (CID.toBinder newBinder, subs')



freshIdent :: Sys.IndexingMode -> Int -> CID.Ident
freshIdent Sys.Globalizing i =
    let
        idx = Text.pack $ show i
    in
        CID.Ident' $ globalPrefix `Text.append` idx

freshIdent Sys.Localizing i =
    let
        idx = Text.pack $ show i
    in
        CID.Ident' $ localPrefix `Text.append` idx



