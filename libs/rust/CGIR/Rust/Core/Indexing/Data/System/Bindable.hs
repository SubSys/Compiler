{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Indexing.Data.System.Bindable where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>), (>>=), (>>), (!!))

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
-- ~ (GCIR) - Rust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Types  as T
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified CGIR.Rust.AST.TermLevel.Stmt        as S
import qualified CGIR.Rust.AST.TermLevel.Patterns    as P
import qualified CGIR.Rust.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.Functions as Decl
import qualified CGIR.Rust.AST.TopLevel.Unions    as Decl

--- Local
import qualified CGIR.Rust.Core.Indexing.Data.Subst  as Sub
import qualified CGIR.Rust.Core.Indexing.Data.System as Sys
-- *



letters :: [Text]
letters =
    Text.pack <$> ([1..] >>= flip M.replicateM ['a'..'z'])


localPrefix :: Text
localPrefix =
    Text.pack "l"

localLabel :: Int -> Text
localLabel i =
    let idx = Text.pack $ show i
    in
        localPrefix `Text.append` idx


globalPrefix :: Text
globalPrefix = Text.pack "g"

globalLabel :: Int -> Text
globalLabel i =
    let idx = Text.pack $ show i
    in
        globalPrefix `Text.append` idx


newGlobalSubst :: Text -> Int -> (ID.Low, Sub.Subst)
newGlobalSubst old idx =
    let
        -- Init Data
        newLabel = globalLabel idx
        
        -- Finish
        newBinder = ID.Low newLabel Nothing
        subs'     = Sub.singleton old newLabel

    in
        (newBinder, subs')


newLocalSubst :: Text -> Int -> (ID.Low, Sub.Subst)
newLocalSubst old idx =
    let
        -- Init Data
        newLabel = localLabel idx
        
        -- Finish
        newBinder = ID.Low newLabel Nothing
        subs'     = Sub.singleton old newLabel

    in
        (newBinder, subs')



bindable :: ID.Low -> Sys.State (ID.Low, Sub.Subst)
bindable (ID.Low txt ns) = do
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
            




