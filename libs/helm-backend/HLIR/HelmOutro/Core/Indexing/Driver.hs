{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Indexing.Driver (
      stdIndexer
    , stdIndexer'
    , globalize
    , localize
    , globalIndexer
    , globalIndexer'
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


--- Local Deps
-- ~ HelmOutro Drivers
import qualified HLIR.HelmOutro.Core.TypeCheck.Driver as TC

-- ~ HelmOutro IR
import qualified HLIR.HelmOutro.Data.Payload as Payload

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
import qualified HLIR.HelmOutro.Core.Indexing.Syntax                as Syntax

-- ~ Indexers
import HLIR.HelmOutro.Core.Indexing.Driver.Localize (localize)
import HLIR.HelmOutro.Core.Indexing.Driver.Globalize (globalize)
-- *


stdIndexer :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
stdIndexer input = do
    result <- input
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ stdIndexer' payload




stdIndexer' :: Payload.Module -> Either Text Payload.Module
stdIndexer' payload =
    let fns =
            Payload.getFunctions payload
                |> globalize
                |> localize
        result =
            Payload.updateFunctions payload fns
                -- |> TC.typeCheck'
    in
        -- case result of
        --     Left err -> Left $ tcErrorPrefix `Text.append` err
        --     Right payload' -> Right payload'
        Right $ Payload.updateFunctions payload fns
    
    -- 
    -- where
    -- 
    --     tcErrorPrefix = Text.pack
    --         "Info: For context `stdIndexer’` just finished indexing, and ran the type checker on the processed (indexed) functions, which returned with the following error (NOTE: My guess would be an error with the indexer…):\n"



globalIndexer :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
globalIndexer upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ globalIndexer' payload


globalIndexer' :: Payload.Module -> Either Text Payload.Module
globalIndexer' payload =
    let fns = Payload.getFunctions payload
    in
        Right $ Payload.updateFunctions payload $ globalize fns




