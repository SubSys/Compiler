{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.Indexing.Driver (
      stdIndexer
    , stdIndexer'
    , globalize
    , localize
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
-- ~ HelmCore Drivers
import qualified HLIR.HelmCore.Core.TypeCheck.Driver as TC

-- ~ HelmCore IR
import qualified HLIR.HelmCore.Data.Payload as Payload

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

--- Local
import qualified HLIR.HelmCore.Core.Indexing.Data.Subst            as Sub
import qualified HLIR.HelmCore.Core.Indexing.Data.System           as Sys
import qualified HLIR.HelmCore.Core.Indexing.Data.System.Bindable  as Bind
import qualified HLIR.HelmCore.Core.Indexing.Data.System.Referable as Ref
import qualified HLIR.HelmCore.Core.Indexing.Syntax                as Syntax
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
                |> TC.typeCheck'
    in
        case result of
            Left err -> Left $ tcErrorPrefix `Text.append` err
            Right payload' -> Right payload'
    
    
    where

        tcErrorPrefix = Text.pack
            "Info: For context `stdIndexer’` just finished indexing, and ran the type checker on the processed (indexed) functions, which returned with the following error (NOTE: My guess would be an error with the indexer…):\n"






-- *
-- | Indexers
-- *

globalize :: [Decl.Function] -> [Decl.Function]
globalize fns =
    fst exec
    where
        exec = Sys.runState
                    Sys.Globalizing
                    (Syntax.indexer fns)
                    0
                    Sub.empty


localize :: [Decl.Function] -> [Decl.Function]
localize fns =
    fst exec
    where
        exec = Sys.runState
                    Sys.Localizing
                    (Syntax.indexer fns)
                    0
                    Sub.empty






