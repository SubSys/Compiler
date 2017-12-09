{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Indexing.Driver (
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


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP

import qualified CGIR.Rust.Render.Syntax as Display

-- ~ Upstream
import qualified SLIR.HelmSyntax.Core  as HelmSyntax
import qualified HLIR.HelmCore.Core    as HelmCore
import qualified HLIR.HelmOutro.Core   as HelmOutro

import qualified LLIR.LightRoast.Core as LightRoast



--- Local Deps
-- ~ LightRoast Payload
import qualified CGIR.Rust.Data.Payload as Payload

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
import qualified CGIR.Rust.Core.Indexing.Data.Subst            as Sub
import qualified CGIR.Rust.Core.Indexing.Data.System           as Sys
import qualified CGIR.Rust.Core.Indexing.Data.System.Bindable  as Bind
import qualified CGIR.Rust.Core.Indexing.Data.System.Referable as Ref
import qualified CGIR.Rust.Core.Indexing.Syntax                as Syntax

-- ~ Indexers
import CGIR.Rust.Core.Indexing.Driver.Localize (localize)
import CGIR.Rust.Core.Indexing.Driver.Globalize (globalize)
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




