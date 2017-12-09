{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Desugar.Driver (
      desugar
    , desugar'
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show)

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


-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified SLIR.HelmSyntax.Pass.Desugar as DesugarPass
-- *



desugar :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
desugar input = do
        input' <- input
        
        case input' of
            Left err -> return $ Left err
            Right payload ->
                return $ desugar' payload


desugar' :: Payload.Module -> Either Text Payload.Module
desugar' payload =
    let functions = Payload.getFunctions payload
            |> passes

    in
        Right $ update functions
    
    where
        update = Payload.updateFunctions payload


-- *
-- | Internal
-- *

passes :: [Decl.Function] -> [Decl.Function]
passes fns =
    fns |> DesugarPass.desugarIfTerms
        |> DesugarPass.desugarFnArgs
        |> DesugarPass.desugarBinOps



