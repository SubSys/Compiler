{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.InitDeps.Driver (
      initDeps
) where


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

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload as Payload
import qualified SLIR.HelmSyntax.Data.Initialization as Init

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

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Base       as Base
import qualified SLIR.HelmSyntax.AST.Data.Header.ImportDecl as Decl

--- Local
import SLIR.HelmSyntax.Core.Module.InitDeps.Data.MiscAliases (ForeignModule, ForeignNamespaceOverride)

-- ~
import qualified SLIR.HelmSyntax.Core.Module.InitDeps.Data.Report          as Report
import qualified SLIR.HelmSyntax.Core.Module.InitDeps.Process.ImportDecls  as Process
-- *



initDeps :: [ForeignModule] -> IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
initDeps foreignPayloads upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            initDeps' foreignPayloads payload



initDeps' :: [ForeignModule] -> Payload.Module -> IO (Either Text Payload.Module)
initDeps' foreignPayloads payload =
    case Process.processImports foreignPayloads payload of
        Left err ->
            return
                $ Left
                $ Text.pack
                $ PP.prettyShow err 
        
        Right payload' ->
            return
                $ Right payload'




