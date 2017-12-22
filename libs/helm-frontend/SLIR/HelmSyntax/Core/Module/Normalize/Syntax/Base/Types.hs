{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Normalize.Syntax.Base.Types (
    normType
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

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
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

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
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Utils.Namespace   as NS
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Data.System       as Sys
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Syntax.Base.Ident as ID
-- *



normType :: T.Type -> Sys.Env T.Type

normType (T.Tuple ts meta) = do
    ts' <- M.mapM normType ts

    return
        $ T.Tuple ts' meta

normType (T.List ty meta) = do
    ty' <- normType ty

    return
        $ T.List ty' meta

normType (T.Union name args meta) = do
    name' <- ID.normBig name
    args' <- M.mapM normType args
    
    return
        $ T.Union name' args' meta

normType (T.Var id' meta) =
    return
        $ T.Var id' meta

normType (T.Arr ty1 ty2 meta) = do
    ty1' <- normType ty1
    ty2' <- normType ty2
    
    return
        $ T.Arr ty1' ty2' meta

normType (T.Parens ty meta) = do
    ty' <- normType ty
    
    return
        $ T.Parens ty' meta



normType (T.String meta) =
    return
        $ T.String meta

normType (T.Char meta) =
    return
        $ T.Char meta

normType (T.Int meta) =
    return
        $ T.Int meta

normType (T.Float meta) =
    return
        $ T.Float meta

normType (T.Bool meta) =
    return
        $ T.Bool meta

normType (T.Superposed x ts) = do
    x' <- normType x
    ts' <- M.mapM normType ts
    
    return
        $ T.Superposed x' ts'


-- TODO:...
-- normType (T.Record fields meta) =
--     return
--         $ T.Record fields meta






