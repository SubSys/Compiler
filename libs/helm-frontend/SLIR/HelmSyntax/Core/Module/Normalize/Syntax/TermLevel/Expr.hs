{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Normalize.Syntax.TermLevel.Expr (
    normExpr
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
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Utils.Namespace           as NS
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Data.System               as Sys

import qualified SLIR.HelmSyntax.Core.Module.Normalize.Syntax.Base.Types         as T
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Syntax.Base.Ident         as ID
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Syntax.TermLevel.Patterns as P
-- *





normExpr :: (Decl.Function -> Sys.Env Decl.Function)
         -> E.Expr
         -> Sys.Env E.Expr


normExpr f (E.Var name meta) = do
    name' <- ID.normLow name
    
    return
        $ E.Var name' meta

normExpr f (E.Lit val meta) =
    return $ E.Lit val meta

normExpr f (E.Tuple items meta) = do
    items' <- M.mapM (normExpr f) items
    
    return
        $ E.Tuple items' meta

normExpr f (E.List xs meta) = do
    xs' <- M.mapM (normExpr f) xs
    
    return
        $ E.List xs' meta

normExpr f (E.Con name meta) = do
    name' <- ID.normBig name
    
    return
        $ E.Con name' meta

normExpr f (E.BinOp sym e1 e2 meta) = do
    sym' <- ID.normSym sym
    e1' <- normExpr f e1
    e2' <- normExpr f e2
    
    return
        $ E.BinOp sym' e1' e2' meta


normExpr f (E.If intros elseExpr meta) = do
    intros' <- M.mapM (normIfBranch f) intros
    elseExpr' <- normExpr f elseExpr
    
    return
        $ E.If intros' elseExpr' meta


normExpr f (E.Let fns expr meta) = do
    fns' <- M.mapM f fns
    expr' <- normExpr f expr
    
    return
        $ E.Let fns' expr' meta


normExpr f (E.Case con alts meta) = do
    con' <- normExpr f con
    alts' <- M.mapM (P.normCaseAlt (normExpr f)) alts
    
    return
        $ E.Case con' alts' meta



normExpr f (E.Parens expr meta) = do
    expr' <- normExpr f expr
    
    return
        $ E.Parens expr' meta

normExpr f (E.App e1 e2 meta) = do
    e1' <- normExpr f e1
    e2' <- normExpr f e2
    
    return
        $ E.App e1' e2' meta

normExpr f (E.Abs arg expr meta) = do
    expr' <- normExpr f expr
    
    return
        $ E.Abs arg expr' meta


-- TODO: Records
--
-- normExpr f ns (E.Record fields meta) =
--     E.Record fields meta

-- normExpr f ns (E.RecordUpdate var fields meta) =
--     E.RecordUpdate var fields meta
-- 
-- normExpr f ns (E.RecordAccess field object meta) =
--     E.RecordAccess field object meta




-- *
-- | Internal Helpers
-- *

normIfBranch :: (Decl.Function -> Sys.Env Decl.Function)
             -> (E.Expr, E.Expr)
             -> Sys.Env (E.Expr, E.Expr)
normIfBranch f (con, body) = do
    con' <- normExpr f con
    body' <- normExpr f body
    
    return (con', body')


