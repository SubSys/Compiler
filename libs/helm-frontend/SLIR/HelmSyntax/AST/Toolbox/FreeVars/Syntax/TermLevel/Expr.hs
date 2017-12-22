{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.AST.Toolbox.FreeVars.Syntax.TermLevel.Expr (
    queryExpr
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

-- ~~ Auxiliary AST - Nodes & Utils
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident            as CID
import qualified SLIR.HelmSyntax.AST.Toolbox.SudoFFI                      as SudoFFI
import qualified SLIR.HelmSyntax.AST.Toolbox.TopLevel.Functions.Recursive as Rec


--- Local
import qualified SLIR.HelmSyntax.AST.Toolbox.FreeVars.Data.System          as Sys
import qualified SLIR.HelmSyntax.AST.Toolbox.FreeVars.Internal.Utils.Scope as Scope

-- ~ Sub Queries
import qualified SLIR.HelmSyntax.AST.Toolbox.FreeVars.Syntax.TermLevel.Patterns as P
-- *





queryExpr :: (Decl.Function -> Sys.Bindable Decl.Function) -> E.Expr -> Sys.Query E.Expr

queryExpr f (E.Var name meta) = do
    Scope.emitRef name
    
    return (E.Var name meta)

queryExpr f (E.Lit val meta) =
    return (E.Lit val meta)


queryExpr f (E.Tuple items meta) = do
    items' <- M.mapM (queryExpr f) items
    -- *
    
    -- *
    return (E.Tuple items' meta)

queryExpr f (E.List xs meta) = do
    xs' <- M.mapM (queryExpr f) xs
    -- *
    
    -- *
    return (E.List xs' meta)

queryExpr f (E.Con id' meta) =
    return (E.Con id' meta)



queryExpr f (E.BinOp sym e1 e2 meta) = do
    Scope.emitRef sym
    -- *
    
    -- *
    e1' <- queryExpr f e1
    e2' <- queryExpr f e2
    -- *
    
    -- *
    return (E.BinOp sym e1' e2' meta)

queryExpr f (E.If intros elseExpr meta) = do
    intros' <- M.mapM (applyIfBranch f) intros
    elseExpr' <- queryExpr f elseExpr
    -- *
    
    -- *
    return (E.If intros' elseExpr' meta)

queryExpr f (E.Let fns expr meta) = do
    -- (fns', binders) <- List.unzip <$> M.mapM f fns
    (fns', binders) <- applyLetBinders f fns
    -- *
    
    -- *
    expr' <- Scope.without binders (queryExpr f expr)
    -- *
    
    -- *
    return (E.Let fns' expr' meta)
    

queryExpr f (E.Case con alts meta) = do
    con' <- queryExpr f con
    alts' <- M.mapM (P.queryCaseAlt (queryExpr f)) alts
    -- *
    
    -- *
    return (E.Case con' alts' meta)

queryExpr f (E.Parens expr meta) = do
    expr' <- queryExpr f expr
    -- *
    
    -- *
    return (E.Parens expr' meta)

queryExpr f (E.App e1 e2 meta) = do
    e1' <- queryExpr f e1
    e2' <- queryExpr f e2
    -- *
    
    -- *
    return (E.App e1' e2' meta)

queryExpr f (E.Abs arg expr meta) = do
    expr' <- Scope.without [CID.ident arg] (queryExpr f expr)
    -- *
    
    -- *
    return (E.Abs arg expr' meta)



queryExpr f (E.AltAbs args expr scheme) = do
    expr' <- Scope.without (CID.idents args) (queryExpr f expr)
    -- *
    
    -- *
    return (E.AltAbs args expr' scheme)


-- queryExpr f (E.RecordUpdate var fields meta) =
--     return (E.RecordUpdate var fields meta)
-- 
-- queryExpr f (E.RecordAccess field object meta) =
--     return (E.RecordAccess field object meta)
-- 
-- queryExpr f (E.Record fields meta) =
--     return (E.Record fields meta)



-- *
-- | Internal Helpers
-- *


applyIfBranch :: (Decl.Function -> Sys.Bindable Decl.Function)
              -> (E.Expr, E.Expr)
              -> Sys.Query (E.Expr, E.Expr)
applyIfBranch f (con, body) = do
    con' <- queryExpr f con
    body' <- queryExpr f body
    
    return (con', body')



applyLetBinders :: (Decl.Function -> Sys.Bindable Decl.Function)
                -> [Decl.Function]
                -> Sys.Query ([Decl.Function], [CID.Ident])
applyLetBinders f [] = return ([], [])

applyLetBinders f (decl:rest) = do
    (decl', binder) <- f decl
    
    (rest', binders) <- Scope.without binder (applyLetBinders f rest)
    
    return (decl' : rest', binder ++ binders)


