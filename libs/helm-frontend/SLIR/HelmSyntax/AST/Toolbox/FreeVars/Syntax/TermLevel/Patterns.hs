{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.AST.Toolbox.FreeVars.Syntax.TermLevel.Patterns (
    queryCaseAlt
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


--- Local Prelude
import SLIR.HelmSyntax.AST.Toolbox.FreeVars.Internal.Utils.SyntaxHelpers (binder)

--- Local
import qualified SLIR.HelmSyntax.AST.Toolbox.FreeVars.Data.System          as Sys
import qualified SLIR.HelmSyntax.AST.Toolbox.FreeVars.Internal.Utils.Scope as Scope
-- *




queryCaseAlt :: (E.Expr -> Sys.Query E.Expr) -> P.CaseAlt -> Sys.Query P.CaseAlt
queryCaseAlt queryExpr (P.CaseAlt patrn expr meta) = do
    (patrn', binders) <- queryPattern patrn
    expr'  <- Scope.without binders (queryExpr expr)
    
    return (P.CaseAlt patrn' expr' meta)


queryPattern :: P.Pattern -> Sys.Bindable P.Pattern

queryPattern (P.Lit lit meta) =
    binder (P.Lit lit meta) []



queryPattern (P.List xs meta) = do
    (xs', binders) <- List.unzip <$> M.mapM queryPattern xs
    
    binder (P.List xs' meta) (flatten binders)

queryPattern (P.Cons xs Nothing meta) = do
    (xs', binders) <- List.unzip <$> M.mapM queryPattern xs
    
    binder (P.Cons xs' Nothing meta) (flatten binders)

queryPattern (P.Cons xs (Just rest) meta) = do
    (xs', binders1) <- List.unzip <$> M.mapM queryPattern xs
    
    (rest', binders2) <- queryPattern rest
    
    
    binder (P.Cons xs' (Just rest') meta) (flatten binders1 ++ binders2)


queryPattern (P.Tuple items meta) = do
    (items', binders) <- List.unzip <$> M.mapM queryPattern items
    
    binder (P.Tuple items' meta) (flatten binders)

queryPattern (P.Con name args meta) = do
    (args', binders) <- List.unzip <$> M.mapM queryPattern args
    
    binder (P.Con name args' meta) (flatten binders)

queryPattern (P.Var ident meta) =
    binder (P.Var ident meta) [CID.ident ident]

queryPattern (P.Wildcard meta) =
    binder (P.Wildcard meta) []



-- TODO: ...
-- queryPattern (P.Record vars meta) =
--     binder (P.Record vars meta) []




