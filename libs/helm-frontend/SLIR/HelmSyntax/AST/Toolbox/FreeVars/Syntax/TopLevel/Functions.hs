{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.AST.Toolbox.FreeVars.Syntax.TopLevel.Functions (
    queryFunction
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

-- ~ Sub Queries
import qualified SLIR.HelmSyntax.AST.Toolbox.FreeVars.Syntax.TermLevel.Expr as E
-- *



queryFunction :: Decl.Function -> Sys.Bindable Decl.Function
queryFunction fn@(Rec.isRecDecl -> True) =
    applyExpr fn (getName fn : getArgs fn)


queryFunction fn =
    
    applyExpr fn (getArgs fn)


-- *
-- | Internal Helpers
-- *
applyExpr :: Decl.Function -> [CID.Ident] -> Sys.Bindable Decl.Function
applyExpr (Decl.FnDecl name args expr sig meta) binders = do
    expr' <- Scope.without binders (E.queryExpr queryFunction expr)
    
    binder (Decl.FnDecl name args expr' sig meta) [CID.ident name]

applyExpr (Decl.OpDecl name args expr sig meta) binders = do
    expr' <- Scope.without binders (E.queryExpr queryFunction expr)
    
    binder (Decl.OpDecl name args expr' sig meta) [CID.ident name]


getName :: Decl.Function -> CID.Ident
getName = CID.ident

getArgs :: Decl.Function -> [CID.Ident]
getArgs (Decl.FnDecl _ args _ _ _) =
    CID.idents args

getArgs (Decl.OpDecl _ args _ _ _) =
    CID.idents args

