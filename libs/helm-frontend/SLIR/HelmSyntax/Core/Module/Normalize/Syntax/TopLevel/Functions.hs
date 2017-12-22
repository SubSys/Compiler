{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Normalize.Syntax.TopLevel.Functions (
    normFunction
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
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Utils.Namespace       as NS
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Data.System           as Sys

import qualified SLIR.HelmSyntax.Core.Module.Normalize.Syntax.Base.Types     as T
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Syntax.Base.Ident     as ID
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Syntax.TermLevel.Expr as E
-- *




normFunction :: Decl.Function -> Sys.Env Decl.Function
normFunction (Decl.FnDecl name args expr sig meta) = do
    name' <- ID.normLow name
    expr' <- E.normExpr normFunction expr
    sig' <- normSig sig
    return
        $ Decl.FnDecl name' args expr' sig' meta

normFunction (Decl.OpDecl sym args expr sig meta) = do
    sym' <- ID.normSym sym
    expr' <- E.normExpr normFunction expr
    sig' <- normSig sig
    return
        $ Decl.OpDecl sym' args expr' sig' meta



-- *
-- | Normalize Sig
-- *


-- NOTE: All sigs should be validated at this point...

normSig :: Maybe Etc.Signature -> Sys.Env (Maybe Etc.Signature)
normSig (Just (Etc.Validated (T.Forall as ty) meta)) = do
    ty' <- T.normType ty
    
    return
        $ Just
        $ Etc.Validated (T.Forall as ty') meta


