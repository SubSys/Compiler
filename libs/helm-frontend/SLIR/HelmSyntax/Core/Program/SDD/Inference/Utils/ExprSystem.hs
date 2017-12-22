{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.ExprSystem (
      instantiate
    , instantiate'
    , freshVar
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


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Payload

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


--- Local Internal-Utils
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Internal.FreshIdents as Fresh

--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env         as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint     as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System      as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Types                as TySub
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Expr                 as ExSub
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem as TS
-- *




instantiate :: Decl.Function -> E.Expr
instantiate fn@(Rec.isRecDecl -> True) = error "NOT YET SUPPORTED: instantiate rec fn"

instantiate fn@(Decl.FnDecl name args expr sig meta) =
    -- E.SudoNode fn (Fold.foldr E.Abs' expr args)
    E.AltAbs args expr (serializeSig sig)

instantiate fn@(Decl.OpDecl name args expr sig meta) =
    E.AltAbs args expr (serializeSig sig)



instantiate' :: Decl.Function -> E.Expr
instantiate' fn@(Rec.isRecDecl -> True) = error "NOT YET SUPPORTED: instantiate rec fn"

instantiate' fn@(Decl.FnDecl name args expr sig meta) =
    Fold.foldr E.Abs' expr args

instantiate' fn@(Decl.OpDecl name args expr sig meta) =
    Fold.foldr E.Abs' expr args




-- *
-- | Internal Helpers
-- *

serializeSig :: Maybe Etc.Signature -> Maybe T.Scheme
serializeSig (Just (Etc.Validated scheme meta)) = Just scheme
-- serializeSig (Just (Etc.Unresolved ty meta))    = Just ty
-- serializeSig _ = Nothing

freshVar :: Sys.Infer E.Expr
freshVar = do
    ident <- Fresh.freshIdent

    return $ E.Var' ident


