{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Normalize.Syntax.TermLevel.Patterns (
    normCaseAlt
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
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Utils.Namespace as NS
import qualified SLIR.HelmSyntax.Core.Module.Normalize.Data.System     as Sys
-- *


normCaseAlt :: (E.Expr -> Sys.Env E.Expr)
            -> P.CaseAlt
            -> Sys.Env P.CaseAlt
normCaseAlt f (P.CaseAlt patrn expr meta) = do
    patrn' <- normPattern patrn
    expr' <- f expr

    return
        $ P.CaseAlt patrn' expr' meta


normPattern :: P.Pattern -> Sys.Env P.Pattern

normPattern (P.Lit lit meta) =
    return
        $ P.Lit lit meta

normPattern (P.Record vars meta) =
    return
        $ P.Record vars meta

normPattern (P.List xs meta) =
    return
        $ P.List xs meta

normPattern (P.Cons xs rest meta) =
    return
        $ P.Cons xs rest meta

normPattern (P.Tuple items meta) =
    return
        $ P.Tuple items meta

normPattern (P.Con id' args meta) =
    return
        $ P.Con id' args meta

normPattern (P.Var id' meta) =
    return
        $ P.Var id' meta

normPattern (P.Wildcard meta) =
    return
        $ P.Wildcard meta



