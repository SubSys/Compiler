{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env (
    ExprEnv(..)
  , Entry(..)
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

-- ~~ Auxiliary Nodes
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID

--- Local
-- *



-- | Types
--
-- type TypesEnv = Map.Map CID.Ident T.Scheme


type ExprEnv = Map.Map CID.Ident Entry

data Entry
    = Normal     T.Scheme
    | Superposed T.Scheme E.Expr
    | Overloaded (Map.Map T.Scheme E.Expr)
    deriving (Show, Data, Typeable)


