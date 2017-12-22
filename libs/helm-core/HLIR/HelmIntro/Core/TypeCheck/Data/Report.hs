{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.TypeCheck.Data.Report where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

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


--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

--- Local
import HLIR.HelmIntro.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident    as CID
-- *







data TypeError
    = UnificationFail T.Type T.Type
    | InfiniteType CID.Ident T.Type
    | UnboundVariable Text
    | UnboundConstructor Text
    | Ambigious [Constraint]
    | AmbigiousOverloadedType T.Type [T.Type]
    | UnificationMismatch [T.Type] [T.Type]
    | OverloadedTypeFail T.Type [T.Type]
    | ConflictingOverloadedTypeArity [T.Type]
    deriving (Show)








