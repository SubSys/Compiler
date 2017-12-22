{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.AST.Toolbox.FreeVars.Internal.Utils.Scope (
      without
    , emitRef
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
import qualified SLIR.HelmSyntax.AST.Toolbox.FreeVars.Data.System as Sys
-- *



without :: [CID.Ident] -> Sys.Query a -> Sys.Query a
without vs =
    M.censor filter
    where
        filter = without' vs


without' :: [CID.Ident] -> [CID.Ident] -> [CID.Ident]
without' binders refs =
    List.filter (not . isSudoFFI)
        $ Fold.foldr (List.filter . (/=)) refs binders



emitRef :: CID.Identifiable a => a -> Sys.Query ()
emitRef ref =
    M.tell [CID.ident ref]



isSudoFFI :: CID.Ident -> Bool
isSudoFFI (CID.Ident txt (SudoFFI.isSudoNS -> True) _) = True
isSudoFFI _ = False
