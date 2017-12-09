{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.Indexing.Data.Subst where


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
-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as ID
import qualified HLIR.HelmCore.AST.Base.Types  as T
import qualified HLIR.HelmCore.AST.Base.Values as V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as Decl

--- Local
-- *



-- *
-- | # Substitutions
-- *
type OldName = Text
type NewName = Text

newtype Subst = Subst (Map.Map OldName NewName)







-- *
-- | Substitution - Utils
-- *

empty :: Subst
empty = Subst Map.empty

extend :: Subst -> (OldName, NewName) -> Subst
extend (Subst s) (oldname, newname) =
    Subst $ Map.insert oldname newname s

remove :: Subst -> OldName -> Subst
remove (Subst s) var = Subst (Map.delete var s)

extends :: Subst -> [(OldName, NewName)] -> Subst
extends (Subst s) xs = Subst $ Map.union (Map.fromList xs) s

lookup :: OldName -> Subst -> Maybe NewName
lookup key (Subst tys) = Map.lookup key tys

merge :: Subst -> Subst -> Subst
merge (Subst a) (Subst b) = Subst (Map.union a b)

mergeSubs :: [Subst] -> Subst
mergeSubs = Fold.foldl' merge empty

singleton :: OldName -> NewName -> Subst
singleton x y = Subst (Map.singleton x y)

keys :: Subst -> [OldName]
keys (Subst s) = Map.keys s

fromList :: [(OldName, NewName)] -> Subst
fromList xs = Subst (Map.fromList xs)

toList :: Subst -> [(OldName, NewName)]
toList (Subst s) = Map.toList s





