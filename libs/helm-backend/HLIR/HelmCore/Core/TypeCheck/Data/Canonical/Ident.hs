{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmCore.Core.TypeCheck.Data.Canonical.Ident where


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
-- ~ HelmCore IR
import qualified HLIR.HelmCore.Data.Payload as Payload

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


-- | 
-- Canonical identification for HelmSyntax’s various naming representations.
--
newtype Ident = Ident Text
    deriving (Show, Eq, Ord)






class Identifiable a where
    ident :: a -> Ident



-- *
-- | Identifiable Instances
-- *

instance Identifiable ID.Low where
    ident (ID.Low x _) = Ident x

instance Identifiable ID.Big where
    ident (ID.Big x _) = Ident x


instance Identifiable ID.Binder where
    ident (ID.Binder x _) = Ident x

instance Identifiable ID.Ref where
    ident (ID.Ref x _) = Ident x


instance Identifiable Decl.Function where
    ident (Decl.Function (ID.Binder txt _) _ _) = Ident txt



-- *
-- | Misc. Aux. Instances
-- *
idents :: Identifiable a => [a] -> [Ident]
idents = map ident


-- *
-- | Misc. Helpers
-- *


toLow :: Ident -> ID.Low
toLow (Ident txt) =
    ID.Low txt Nothing


toBig :: Ident -> ID.Big
toBig (Ident txt) =
    ID.Big txt Nothing

toBinder :: Ident -> ID.Binder
toBinder (Ident txt) =
    ID.Binder txt Nothing


toRef :: Ident -> ID.Ref
toRef (Ident txt) =
    ID.Ref txt Nothing



-- *
-- | Internals
-- *





