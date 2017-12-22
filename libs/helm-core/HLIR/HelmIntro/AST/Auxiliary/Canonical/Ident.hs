{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)
import Data.Monoid ((<>))

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
import qualified HLIR.HelmIntro.AST.Data.Base.Ident    as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types    as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values   as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

--- Local
-- *


-- | 
-- Canonical identification for HelmSyntax’s various naming representations.
--
data Ident = Ident Text (Maybe ID.Namespace)
    deriving (Show, Eq, Ord)



pattern Ident' :: Text -> Ident
pattern Ident' txt = Ident txt Nothing




class Identifiable a where
    ident :: a -> Ident



-- *
-- | Identifiable Instances
-- *

instance Identifiable ID.Low where
    ident (ID.Low x ns) = Ident x ns

instance Identifiable ID.Big where
    ident (ID.Big x ns) = Ident x ns


instance Identifiable ID.Binder where
    ident (ID.Binder x ns) = Ident x ns

instance Identifiable ID.Ref where
    ident (ID.Ref x ns) = Ident x ns

instance Identifiable Decl.Function where
    ident (Decl.Function (ID.Binder txt ns) _ _ _) = Ident txt ns


instance Identifiable Decl.Union where
    ident (Decl.Union name _ _) = ident name



-- *
-- | Misc. Aux. Instances
-- *
idents :: Identifiable a => [a] -> [Ident]
idents = map ident


-- *
-- | Misc. Helpers
-- *


toLow :: Ident -> ID.Low
toLow (Ident txt ns) =
    ID.Low txt ns


toBig :: Ident -> ID.Big
toBig (Ident txt ns) =
    ID.Big txt ns


toRef :: Ident -> ID.Ref
toRef (Ident txt ns) =
    ID.Ref txt ns


toBinder :: Ident -> ID.Binder
toBinder (Ident txt ns) =
    ID.Binder txt ns



