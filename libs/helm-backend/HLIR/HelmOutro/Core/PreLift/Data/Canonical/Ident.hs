{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmOutro.Core.PreLift.Data.Canonical.Ident where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error)

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


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP

import qualified HLIR.HelmOutro.Render.Utils as Display


-- ~ Upstream
import qualified SLIR.HelmSyntax.Core  as HelmSyntax
import qualified HLIR.HelmCore.Core    as HelmCore


--- Local Deps
-- ~ HelmOutro Payload
import qualified HLIR.HelmOutro.Data.Payload as Payload

-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl

-- ~ HelmOutro Drivers
import qualified HLIR.HelmOutro.Core.Indexing.Driver as Driver

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
    ident (Decl.Function (ID.Binder txt _) _ _ _) = Ident txt



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


