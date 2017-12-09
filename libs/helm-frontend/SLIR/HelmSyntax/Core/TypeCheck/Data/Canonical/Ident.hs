{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.Core.TypeCheck.Data.Canonical.Ident where


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

--- Local
-- *


-- | 
-- Canonical identification for HelmSyntax’s various naming representations.
--
data Ident = Ident Text (Maybe Meta.Meta)
    deriving (Show)



pattern Ident' :: Text -> Ident
pattern Ident' txt = Ident txt Nothing




class Identifiable a where
    ident :: a -> Ident



-- *
-- | Identifiable Instances
-- *

instance Identifiable ID.Low where
    ident (ID.Low x _ m) = Ident x m

instance Identifiable ID.Big where
    ident (ID.Big x _ m) = Ident x m


instance Identifiable ID.Sym where
    ident (ID.Sym x _ m) = Ident x m


instance Identifiable Decl.Function where
    ident (Decl.FnDecl (ID.Low txt _ m) _ _ _ _) = Ident txt m
    ident (Decl.OpDecl (ID.Sym txt _ m) _ _ _ _) = Ident txt m



-- *
-- | Misc. Aux. Instances
-- *
idents :: Identifiable a => [a] -> [Ident]
idents = map ident


-- *
-- | Misc. Helpers
-- *


toLow :: Ident -> ID.Low
toLow (Ident txt m) =
    ID.Low txt Nothing m


toSym :: Ident -> ID.Sym
toSym (Ident txt m) =
    ID.Sym txt Nothing m


toBig :: Ident -> ID.Big
toBig (Ident txt m) =
    ID.Big txt Nothing m


-- *
-- | Internals
-- *


instance Eq Ident where
    (==) (Ident txt1 _) (Ident txt2 _) =
        txt1 == txt2


instance Ord Ident where
    compare (Ident txt1 _) (Ident txt2 _) =
        compare txt1 txt2




