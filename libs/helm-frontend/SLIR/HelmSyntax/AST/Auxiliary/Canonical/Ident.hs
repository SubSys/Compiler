{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.Data (Data, Typeable)

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
data Ident = Ident Text (Maybe ID.Namespace) (Maybe Meta.Meta)
    deriving (Show, Data, Typeable)



pattern Ident' :: Text -> Ident
pattern Ident' txt = Ident txt Nothing Nothing




class Identifiable a where
    ident :: a -> Ident



-- *
-- | Identifiable Instances
-- *

instance Identifiable ID.Low where
    ident (ID.Low x ns m) = Ident x ns m

instance Identifiable ID.Big where
    ident (ID.Big x ns m) = Ident x ns m


instance Identifiable ID.Sym where
    ident (ID.Sym x ns m) = Ident x ns m


instance Identifiable Decl.Function where
    ident (Decl.FnDecl (ID.Low txt ns m) _ _ _ _) = Ident txt ns m
    ident (Decl.OpDecl (ID.Sym txt ns m) _ _ _ _) = Ident txt ns m


instance Identifiable Decl.Union where
    ident (Decl.Union name _ _ _) = ident name



-- *
-- | Misc. Aux. Instances
-- *
idents :: Identifiable a => [a] -> [Ident]
idents = map ident


-- *
-- | Misc. Helpers
-- *


toLow :: Ident -> ID.Low
toLow (Ident txt ns m) =
    ID.Low txt ns m


toSym :: Ident -> ID.Sym
toSym (Ident txt ns m) =
    ID.Sym txt ns m


toBig :: Ident -> ID.Big
toBig (Ident txt ns m) =
    ID.Big txt ns m


-- *
-- | Internals
-- *


instance Eq Ident where
    (==) (Ident txt1 ns1 _) (Ident txt2 ns2 _) =
        (txt1 == txt2) && (ns1 == ns2)


instance Ord Ident where
    compare (Ident txt1 ns1 _) (Ident txt2 ns2 _) =
        
        compare txt1 txt2 <> compare ns1 ns2




