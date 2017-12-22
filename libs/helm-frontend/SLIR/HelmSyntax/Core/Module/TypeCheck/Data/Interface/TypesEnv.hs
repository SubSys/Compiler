{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Interface.TypesEnv where


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
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID
-- *

-- | NOTE:
-- * I usually import this as
--   `import qualified SLIR.HelmSyntax.Core.Module.TypeCheck.Data.Interface.TypesEnv as TI`,
--   just because it’s a couple characters shorter... 
--   I.e. for 'Types - Interface'.


newtype Env = Env
    { types :: Map.Map CID.Ident T.Scheme
    }
    deriving (Eq, Show)

empty :: Env
empty = Env Map.empty

extend :: Env -> (CID.Ident, T.Scheme) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

remove :: Env -> CID.Ident -> Env
remove (Env env) var = Env (Map.delete var env)

extends :: Env -> [(CID.Ident, T.Scheme)] -> Env
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }

lookup :: CID.Ident -> Env -> Maybe T.Scheme
lookup key (Env tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (Env a) (Env b) = Env (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = Fold.foldl' merge empty

singleton :: CID.Ident -> T.Scheme -> Env
singleton x y = Env (Map.singleton x y)

keys :: Env -> [CID.Ident]
keys (Env env) = Map.keys env

fromList :: [(CID.Ident, T.Scheme)] -> Env
fromList xs = Env (Map.fromList xs)

toList :: Env -> [(CID.Ident, T.Scheme)]
toList (Env env) = Map.toList env

instance Monoid.Monoid Env where
    mempty = empty
    mappend = merge




