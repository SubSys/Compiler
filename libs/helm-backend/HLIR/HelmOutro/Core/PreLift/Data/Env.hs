{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.PreLift.Data.Env where


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
import qualified HLIR.HelmOutro.Core.PreLift.Data.Canonical.Ident as CID
-- *




newtype Env = Env
    { types :: Map.Map CID.Ident T.Type
    }
    deriving (Eq, Show)

empty :: Env
empty = Env Map.empty

extend :: Env -> (CID.Ident, T.Type) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

remove :: Env -> CID.Ident -> Env
remove (Env env) var = Env (Map.delete var env)

extends :: Env -> [(CID.Ident, T.Type)] -> Env
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }

lookup :: CID.Ident -> Env -> Maybe T.Type
lookup key (Env tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (Env a) (Env b) = Env (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = Fold.foldl' merge empty

singleton :: CID.Ident -> T.Type -> Env
singleton x y = Env (Map.singleton x y)

keys :: Env -> [CID.Ident]
keys (Env env) = Map.keys env

fromList :: [(CID.Ident, T.Type)] -> Env
fromList xs = Env (Map.fromList xs)

toList :: Env -> [(CID.Ident, T.Type)]
toList (Env env) = Map.toList env

instance Monoid.Monoid Env where
    mempty = empty
    mappend = merge





