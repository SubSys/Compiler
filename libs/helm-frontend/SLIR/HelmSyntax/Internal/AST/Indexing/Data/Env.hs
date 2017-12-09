{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Internal.AST.Indexing.Data.Env where


-- *
import Core
import Core.Control.Flow
import Prelude (mapM_, IO, String, return, (<$>))

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Control.Monad.State.Lazy as State




--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *




{-# ANN module "HLint: ignore" #-}








data Env = Env Counter Subst

type Counter = Int


type Subst = Map.Map Text Int


initEnv :: Env
initEnv = Env 0 Map.empty


incCounter :: Env -> Env
incCounter (Env c s) = Env (c + 1) s

getCounter :: Env -> Counter
getCounter (Env c _) = c

mergeCounter ::  Env -> Counter -> Env
mergeCounter (Env _ subs) c =
    Env c subs


mergeSubst :: Subst -> Env -> Env
mergeSubst subst (Env counter subs) =
    Env counter (Map.union subst subs)

newSubst :: Text -> Int -> Subst
newSubst name idx =
    Map.fromList [(name, idx)]


getSubst :: Env -> Subst
getSubst (Env _ s) = s







