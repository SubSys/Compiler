{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.AST.Toolbox.Internal.Globalize.Data.Env where


-- *
import Core
import Core.Control.Flow
import Prelude (return)

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Control.Monad.State.Lazy as State


--- Local
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl
-- *




-- *
-- | # Env
-- *


data Env = Env Counter Subst
    deriving (Show)


data Subst = Subst
    { local :: Map.Map Text Int
    , global :: Map.Map Text Int
    }
    deriving (Show)


type Counter = Int




-- *
-- | ## Env Helpers
-- *
initEnv :: Counter -> Env
initEnv idx =
    Env idx initSubst


updateBinderState :: Text -> Counter -> Env -> Env
updateBinderState name idx env =
    let sub = newSubst name idx
        env' = decCounter $ mergeSubst sub env
    in
        env'



-- *
-- | ## Substitution Helpers
-- *
initSubst :: Subst
initSubst =
    Subst
        { local = Map.empty
        , global = Map.empty
        }

mergeSubst :: Subst -> Env -> Env
mergeSubst subst (Env counter subs) =
    Env counter (Map.union subst subs)

newLocalSubst :: Text -> Int -> Subst
newLocalSubst txt idx =
    Map.fromList [(txt, idx)]



getSubst :: Env -> Subst
getSubst (Env _ subs) = subs

clearSubst :: Env -> Env
clearSubst (Env c _) =
    Env c Map.empty


-- *
-- | ## Counter Helpers
-- *


getCounterIdx :: Env -> Counter
getCounterIdx (Env c _) = c


-- | Decrement Counter
--
decCounter :: Env -> Env
decCounter (Env counter subs) =
    Env (counter + 1) subs


