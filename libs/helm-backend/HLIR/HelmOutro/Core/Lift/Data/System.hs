{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Lift.Data.System where


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
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP

--- Local Deps
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

-- ~ Auxiliary Nodes
import qualified HLIR.HelmOutro.AST.Auxiliary.Canonical.Ident as CID

--- Local
-- *


{-# ANN module "HLint: ignore" #-}


runState :: Env
         -> Lift a
         -> Either LiftError (a, [Decl.Function])
runState env m = M.runExcept $ M.evalRWST m env initCounter


initCounter =
    Counter 0

initEnv :: [Decl.Function] -> Env
initEnv fns =
    let globals = map CID.ident fns
    in
        (Map.empty, globals)



-- *
-- | ## System
-- *

data LiftError
    = UnknownType Text
    deriving (Show)

newtype Counter = Counter Int

type Globals = [CID.Ident]
type LiftedEnv = (Map.Map CID.Ident [CID.Ident])

type Env = (LiftedEnv, Globals)


type Lift a = M.RWST Env [Decl.Function] Counter (M.Except LiftError) a



getLifted :: Lift LiftedEnv
getLifted = do
    (x, _) <- M.ask
    return x

getGlobals :: Lift Globals
getGlobals = do
    (_, x) <- M.ask
    return x





