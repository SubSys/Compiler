{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.TypeCheck.Data.System where


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
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Interface.TypesEnv     as TI
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident              as CID
-- *




-- *
-- | # Data Types
-- *


newtype Counter = Counter Int
    deriving (Show)


type Overloaded = Map.Map CID.Ident [T.Scheme]

type State a = M.RWST (TI.Env, Overloaded) [Con.Constraint] Counter (M.Except Report.TypeError) a




type Syntax a = State (a, T.Type, TI.Env)



-- |
-- Simply Constrain some type, pair.
type Constrain = State ()


-- | 
-- For emitting a constraint of something & returning something else…
type Constrain' a = State a






-- *
-- | # Misc. Utils
-- *




initCounter :: Counter
initCounter =
    Counter 0


incCounter :: State Int
incCounter = do
    (Counter c) <- M.get
    M.put (Counter $ c + 1)
    
    return c
    


-- | Run the inference monad
runState :: (TI.Env, Overloaded)
         -> State a
         -> Either Report.TypeError (a, [Con.Constraint])
runState env m = M.runExcept $ M.evalRWST m env initCounter



runInfer :: (TI.Env, Overloaded)
         -> State (a, T.Type, TI.Env)
         -> Either Report.TypeError (a, T.Type, TI.Env, [Con.Constraint])
runInfer env m =
    M.runExcept $ pack <$> M.evalRWST m env initCounter
    
    where
        pack ((x, t, e), cs) = (x, t, e, cs)



-- *
-- | Misc. State Helpers
-- *


getEnv :: State TI.Env
getEnv =
    fst <$> M.ask


getOverloads :: State Overloaded
getOverloads =
    snd <$> M.ask






