{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.TypeCheck.Data.System where


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
-- ~ HelmOutro IR
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
--- Local
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Env                    as Env
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Report                 as Report
-- *




-- *
-- | # Data Types
-- *


newtype Counter = Counter Int
    deriving (Show)


type State a = M.RWST Env.Env [Con.Constraint] Counter (M.Except Report.TypeError) a




type Syntax a = State (a, T.Type, Env.Env)



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
runState :: Env.Env
         -> State a
         -> Either Report.TypeError (a, [Con.Constraint])
runState env m = M.runExcept $ M.evalRWST m env initCounter



runInfer :: Env.Env
         -> State (a, T.Type, Env.Env)
         -> Either Report.TypeError (a, T.Type, Env.Env, [Con.Constraint])
runInfer env m =
    M.runExcept $ pack <$> M.evalRWST m env initCounter
    
    where
        pack ((x, t, e), cs) = (x, t, e, cs)












