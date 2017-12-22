{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>))

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


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Payload

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

-- ~~ Auxiliary Nodes
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID


--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env     as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report            as Report
-- *




-- *
-- | State Related - Data Types 
-- *
newtype Counter = Counter Int

type Infer a = M.RWST Env.ExprEnv [Con.Constraint] Counter (M.Except Report.TypeError) a


type Syntax a = Infer (a, T.Type, Env.ExprEnv)


-- *
-- | Misc. Utils
-- *

incCounter :: Infer Int
incCounter = do
    (Counter i) <- M.get
    M.put (Counter $ i + 1)
    
    return i



initCounter :: Counter
initCounter =
    Counter 0

    


-- | Run the inference monad
-- runState :: (TI.Env, Overloaded)
--          -> State a
--          -> Either Report.TypeError (a, [Con.Constraint])
-- runState env m = M.runExcept $ M.evalRWST m env initCounter



runInfer :: Env.ExprEnv
         -> Infer (a, T.Type, Env.ExprEnv)
         -> Either Report.TypeError (a, T.Type, Env.ExprEnv, [Con.Constraint])
runInfer env m =
    M.runExcept $ pack <$> M.evalRWST m env initCounter

    where
        pack ((x, t, e), cs) = (x, t, e, cs)





