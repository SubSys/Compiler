{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Program.Core.Lift.Data.System (
    Lift
  , Env
  , incCounter
  , runLift
  , emptyEnv
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    (return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP




-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Program.Data.Interface as I

-- + HelmSyntax AST Renderer
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Driver as Syntax

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope                         as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident               as ID
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.SudoFFI   as SudoFFI
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Functions.Recursive as Rec
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Type                as T
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Binders             as Binder

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Header   as Header

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl


-- + Local
import qualified SLIR.HelmSyntax.Program.Core.Lift.Data.Report as Report
-- *




-- | Lifter - Data Types
---

type Env = Map.Map ID.Ident [ID.Ident]

newtype Counter = Counter Int


type Lift a = M.RWST Env [Decl.Function] Counter (M.Except Report.LiftError) a














-- | Lifter - Utils
---

initCounter :: Counter
initCounter = Counter 0

incCounter :: Lift Int
incCounter = do
    (Counter i) <- M.get
    
    M.put (Counter $ i + 1)
    
    return i


emptyEnv :: Env
emptyEnv = Map.empty


runLift :: Env -> Lift a -> Either Report.LiftError (a, [Decl.Function])
runLift env m = M.runExcept $ M.evalRWST m env initCounter









