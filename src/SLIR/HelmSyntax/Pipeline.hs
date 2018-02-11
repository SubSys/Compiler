{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Pipeline where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    ( return
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


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as Module
import qualified SLIR.HelmSyntax.Program.Data.Interface as Program

-- + HelmSyntax AST Renderer
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Driver as Syntax

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope           as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident as ID

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
import qualified SLIR.HelmSyntax.Pipeline.Interface.ToProgram as Interface

import qualified SLIR.HelmSyntax.Module.System.InitDeps.Driver as InitDeps

-- ++ Module Pipeline
import qualified SLIR.HelmSyntax.Module.Core.InfixResolve.Driver         as Driver -- TODO
import qualified SLIR.HelmSyntax.Module.Core.Parser.Driver               as Driver
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Driver            as Driver
-- +++ Toolchain Specific
import qualified SLIR.HelmSyntax.Module.System.InitDeps.Driver           as Driver
import qualified SLIR.HelmSyntax.Module.System.Normalize.Driver          as Driver
-- +++ Per-Module Validations
import qualified SLIR.HelmSyntax.Module.Validity.UserspaceLiteral.Driver as Validate
-- ++ Program Pipeline
import qualified SLIR.HelmSyntax.Program.Core.Desugar.Driver             as Driver
import qualified SLIR.HelmSyntax.Program.Core.Lift.Driver                as Driver
import qualified SLIR.HelmSyntax.Program.Core.TypeCheck.Driver           as Driver'
-- *


pipeline :: [InitDeps.ForeignModule] -> Pre.FilePath -> IO String -> IO (Either Text Program.Program)
pipeline dependencies filePathInfo sourceCode =
    sourceCode
        |> Driver.runModuleParser filePathInfo
        |> Validate.userspaceLiteral
        |> Driver.typeCheck
        |> Interface.toProgram
        |> Driver.desugar
        |> Driver.lambdaLift
        |> Driver'.typeCheck



