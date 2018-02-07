{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Program.Dev.Utils.Interface.ToModule (
    toModule
  , toModule'
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

import Data.Data (Data, Typeable)

import qualified Prelude as Pre


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
import qualified Data.Functor.Foldable as F


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

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
import qualified SLIR.HelmSyntax.Module.Data.Interface  as Module
import qualified SLIR.HelmSyntax.Program.Data.Interface as Program
-- *


toModule :: IO (Either Text Program.Program) -> IO (Either Text Module.Module)
toModule input = do
    result <- input

    case result of
        Left err -> return $ Left err
        Right payload ->
            return
                $ Right
                $ toModule' payload




toModule' :: Program.Program -> Module.Module 
toModule' payload =
    Module.Module
        { Module.header = Module.Header
            { Module.moduleName = ID.Namespace [Text.pack "Dev"]
            , Module.exports    = Header.Everything
            , Module.imports    = []
            , Module.modulePath = Text.pack "/dev/null"
            }
        , Module.program = Module.Program
            { Module.functions = fns
            , Module.unions = uns
            , Module.fixities = []
            }
        
        , Module.deps = Module.emptyDependencies
        }
    where
        fns = Program.getFunctions payload
        uns = Program.getUnions payload





