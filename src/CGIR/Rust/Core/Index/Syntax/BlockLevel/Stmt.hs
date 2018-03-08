{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Index.Syntax.BlockLevel.Stmt (
    indexStmt
  , indexBlock
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
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
import qualified Data.Data                    as Data

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + RustCG AST Interface
import qualified CGIR.Rust.Data.Interface as I

-- + RustCG AST
-- ++ Base
import qualified CGIR.Rust.AST.Data.Base.Ident                 as ID
import qualified CGIR.Rust.AST.Data.Base.Literals              as Lit
import qualified CGIR.Rust.AST.Data.Base.Types                 as T
import qualified CGIR.Rust.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.Rust.AST.Data.TermLevel.Stmt            as S
import qualified CGIR.Rust.AST.Data.TermLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified CGIR.Rust.AST.Data.TopLevel.Enums.Variants   as Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Enums            as Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Functions        as Decl

-- + Local Prelude
import CGIR.Rust.Core.Index.Data.System (enter, binder)

-- + Local
import qualified CGIR.Rust.Core.Index.Data.System                as Sys
import qualified CGIR.Rust.Core.Index.Scope.Bindable             as Scope
import qualified CGIR.Rust.Core.Index.Scope.Referable            as Scope
import qualified CGIR.Rust.Core.Index.Scope.Utils                as Scope
import qualified CGIR.Rust.Core.Index.Syntax.BlockLevel.Patterns as P
-- *

-- TODO: multi-line stmts
--


indexBlock :: S.Block -> Sys.Index S.Block
indexBlock (S.Block [stmt]) = do
    (stmt', _) <- indexStmt stmt
    
    enter (S.Block [stmt'])


indexStmt :: S.Stmt -> Sys.Index S.Stmt
indexStmt (S.Box value) = do
    (value', _) <- indexStmt value
    
    enter (S.Box value')
    
indexStmt (S.Lit val) =
    enter (S.Lit val)
    
indexStmt (S.Ref path) = do
    (path', _) <- Scope.referable path
    
    enter (S.Ref path')
    
indexStmt (S.FunCall path args) = do
    (path', _) <- Scope.referable path
    (args', _) <- List.unzip <$> M.mapM indexStmt args
    
    enter (S.FunCall path' args')

indexStmt (S.ConCall path args) = do
    (args', _) <- List.unzip <$> M.mapM indexStmt args
    
    enter (S.ConCall path args)

indexStmt (S.If intros elseStmt) = do
    (intros', _) <- indexIfIntros intros
    (elseStmt', _) <- indexStmt elseStmt
    
    enter (S.If intros' elseStmt')
    
indexStmt (S.Match con arms) = do
    (con', _) <- indexStmt con
    (arms', _) <- List.unzip <$> M.mapM (P.indexArm indexStmt) arms
    
    enter (S.Match con' arms')
    
indexStmt (S.List xs) = do
    (xs', _) <- List.unzip <$> M.mapM indexStmt xs
    
    enter (S.List xs')
    
indexStmt (S.Tuple items) = do
    (items', _) <- List.unzip <$> M.mapM indexStmt items
    
    enter (S.List items')
    



-- | Internal Helpers
--

indexIfIntros :: [(S.Stmt, S.Stmt)] -> Sys.Index [(S.Stmt, S.Stmt)]
indexIfIntros intros = do
    intros' <- M.mapM indexIfIntro intros
    
    enter intros'
    
    where
        indexIfIntro :: (S.Stmt, S.Stmt) -> Sys.State (S.Stmt, S.Stmt)
        indexIfIntro (con, body) = do
            (con', _) <- indexStmt con
            (body', _) <- indexStmt body
            
            return (con', body')


