{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.HelmLL.Core.Index.Syntax.TermLevel.Stmt (
    indexBlock
  , indexStmt
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
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
import qualified Data.Data                    as Data
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + HelmLL Module Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fun
import qualified LLIR.HelmLL.AST.Utils.Generic.SudoFFI     as SudoFFI

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as T
import qualified LLIR.HelmLL.AST.Data.Base.Literals   as V

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as Decl



-- + Local Prelude
import LLIR.HelmLL.Core.Index.Data.System (enter)

-- + Local
import qualified LLIR.HelmLL.Core.Index.Data.System     as Sys
import qualified LLIR.HelmLL.Core.Index.Scope.Referable as Scope
import qualified LLIR.HelmLL.Core.Index.Scope.Utils     as Scope

-- ++ Sub Indexers
import qualified LLIR.HelmLL.Core.Index.Syntax.Base.Etc           as Etc
import qualified LLIR.HelmLL.Core.Index.Syntax.TermLevel.Patterns as P
-- *



indexBlock :: S.Block -> Sys.Index S.Block
indexBlock (S.Block stmts) = do
    (stmts', subs) <- Scope.indexList indexStmt stmts
    
    enter
        (S.Block stmts')
        Map.empty




indexStmt :: S.Stmt -> Sys.Index S.Stmt
indexStmt (S.Ref name) = do
    (name', _) <- Scope.referable name
    
    enter
        (S.Ref name')
        Map.empty

indexStmt (S.Lit val) =
    enter
        (S.Lit val)
        Map.empty


indexStmt (S.FunCall name args) = do
    (name', _) <- Scope.referable name
    (args', _) <- List.unzip <$> M.mapM indexStmt args
    
    enter
        (S.FunCall name' args')
        Map.empty

indexStmt (S.ConCall name args) = do
    (args', _) <- List.unzip <$> M.mapM indexStmt args
    
    enter
        (S.ConCall name args')
        Map.empty

indexStmt (S.If intros elseBlock) = do
    (intros', _) <- indexBranches intros
    (elseBlock', _) <- indexBlock elseBlock
    
    enter
        (S.If intros' elseBlock')
        Map.empty

indexStmt (S.Case con alts) = do
    (con', _) <- indexStmt con
    (alts', _) <- List.unzip <$> M.mapM (P.indexCaseAlt indexBlock) alts
    
    enter
        (S.Case con' alts')
        Map.empty

indexStmt (S.List xs) = do
    (xs', _) <- List.unzip <$> M.mapM indexStmt xs
    
    enter
        (S.List xs')
        Map.empty

indexStmt (S.Tuple items) = do
    (items', _) <- List.unzip <$> M.mapM indexStmt items
    
    enter
        (S.Tuple items')
        Map.empty

indexStmt (S.Loop con body) = do
    (con', _) <- indexStmt con
    (body', _) <- indexBlock body
    
    enter
        (S.Loop con' body')
        Map.empty




-- | Internal Helpers
--


-- NOTE: Maybe use `indexList`?
indexBranches :: [(S.Stmt, S.Block)] -> Sys.Index [(S.Stmt, S.Block)]
indexBranches intros = do
    (intros', subs) <- List.unzip <$> M.mapM indexIfIntro intros
    
    enter
        intros'
        (Map.unions (List.reverse subs))
    
    where
        indexIfIntro :: (S.Stmt, S.Block) -> Sys.Index (S.Stmt, S.Block)
        indexIfIntro (con, body) = do
            (con', s1) <- indexStmt con
            (body', s2) <- indexBlock body
            
            enter
                (con', body')
                (Map.union s1 s2)





