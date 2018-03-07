{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.GLSL.Core.Index.Syntax.BlockLevel.Stmt (
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



-- + GLSL AST Interface
import qualified CGIR.GLSL.Data.Interface as I

-- + GLSL AST
-- ++ Base
import qualified CGIR.GLSL.AST.Data.Base.Ident                 as ID
import qualified CGIR.GLSL.AST.Data.Base.Literals              as Lit
import qualified CGIR.GLSL.AST.Data.Base.Types                 as T
import qualified CGIR.GLSL.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.GLSL.AST.Data.TermLevel.Stmt            as S
-- ++ Decl/Top Level
import qualified CGIR.GLSL.AST.Data.TopLevel.Functions         as Decl
import qualified CGIR.GLSL.AST.Data.TopLevel.Globals           as Decl

-- + Local Prelude
import CGIR.GLSL.Core.Index.Data.System (enter)

-- + Local
import qualified CGIR.GLSL.Core.Index.Data.System     as Sys
import qualified CGIR.GLSL.Core.Index.Scope.Bindable  as Scope
import qualified CGIR.GLSL.Core.Index.Scope.Referable as Scope
import qualified CGIR.GLSL.Core.Index.Scope.Utils     as Scope
-- *




indexBlock :: S.Block -> Sys.Index S.Block

indexBlock (S.Block stmts) = do
    (stmts', subs) <- Scope.indexList indexStmt stmts
    
    enter
        (S.Block stmts')
        subs


indexStmt :: S.Stmt -> Sys.Index S.Stmt
indexStmt (S.Lit value) = enter (S.Lit value) Map.empty
indexStmt (S.Ref ident) = do
    (ident', _) <- Scope.referable ident
    
    enter
        (S.Ref ident')
        Map.empty

indexStmt (S.Assignment lvalue rvalue) = do
    (lvalue', s1) <- indexStmt lvalue
    (rvalue', s2) <- indexStmt rvalue
    
    enter
        (S.Assignment lvalue' rvalue')
        (Map.union s2 s1)

indexStmt (S.FunCall ident args) = do
    (ident', _) <- Scope.referable ident
    (args', ss) <- List.unzip <$> M.mapM indexStmt args
    
    enter
        (S.FunCall ident' args')
        (Map.unions $ List.reverse ss)

indexStmt (S.ConCall ident args) = do
    (args', ss) <- List.unzip <$> M.mapM indexStmt args
    
    enter
        (S.ConCall ident args)
        (Map.unions $ List.reverse ss)

indexStmt (S.MethodAccess name method) = do
    (name', _) <- Scope.referable name
    
    enter
        (S.MethodAccess name' method)
        Map.empty

indexStmt (S.ArrayAccess name index) = do
    (name', _) <- Scope.referable name
    
    enter
        (S.ArrayAccess name' index)
        Map.empty

indexStmt (S.If intros elseStmt) = do
    (intros', s1) <- indexBranches intros
    (elseStmt', s2) <- Scope.indexMaybe indexBlock elseStmt
    
    enter
        (S.If intros' elseStmt')
        (Map.union s2 s1)

indexStmt (S.Switch con branches defBranch) = do
    (con', s1)       <- indexStmt con
    (branches', s2)  <- indexBranches branches
    (defBranch', s3) <- Scope.indexMaybe indexBlock defBranch
    
    enter
        (S.Switch con' branches' defBranch')
        (Map.unions [s3, s2, s1])

indexStmt (S.For (initExpr, conExpr, loopExpr) body) = do
    (initExpr', s1) <- indexStmt initExpr
    (conExpr', s2) <- indexStmt conExpr
    (loopExpr', s3) <- indexStmt loopExpr
    (body', s4) <- indexBlock body
    
    enter
        (S.For (initExpr', conExpr', loopExpr') body')
        (Map.unions [s4, s3, s2, s1])

indexStmt (S.While con body) = do
    (con', s1) <- indexStmt con
    (body', s2) <- indexBlock body
    
    enter
        (S.While con' body')
        (Map.union s2 s1)

indexStmt S.Continue = enter S.Continue Map.empty
indexStmt S.Discard  = enter S.Discard Map.empty
indexStmt S.Break    = enter S.Break Map.empty

indexStmt (S.Return value) = do
    (value', subs) <- Scope.indexMaybe indexStmt value
    
    enter
        (S.Return value')
        subs

indexStmt (S.Init ty name constant) = do
    (name', s1)     <- Scope.bindable name
    (constant', s2) <- Scope.indexMaybe indexStmt constant
    
    enter
        (S.Init ty name' constant')
        (Map.union s1 s2)





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





