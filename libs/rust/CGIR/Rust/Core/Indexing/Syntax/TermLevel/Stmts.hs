{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Indexing.Syntax.TermLevel.Stmts (
      indexStmt
    , indexBlock
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

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
import qualified Text.Show.Prettyprint as PP

--- Local Deps
-- ~ (GCIR) - Rust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Types  as T
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified CGIR.Rust.AST.TermLevel.Stmt        as S
import qualified CGIR.Rust.AST.TermLevel.Patterns    as P
import qualified CGIR.Rust.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.Functions as Decl
import qualified CGIR.Rust.AST.TopLevel.Unions    as Decl



--- Local
import qualified CGIR.Rust.Core.Indexing.Data.Subst            as Sub
import qualified CGIR.Rust.Core.Indexing.Data.System           as Sys
import qualified CGIR.Rust.Core.Indexing.Data.System.Bindable  as Bind
import qualified CGIR.Rust.Core.Indexing.Data.System.Referable as Ref
import qualified CGIR.Rust.Core.Indexing.Data.System.Scope     as Scope

-- ~ Indexable Stuff
import CGIR.Rust.Core.Indexing.Data.System (Index, Indexable(..), enter)

-- ~ Misc. Utils
import CGIR.Rust.Core.Indexing.Auxiliary.Utils (indexList, indexMaybe, indexInputs)

-- ~ Sub Indexers
import qualified CGIR.Rust.Core.Indexing.Syntax.Base.Ident         as ID
import qualified CGIR.Rust.Core.Indexing.Syntax.TermLevel.Patterns as P
-- *



indexStmt :: (Decl.Function -> Sys.Index Decl.Function)
          -> S.Stmt
          -> Sys.Index S.Stmt


indexStmt f (S.Ref ref) = do
        (ref', _) <- ID.indexRef ref

        enter (S.Ref ref') Sub.empty

indexStmt f (S.Lit val) =
        enter (S.Lit val) Sub.empty

-- indexStmt f (S.Record fields) =
--         error "TODO - Not yet supported: Indexing record expr fields…"

indexStmt f (S.Tuple items) = do
        (items', _) <- List.unzip <$> M.mapM (indexStmt f) items

        enter (S.Tuple items') Sub.empty

indexStmt f (S.List xs) = do
        (xs', _) <- List.unzip <$> M.mapM (indexStmt f) xs

        enter (S.List xs') Sub.empty


indexStmt f (S.Case con alts) = do
        (con', _) <- indexStmt f con
        (alts', _) <- List.unzip <$> M.mapM (P.indexCaseAlt indexBlock') alts
        
        enter (S.Case con' alts') Sub.empty
        
    where
        indexBlock' = indexBlock (indexStmt f)



indexStmt f (S.FunCall ref params) = do
    (ref', _) <- ID.indexRef ref
    (params', _) <- List.unzip <$> M.mapM (indexStmt f) params
    
    enter (S.FunCall ref' params') Sub.empty


indexStmt f (S.ConCall con params) = do
    (params', _) <- List.unzip <$> M.mapM (indexStmt f) params
    
    enter (S.ConCall con params') Sub.empty




indexBlock :: (S.Stmt -> Sys.Index S.Stmt) -> Decl.Block -> Sys.Index Decl.Block
indexBlock f (Decl.Block stmts) = do
    (stmts', _) <- indexList f stmts
    
    enter (Decl.Block stmts') Sub.empty



