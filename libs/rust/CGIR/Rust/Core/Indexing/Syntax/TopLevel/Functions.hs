{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Indexing.Syntax.TopLevel.Functions (
    indexFunction
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
import qualified CGIR.Rust.Core.Indexing.Syntax.Base.Etc        as Etc
import qualified CGIR.Rust.Core.Indexing.Syntax.Base.Ident      as ID
import qualified CGIR.Rust.Core.Indexing.Syntax.TermLevel.Stmts as S
-- *






indexFunction (Decl.Function name gs args out body) = do
    mode <- Sys.getIndexingMode
    
    case mode of
        Sys.Globalizing -> global
        Sys.Localizing -> local
    
    where
        -- count = List.length args + getBinderCount expr
        
        indexBlock = S.indexBlock (S.indexStmt indexFunction)
        
        global = do
            (name', s1) <- ID.indexBinder name
            (args', s2) <- indexInputs args
            -- *
            
            -- *
            (body', _) <- Scope.withLocalSubst
                            (Sub.merge s1 s2)
                            (indexBlock body)
            -- *
            
            -- *
            enter (Decl.Function name' gs args' out body') s1

        -- local = do
        --     M.put (Sys.Counter count)
        --     -- *
        -- 
        --     -- *
        --     (args', ss) <- indexInputs args
        --     (expr', _) <- Scope.withLocalSubst ss (S.indexExpr indexFunction expr)
        --     -- *
        -- 
        --     -- *
        --     enter (Decl.Function name args' expr' scheme) Sub.empty
        local = 
            error "TODO: Local indexing not yet supported..."
    







-- *
-- | Internal Helpers
-- *

-- getBinderCount :: S.Stmt -> Int
-- getBinderCount x =
--     List.length [b | b@ID.Binder{} <- Uni.universeBi x]




