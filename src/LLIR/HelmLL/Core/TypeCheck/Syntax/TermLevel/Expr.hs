{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module LLIR.HelmLL.Core.TypeCheck.Syntax.TermLevel.Expr (
    inferBlock
  , inferStmt
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

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP




-- + HelmLL Module Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fn
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
import LLIR.HelmLL.Core.TypeCheck.Inference.Syntax.Base (enter, binder)

-- + Local
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.System      as Sys
import qualified LLIR.HelmLL.Core.TypeCheck.Data.Report                as Report
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.Env         as Env
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Utils.TypeSystem as TS
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Syntax.Scope     as Scope
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Syntax.Constrain as Con
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Utils.General    as Util
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Engine           as Infer

-- ++ Sub Infers
import qualified LLIR.HelmLL.Core.TypeCheck.Syntax.Base.Values        as V
import qualified LLIR.HelmLL.Core.TypeCheck.Syntax.TermLevel.Patterns as P
import qualified LLIR.HelmLL.Core.TypeCheck.Syntax.Base.Etc           as Etc
-- *






{-# ANN module ("HLint: ignore" :: String) #-}



inferBlock :: S.Block -> Sys.Syntax S.Block
inferBlock (S.Block stmts) = do
    (stmts', ts, _) <- Util.inferList inferStmt stmts

    enter
        (S.Block stmts)
        (List.last ts)


inferStmt :: S.Stmt -> Sys.Syntax S.Stmt
inferStmt (S.Lit val) = do
    (val', ty, _) <- V.inferLit val
    
    enter (S.Lit val') ty


inferStmt (S.Tuple items) = do
    (items', ts, _) <- List.unzip3 <$> M.mapM (inferStmt) items
    -- *
    
    -- *
    let t = T.Tuple ts
    -- *
    
    -- *
    enter (S.Tuple items') t


inferStmt (S.List xs) = do
    (xs', ts, _) <- List.unzip3 <$> M.mapM (inferStmt) xs
    -- *
    
    
    -- *
    tv <- TS.freshType
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    enter (S.List xs') tv


inferStmt (S.ConCall id' []) = do
    t <- Scope.lookupEnv id'
    -- *
    
    
    -- *
    enter (S.ConCall id' []) t


inferStmt (S.If intros elseExpr) = do
    tv <- TS.freshType
    (intros', ts, _) <- List.unzip3 <$> M.mapM inferIfbranch intros
    (elseExpr', et, _) <- inferBlock elseExpr
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    Con.unify tv et
    -- *
    
    -- *
    enter (S.If intros' elseExpr') tv


inferStmt (S.Case con alts) = do
    (con', ct, _) <- inferStmt con
    -- *
    
    -- *
    (alts', t) <- P.inferCaseAlts inferBlock ct alts
    -- *
    
    -- *
    enter (S.Case con' alts') t


inferStmt (S.Ref name) = do
    t <- Scope.lookupEnv name
    
    enter (S.Ref name) t


inferStmt (S.FunCall ident args) = do
    (args', ts, _) <- Util.inferList (inferStmt) args
    -- *

    -- *
    t1 <- Scope.lookupEnv ident
    -- *

    -- *
    t <- Con.call ident t1 ts
    -- *
    
    
    -- *
    tv <- TS.freshType
    Con.unify tv t
    -- *
    
    

    -- *
    enter
        (S.FunCall ident args')
        tv


inferStmt (S.ConCall ident args) = do
    (args', ts, _) <- Util.inferList (inferStmt) args
    -- *

    -- *
    t1 <- Scope.lookupEnv ident
    -- *

    -- *
    t <- Con.call ident t1 ts
    -- *

    -- *
    enter
        (S.ConCall ident args')
        t

















-- *
-- | Internal Helpers
-- *






-- updateMeta = return




-- | A single 'If Then' branch.
--
inferIfbranch :: (S.Stmt, S.Block)
              -> Sys.Syntax (S.Stmt, S.Block)

inferIfbranch (intro, outro) = do
    (intro', t1, _) <- inferStmt intro
    (outro', t2, _) <- inferBlock outro
    
    Con.unify t1 T.Bool
    
    enter (intro', outro') t2






