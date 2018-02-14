{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module HLIR.HelmFlat.Core.TypeCheck.Syntax.TermLevel.Expr (
    inferExpr
) where


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


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Module.Data.Interface as I

-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Scope           as Scope
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Ident as ID

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local Prelude
import HLIR.HelmFlat.Core.TypeCheck.Inference.Syntax.Base (enter, binder)

-- + Local
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Data.System      as Sys
import qualified HLIR.HelmFlat.Core.TypeCheck.Data.Report                as Report
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Data.Env         as Env
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Utils.TypeSystem as TS
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Syntax.Scope     as Scope
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Syntax.Constrain as Con
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Utils.General    as Util
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Engine           as Infer

-- ++ Sub Infers
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.Base.Values        as V
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.TermLevel.Patterns as P
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.Base.Etc           as Etc
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.Base.Metadata      as Meta
-- *






{-# ANN module ("HLint: ignore" :: String) #-}







inferExpr :: (Decl.Function -> Sys.Syntax Decl.Function) -> E.Expr -> Sys.Syntax E.Expr
inferExpr f (E.Lit val meta) = do
    (val', ty, _) <- V.inferLit val
    
    -- *
    meta' <- Meta.recordType ty meta
    -- *
    
    enter (E.Lit val' meta') ty


inferExpr f (E.Tuple items meta) = do
    (items', ts, _) <- List.unzip3 <$> M.mapM (inferExpr f) items
    -- *
    
    -- *
    let t = T.Tuple' ts
    -- *
    
    -- *
    meta' <- Meta.recordType t meta
    -- *
    
    -- *
    enter (E.Tuple items' meta') t


inferExpr f (E.List xs meta) = do
    (xs', ts, _) <- List.unzip3 <$> M.mapM (inferExpr f) xs
    -- *
    
    
    -- *
    tv <- TS.freshType
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    meta' <- Meta.recordType tv meta
    -- *
    
    -- *
    enter (E.List xs' meta') tv


inferExpr f (E.Constr id' meta) = do
    t <- Scope.lookupEnv id'
    -- *
    
    -- *
    meta' <- Meta.recordType t meta
    -- *
    
    -- *
    enter (E.Constr id' meta') t


inferExpr f (E.InfixApp sym e1 e2 meta) = do
    (e1', t1, _) <- inferExpr f e1
    (e2', t2, _) <- inferExpr f e2
    -- *

    -- *
    t <- Con.binOpApp sym t1 t2
    -- *
    
    -- *
    meta' <- Meta.recordType t meta
    -- *

    -- *
    enter (E.InfixApp sym e1' e2' meta') t


inferExpr f (E.If intros elseExpr meta) = do
    tv <- TS.freshType
    (intros', ts, _) <- List.unzip3 <$> M.mapM (inferIfbranch f) intros
    (elseExpr', et, _) <- inferExpr f elseExpr
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    Con.unify tv et
    -- *
    
    -- *
    meta' <- Meta.recordType tv meta
    -- *
    
    -- *
    enter (E.If intros' elseExpr' meta') tv



inferExpr f (E.Let fns expr meta) = do
    env <- M.ask

    case Infer.resolveDecls f env fns of
        Left err -> M.throwError err
        Right (fns', env', cs) -> do

            -- |
            -- Just to re-gen. constraints for this env...
            -- TODO - Stub.
            -- * Eventually become something like:
            -- ** `withLocalWriter cs (Scope.withLocalEnv env' (inferExpr f expr))`
            --
            _ <- Util.inferList f fns

            (expr', t1, _) <- Scope.withLocalEnv env' (inferExpr f expr)
            
            
            -- *
            meta' <- Meta.recordType t1 meta
            -- *


            enter (E.Let fns' expr' meta') t1


inferExpr f (E.Case con alts meta) = do
    (con', ct, _) <- inferExpr f con
    -- *
    
    -- *
    (alts', t) <- P.inferCaseAlts (inferExpr f) ct alts
    -- *
    
    -- *
    meta' <- Meta.recordType t meta
    -- *
    
    -- *
    enter (E.Case con' alts' meta') t


inferExpr f (E.Parens expr meta) = do
    (expr', ty, _) <- inferExpr f expr
    -- *
    
    -- *
    meta' <- Meta.recordType ty meta
    -- *
    
    -- *
    enter (E.Parens expr' meta') ty


inferExpr f (E.App e1 e2 meta) = do
    (e1', t1, _) <- inferExpr f e1
    (e2', t2, _) <- inferExpr f e2
    -- *
    
    -- *
    t <- Con.app t1 t2
    -- *
    
    -- *
    meta' <- Meta.recordType t meta
    -- *
    
    
    -- *
    enter (E.App e1' e2' meta') t



inferExpr f (E.Abs arg expr meta) = do
    (tv, scheme) <- TS.freshTSPair
    (expr', et, _) <- Scope.withLocalBinder (ID.get arg, scheme) (inferExpr f expr)
    -- *

    -- *
    let t = tv `T.Arr'` et
    -- *
    
    -- *
    (arg', argTy, _) <- Etc.inferBinder arg
    Con.unify argTy tv
    -- *
    
    -- *
    meta' <- Meta.recordType t meta
    -- *

    -- *
    enter (E.Abs arg' expr' meta') t


-- inferExpr f (E.Var name meta) = do
--     res <- Scope.isOverloaded name
--     case res of
--         Nothing -> do
--             t <- Scope.lookupEnv name
-- 
--             -- *
--             meta' <- Meta.recordType t meta
--             -- *
-- 
--             enter (E.Var name meta') t
-- 
--         -- Just ss -> do
--         --     ts <- M.mapM TS.instantiate ss
--         -- 
--         --     -- *
--         --     tv <- TS.freshType
--         --     superTv@(T.Var ident _) <- TS.freshType
--         --     -- *
--         -- 
--         --     -- *
--         --     Con.unify tv (T.Superposed [ident] ts)
--         --     -- *
--         -- 
--         --     -- *
--         --     meta' <- Meta.recordType tv meta
--         --     -- *
--         -- 
--         --     enter (E.Var name meta') tv
-- 
-- 
--         Just ss -> do
--             t <- OL.synthType ss
--             -- *
-- 
--             -- *
--             meta' <- Meta.recordType t meta
--             -- *
-- 
--             -- *
--             enter (E.Var name meta') t




inferExpr f (E.Var name meta) = do
    res <- Scope.isOverloaded name
    case res of
        Nothing -> do
            t <- Scope.lookupEnv name
            
            -- *
            meta' <- Meta.recordType t meta
            -- *
            
            enter (E.Var name meta') t
        
        Just ss -> do
            -- ts <- M.mapM TS.instantiate ss
            -- t <- genOverloaded ts
            t <- Con.overloaded ss
            -- *
            
            -- *
            meta' <- Meta.recordType t meta
            -- *
            
            enter (E.Var name meta') t






inferExpr f (E.FunCall ident args ty meta) = do
    res <- Scope.isOverloaded ident
    
    case res of
        Nothing -> do
            -- (args', ts, _) <- List.unzip3 <$> M.mapM (inferExpr f) args
            (args', ts, _) <- Util.inferList (inferExpr f) args
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
            -- meta' <- Meta.recordType tv meta
            meta' <- Meta.recordOverloadedTargetType tv t1 meta
            -- *
            
            

            -- *
            enter
                (E.FunCall ident args' (Just tv) meta')
                tv -- Ensure this is a type variable for later substitution of infered type,
                   -- (for `HLIR.HelmFlat.Core.TypeCheck.Inference.Engine`)...
        
        Just ss -> do
            t <- Con.overloaded ss
            -- *
            
            -- *
            (args', ts, _) <- Util.inferList (inferExpr f) args
            -- *
            
            -- *
            tv <- TS.freshType
            let ts1 = Fold.foldr T.Arr' tv ts
            -- *
            
            -- *
            Con.unify ts1 t
            -- *
            
            -- *
            meta' <- Meta.recordOverloadedTargetType tv ts1 meta
            -- *
            
            -- *
            enter
                (E.FunCall ident args' (Just tv) meta')
                -- (E.FunCall ident args' (Just t) meta') -- NOTE: TODO: use `t` for overloading resolve...
                tv





-- inferExpr f (E.FunCall ident args ty meta) = do
--     -- (args', ts, _) <- List.unzip3 <$> M.mapM (inferExpr f) args
--     (args', ts, _) <- Util.inferList (inferExpr f) args
--     -- *
-- 
--     -- *
--     t1 <- Scope.lookupEnv ident
--     -- *
-- 
--     -- *
--     t <- Con.call ident t1 ts
--     -- *
-- 
-- 
--     -- *
--     meta' <- Meta.recordType t meta
--     -- *
-- 
--     -- *
--     enter
--         (E.FunCall ident args' (Just t) meta')
--         t -- Ensure this is a type variable for later substitution of infered type,
--            -- (for `HLIR.HelmFlat.Core.TypeCheck.Inference.Engine`)...


inferExpr f (E.ConCall ident args ty meta) = do
    -- (args', ts, _) <- List.unzip3 <$> M.mapM (inferExpr f) args
    (args', ts, _) <- Util.inferList (inferExpr f) args
    -- *

    -- *
    t1 <- Scope.lookupEnv ident
    -- *

    -- *
    t <- Con.call ident t1 ts
    -- *
    
    -- *
    meta' <- Meta.recordType t meta
    -- *


    -- *
    enter
        (E.ConCall ident args' (Just t) meta')
        t -- Ensure this is a type variable for later substitution…

















-- *
-- | Internal Helpers
-- *






-- updateMeta = return




-- | A single 'If Then' branch.
--
inferIfbranch :: (Decl.Function -> Sys.Syntax Decl.Function)
              -> (E.Expr, E.Expr)
              -> Sys.Syntax (E.Expr, E.Expr)

inferIfbranch f (intro, outro) = do
    (intro', t1, _) <- inferExpr f intro
    (outro', t2, _) <- inferExpr f outro
    
    Con.unify t1 T.Bool'
    
    enter (intro', outro') t2




alpha f x y =
    f x y







