{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Core.Program.SDD.Syntax.TermLevel.Expr (
    inferExpr
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl


-- ~~ Auxiliary AST - Nodes & Utils
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident            as CID
import qualified SLIR.HelmSyntax.AST.Toolbox.SudoFFI                      as SudoFFI
import qualified SLIR.HelmSyntax.AST.Toolbox.TopLevel.Functions.Recursive as Rec


--- Local Prelude
import SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Base (enter, binder)

--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env               as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                      as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System            as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Scope     as Scope
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem       as TS
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Constrain as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Auxiliary        as Aux
import qualified SLIR.HelmSyntax.Core.Program.SDD.Resolve.Engine                   as Resolve

-- ~ Sub Inferers
import qualified SLIR.HelmSyntax.Core.Program.SDD.Syntax.Base.Values        as V
import qualified SLIR.HelmSyntax.Core.Program.SDD.Syntax.TermLevel.Patterns as P
-- *



inferExpr :: (Decl.Function -> Sys.Syntax Decl.Function) -> E.Expr -> Sys.Syntax E.Expr
inferExpr f (E.Lit val meta) = do
    (val', ty, _) <- V.inferLit val
    
    enter (E.Lit val' meta) ty


inferExpr f (E.Var name meta) = do
    t <- Scope.lookupEnv (inferExpr f) name
    
    enter (E.Var name meta) t


inferExpr f (E.Tuple items metaOpt) = do
    (items', ts, _) <- List.unzip3 <$> M.mapM (inferExpr f) items
    -- *
    
    -- *
    let t = T.Tuple' ts
    -- *
    
    -- *
    enter (E.Tuple items' metaOpt) t


inferExpr f (E.List xs metaOpt) = do
    (xs', ts, _) <- List.unzip3 <$> M.mapM (inferExpr f) xs
    -- *
    
    
    -- *
    tv <- TS.freshType
    M.mapM_ (Con.unify tv) ts
    -- *
    
    
    -- *
    enter (E.List xs' metaOpt) tv


inferExpr f (E.Con id' metaOpt) = do
    t <- Scope.lookupEnv' id'
    -- *
    
    
    -- *
    enter (E.Con id' metaOpt) t


inferExpr f (E.BinOp sym e1 e2 metaOpt) = do
    (e1', t1, _) <- inferExpr f e1
    (e2', t2, _) <- inferExpr f e2
    -- *
    
    -- *
    t <- Con.binOpApp sym t1 t2
    -- *
    
    -- *
    enter (E.BinOp sym e1' e2' metaOpt) t


inferExpr f (E.If intros elseExpr metaOpt) = do
    tv <- TS.freshType
    (intros', ts, _) <- List.unzip3 <$> M.mapM (inferIfbranch f) intros
    (elseExpr', et, _) <- inferExpr f elseExpr
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    Con.unify tv et
    -- *
    
    -- *
    enter (E.If intros' elseExpr' metaOpt) tv


-- inferExpr f (E.Let fns expr metaOpt) = do
--     (fns', ts, env) <- List.unzip3 <$> M.mapM f fns
--     -- *
-- 
--     -- *
--     (expr', t, _) <- Scope.withLocalEnv env (inferExpr f expr)
--     -- *
-- 
-- 
--     -- *
--     enter (E.Let fns' expr' metaOpt) t


inferExpr f (E.Let fns expr meta) = do
    env <- M.ask

    case Resolve.resolveDecls f env fns of
        Left err -> M.throwError err
        Right (fns', env', cs) -> do

            -- |
            -- Just to re-gen. constraints for this env...
            -- TODO - Stub.
            -- * Eventually become something like:
            -- ** `withLocalWriter cs (Scope.withLocalEnv env' (inferExpr f expr))`
            --
            _ <- Aux.inferList f fns

            (expr', t1, _) <- Scope.withLocalEnv env' (inferExpr f expr)


            enter (E.Let fns' expr' meta) t1


inferExpr f (E.Case con alts metaOpt) = do
    (con', ct, _) <- inferExpr f con
    -- *
    
    -- *
    (alts', t) <- P.inferCaseAlts (inferExpr f) ct alts
    -- *
    
    
    -- *
    enter (E.Case con' alts' metaOpt) t


inferExpr f (E.Parens expr metaOpt) = do
    (expr', ty, _) <- inferExpr f expr
    -- *
    
    
    -- *
    enter (E.Parens expr' metaOpt) ty


inferExpr f (E.App e1 e2 metaOpt) = do
    (e1', t1, _) <- inferExpr f e1
    (e2', t2, _) <- inferExpr f e2
    -- *
    
    -- *
    t <- Con.app t1 t2
    -- *
    
    
    -- *
    enter (E.App e1' e2' metaOpt) t


inferExpr f (E.Abs arg expr meta) = do
    (tv, scheme) <- TS.freshTSPair
    (expr', et, _) <- Scope.withLocalBinder (arg, scheme) (inferExpr f expr)
    -- *

    -- *
    let t = tv `T.Arr'` et
    -- *

    -- *
    enter (E.Abs arg expr' meta) t


inferExpr f expr@(E.AltAbs _ (SudoFFI.callsSudo -> True) (Just scheme)) = do
    t  <- TS.instantiate scheme
    -- *
    
    -- *
    enter expr t
    

inferExpr f (E.AltAbs args expr scheme) = do
    (args', ts, exprEnv) <- Aux.inferList inferArg args
    (expr', et, _)       <- Scope.withLocalEnv exprEnv (inferExpr f expr)
    let t1 = Fold.foldr T.Arr' et ts
    -- *
    
    -- *
    Con.unifyScheme t1 scheme
    -- *

    -- *
    enter (E.AltAbs args' expr' scheme) t1
    
    where
        
        inferArg :: ID.Low -> Sys.Syntax ID.Low
        inferArg name = do
            env <- M.ask
            (tv, scheme) <- TS.freshTSPair

            let env' = Map.insert (CID.ident name) (Env.Normal scheme) env

            binder name tv env'




-- TODO:...

-- inferExpr f (E.Record fields metaOpt) = do
--     tv <- TS.freshType
--     -- *
-- 
-- 
--     -- *
--     enter (E.Record fields metaOpt) tv
-- 
-- inferExpr f (E.RecordUpdate var fields metaOpt) = do
--     tv <- TS.freshType
--     -- *
-- 
-- 
--     -- *
--     enter (E.RecordUpdate var fields metaOpt) tv
-- 
-- 
-- inferExpr f (E.RecordAccess field object metaOpt) = do
--     tv <- TS.freshType
--     -- *
-- 
--     -- *
--     enter (E.RecordAccess field object metaOpt) tv





-- *
-- | Internal Helpers
-- *




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




