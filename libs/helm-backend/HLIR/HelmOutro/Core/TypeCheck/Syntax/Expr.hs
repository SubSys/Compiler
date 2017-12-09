{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.TypeCheck.Syntax.Expr (
    inferExpr
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)

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
import qualified Data.String   as String

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


--- Local Deps
-- ~ HelmOutro IR
import qualified HLIR.HelmOutro.Data.Payload as Payload

-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl

--- Local
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Env                    as Env
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Canonical.Ident        as CID
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System.Constraints     as Con
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System.Scope           as Scope
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Unification.Constraint as Con

-- ~ Special - Misc. Helpers
import HLIR.HelmOutro.Core.TypeCheck.Data.System.Syntax.Helpers (enter, binder)
import HLIR.HelmOutro.Core.TypeCheck.Syntax.Misc.Utils          (inferList, inferArgs)

-- ~ Sub Infers
import qualified HLIR.HelmOutro.Core.TypeCheck.Syntax.Base.Values as V
import qualified HLIR.HelmOutro.Core.TypeCheck.Syntax.Patterns    as P

-- ~ Misc.
import qualified HLIR.HelmOutro.Core.TypeCheck.Resolve as Resolve
-- *


{-# ANN module ("HLint: ignore" :: String) #-}








inferExpr :: (Decl.Function -> Sys.Syntax Decl.Function)
          -> E.Expr
          -> Sys.Syntax E.Expr

inferExpr f (E.Var name) = do
    t <- Scope.lookupEnv name
    -- *
    
    -- *
    enter (E.Var name) t


inferExpr f (E.Lit val) = do
    (val', t, _) <- V.inferLit val
    -- *
    
    -- *
    enter (E.Lit val') t


inferExpr f (E.Tuple items) = do
    (items', ts, env) <- inferExprList f items
    -- *
    
    let t = T.Tuple ts
    
    -- *
    binder (E.Tuple items') t env


inferExpr f (E.List xs) = do
    tv           <- TS.freshType
    (xs', ts, env) <- inferExprList f xs
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    binder (E.List xs') tv env


inferExpr f (E.Con name) = do
    t <- Scope.lookupConstr name
    -- *
    
    -- *
    enter (E.Con name) t



inferExpr f (E.Let fns expr) = do
    env <- M.ask
    
    case Resolve.resolveDecls f env fns of
        Left err -> M.throwError err
        Right (fns', env', cs) -> do
            
            (expr', t1, resEnv) <- Scope.withLocalEnv env' (inferExpr f expr)
            
            _ <- (inferList f fns)
            
            
            
            enter (E.Let fns' expr') t1



inferExpr f (E.Case con alts) = do
    (con', ct, _) <- inferExpr f con
    (alts', t)    <- P.inferCaseAlts (inferExpr f) ct alts
    -- *
    
    -- *
    enter (E.Case con' alts') t


inferExpr f (E.App e1 e2) = do
    (e1', t1, env1) <- inferExpr f e1
    (e2', t2, env2) <- inferExpr f e2
    -- *
    
    -- *
    t <- Con.app t1 t2
    -- *
    
    -- *
    binder (E.App e1' e2') t (Env.merge env1 env2)


inferExpr f (E.Abs args expr) = do
    -- * Infer Expr
    (args', ts, exprEnv)  <- inferArgs args
    (expr', et, resEnv)   <- Scope.withLocalEnv exprEnv (inferExpr f expr)
    let t1 = Fold.foldr T.Arr et ts
    -- *
    
    binder (E.Abs args' expr') t1 resEnv


inferExpr f (E.FunCall ref []) = do
    t <- Scope.lookupEnv ref
    
    enter (E.FunCall ref []) t
    
inferExpr f (E.FunCall ref params) = do
    (params', ts, env) <- inferExprList f params
    -- *
    
    -- *
    t1 <- Scope.lookupEnv ref
    -- *
    
    -- *
    t <- Con.funCall t1 ts
    -- *
    
    -- *
    binder (E.FunCall ref params') t env


inferExpr f (E.ConCall con params) = do
    (params', ts, env) <- inferExprList f params
    -- *
    
    -- *
    t1 <- Scope.lookupConstr con
    -- *
    
    -- *
    t <- Con.conCall t1 ts
    -- *
    
    -- *
    binder (E.ConCall con params') t env


-- *
-- | TODO: Infer Records
-- *

inferExpr f (E.Record fields) =
    error "TODO - Not yet supported: 'inferExpr Record'…"


















-- *
-- | Internal Helpers
-- *


-- TODO:

withLocalWriter :: [Con.Constraint] -> Sys.State a -> Sys.State a
withLocalWriter cs0 =
    M.censor override
    where
        
        override cs = cs






inferExprList :: (Decl.Function -> Sys.Syntax Decl.Function)
              -> [E.Expr]
              -> Sys.State ([E.Expr], [T.Type], Env.Env)
inferExprList f es =
    inferList (inferExpr f) es







-- | A single 'If Then' branch.
--
inferIfbranch :: (Decl.Function -> Sys.Syntax Decl.Function)
              -> (E.Expr, E.Expr)
              -> Sys.Syntax (E.Expr, E.Expr)

inferIfbranch f (intro, outro) = do
    (intro', t1, _) <- inferExpr f intro
    (outro', t2, _) <- inferExpr f outro
    
    Con.unify t1 T.Bool
    
    enter (intro', outro') t2






