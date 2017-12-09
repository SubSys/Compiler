{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.TypeCheck.Syntax.Expr (
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
-- ~ HelmCore IR
import qualified HLIR.HelmCore.Data.Payload as Payload

-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as ID
import qualified HLIR.HelmCore.AST.Base.Types  as T
import qualified HLIR.HelmCore.AST.Base.Values as V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as Decl

--- Local
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Env                    as Env
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmCore.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmCore.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Canonical.Ident        as CID
import qualified HLIR.HelmCore.Core.TypeCheck.Data.System.Constraints     as Con
import qualified HLIR.HelmCore.Core.TypeCheck.Data.System.Scope           as Scope
import qualified HLIR.HelmCore.Core.TypeCheck.Data.Unification.Constraint as Con

-- ~ Special - Misc. Helpers
import HLIR.HelmCore.Core.TypeCheck.Data.System.Syntax.Helpers (enter)
import HLIR.HelmCore.Core.TypeCheck.Syntax.Misc.Utils          (inferList)

-- ~ Sub Infers
import qualified HLIR.HelmCore.Core.TypeCheck.Syntax.Base.Values as V
import qualified HLIR.HelmCore.Core.TypeCheck.Syntax.Patterns    as P

-- ~ Misc.
import qualified HLIR.HelmCore.Core.TypeCheck.Resolve as Resolve
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
    (items', ts, _) <- inferExprList f items
    -- *
    
    let t = T.Tuple ts
    
    -- *
    enter (E.Tuple items') t


inferExpr f (E.List xs) = do
    tv           <- TS.freshType
    (xs', ts, _) <- inferExprList f xs
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    enter (E.List xs') tv


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
            
            -- |
            -- Just to re-gen. constraints for this env...
            -- TODO - Stub.
            -- * Eventually become something like:
            -- ** `withLocalWriter cs (Scope.withLocalEnv env' (inferExpr f expr))`
            --
            _ <- withLocalWriter cs (inferList f fns)
            
            (expr', t1, _) <- Scope.withLocalEnv env' (inferExpr f expr)
            
            
            enter (E.Let fns' expr') t1



inferExpr f (E.Case con alts) = do
    (con', ct, _) <- inferExpr f con
    (alts', t)    <- P.inferCaseAlts (inferExpr f) ct alts
    -- *
    
    -- *
    enter (E.Case con' alts') t


inferExpr f (E.App e1 e2) = do
    (e1', t1, _) <- inferExpr f e1
    (e2', t2, _) <- inferExpr f e2
    -- *
    
    -- *
    t <- Con.app t1 t2
    -- *
    
    -- *
    enter (E.App e1' e2') t


inferExpr f (E.Abs arg expr) = do
    (tv, scheme) <- TS.freshTSPair
    (expr', et, _) <- Scope.withLocalBinder (arg, scheme) (inferExpr f expr)
    -- *
    
    -- *
    let t = tv `T.Arr` et
    -- *
    
    -- *
    enter (E.Abs arg expr') t



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




