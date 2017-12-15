{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.TypeCheck.Syntax.Expr (
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
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Env                    as Env
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Report                 as Report
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Subst                  as Sub
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.System                 as Sys
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.TypeSystem             as TS
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident        as CID
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.System.Constraints     as Con
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.System.Scope           as Scope
import qualified SLIR.HelmSyntax.Core.TypeCheck.Data.Unification.Constraint as Con

-- ~ Special - Misc. Helpers
import SLIR.HelmSyntax.Core.TypeCheck.Data.System.Syntax.Helpers (enter)
import SLIR.HelmSyntax.Core.TypeCheck.Syntax.Misc.Utils          (inferList)

-- ~ Sub Infers
import qualified SLIR.HelmSyntax.Core.TypeCheck.Syntax.Base.Values as V
import qualified SLIR.HelmSyntax.Core.TypeCheck.Syntax.Patterns    as P

-- ~ Misc.
import qualified SLIR.HelmSyntax.Core.TypeCheck.Resolve as Resolve
-- *


{-# ANN module ("HLint: ignore" :: String) #-}








inferExpr :: (Decl.Function -> Sys.Syntax Decl.Function)
          -> E.Expr
          -> Sys.Syntax E.Expr

inferExpr f (E.Lit val meta) = do
    (val', t, _) <- V.inferLit val
    -- *
    
    -- *
    enter (E.Lit val' meta) t


inferExpr f (E.Tuple items meta) = do
    (items', ts, _) <- inferExprList f items
    -- *
    
    let t = T.Tuple' ts
    
    -- *
    enter (E.Tuple items' meta) t


inferExpr f (E.List xs meta) = do
    tv           <- TS.freshType
    (xs', ts, _) <- inferExprList f xs
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    enter (E.List xs' meta) tv


inferExpr f (E.Con name meta) = do
    t <- Scope.lookupConstr name
    -- *
    
    -- *
    enter (E.Con name meta) t


inferExpr f (E.BinOp sym e1 e2 meta) = do
    (e1', t1, _) <- inferExpr f e1
    (e2', t2, _) <- inferExpr f e2
    -- *
    
    -- *
    t <- Con.binOpApp sym t1 t2
    -- *
    
    -- *
    enter (E.BinOp sym e1' e2' meta) t


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
    enter (E.If intros' elseExpr' meta) tv

    -- Resolve


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
            _ <- withLocalWriter cs (inferList f fns)
            
            (expr', t1, _) <- Scope.withLocalEnv env' (inferExpr f expr)
            
            
            enter (E.Let fns' expr' meta) t1



inferExpr f (E.Case con alts meta) = do
    (con', ct, _) <- inferExpr f con
    (alts', t)    <- P.inferCaseAlts (inferExpr f) ct alts
    -- *
    
    -- *
    enter (E.Case con' alts' meta) t


inferExpr f (E.Parens expr meta) = do
    (expr', t, _) <- inferExpr f expr
    -- *
    
    -- *
    enter (E.Parens expr' meta) t


inferExpr f (E.App e1 e2 meta) = do
    (e1', t1, _) <- inferExpr f e1
    (e2', t2, _) <- inferExpr f e2
    -- *
    
    -- *
    t <- Con.app t1 t2
    -- *
    
    -- *
    enter (E.App e1' e2' meta) t


inferExpr f (E.Abs arg expr meta) = do
    (tv, scheme) <- TS.freshTSPair
    (expr', et, _) <- Scope.withLocalBinder (arg, scheme) (inferExpr f expr)
    -- *
    
    -- *
    let t = tv `T.Arr'` et
    -- *
    
    -- *
    enter (E.Abs arg expr' meta) t



-- *
-- | TODO: Infer Records
-- *

inferExpr f (E.Record fields meta) =
    error "TODO - Not yet supported: 'inferExpr Record'…"

inferExpr f (E.RecordUpdate var fields meta) = do
    error "TODO - Not yet supported: 'inferExpr RecordUpdate'…"

inferExpr f (E.RecordAccess field object meta) =
    error "TODO - Not yet supported: 'inferExpr RecordAccess'…"


inferExpr f (E.Var name meta) = do
    res <- Scope.isOverloaded name
    case res of
        Nothing -> do
            t <- Scope.lookupEnv name
            enter (E.Var name meta) t
        
        Just ts -> do
            t <- genOverloaded ts
            
            enter (E.Var name meta) t













-- *
-- | Internal Helpers
-- *


-- |  Generalize Overloaded Types
-- TODO: Move...
genOverloaded :: [T.Type] -> Sys.State T.Type
genOverloaded ts
    | List.all (== base1) restBases = do
        t <- TS.instantiate $ TS.closeOver base1
        -- *

        -- *
        superTv <- TS.freshType
        let t2 = T.Superposed superTv ts
        -- *
        
        -- *
        Con.unify t t2
        -- *
        
        -- *
        return t
    
    | otherwise =
        M.throwError (Report.ConflictingOverloadedTypeArity ts)


    where
        
        xs1@(base1:restBases) = map base ts
        
        base :: T.Type -> T.Type
        base x = fst $ M.runState (Uni.transformM f x) [0..]
        
        f :: (M.MonadState [Int] m) => T.Type -> m T.Type
        f (T.Arr t1 t2 _) = return $ T.Arr' t1 t2
        
        f x = do
            (x:xs) <- M.get
            M.put xs
            
            return $ T.Var' (ID.Low' (label x) Nothing)
        
        
        label x =
            Text.pack $ show x
        
        
        -- checkBases :: T.Type -> T.Type -> Bool
        -- checkBases 



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
    
    Con.unify t1 T.Bool'
    
    enter (intro', outro') t2




