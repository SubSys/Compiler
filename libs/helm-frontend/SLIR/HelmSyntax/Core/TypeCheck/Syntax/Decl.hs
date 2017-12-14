{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Core.TypeCheck.Syntax.Decl (
      inferDecl
    , inferDeclList
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

import qualified Data.Generics.Uniplate.Data as Uni


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

-- ~ Special - Misc. Helpers
import SLIR.HelmSyntax.Core.TypeCheck.Data.System.Syntax.Helpers (enter, binder)
import SLIR.HelmSyntax.Core.TypeCheck.Syntax.Misc.Utils          (inferList)
import SLIR.HelmSyntax.Core.TypeCheck.Sudo.FFI.Helpers           (ifSudoFFI, ignoreDecl)

-- ~ Sub Infers
import qualified SLIR.HelmSyntax.Core.TypeCheck.Syntax.Expr as E
-- *



{-# ANN module ("HLint: ignore" :: String) #-}





inferDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferDecl fn@(ifSudoFFI -> True)  = ignoreDecl fn
inferDecl fn@(isRec -> True)      = inferRecDecl fn

inferDecl (Decl.FnDecl name args expr sig meta) = do
    initialEnv <- Sys.getEnv
    (tv, scheme) <- TS.freshTSPair
    -- *
    
    -- * Infer Expr
    (args', ts, exprEnv) <- inferArgs args
    (expr', et, _)       <- Scope.withLocalEnv exprEnv (inferExpr' expr)
    let t1 = Fold.foldr T.Arr' et ts
    -- *
    
    -- *
    Con.unify tv t1
    Con.unifySignature t1 sig
    -- *
    
    -- *
    let newEnv = Env.extend initialEnv (CID.ident name, scheme)
    -- *
    
    
    
    -- *
    overloadFlag <- Scope.isOverloaded name
    case overloadFlag of
        Just{} ->
            enter (Decl.FnDecl name args' expr' sig meta) tv
        
        Nothing ->
            binder (Decl.FnDecl name args' expr' sig meta) tv newEnv



inferDecl (Decl.OpDecl sym args expr sig meta) = do
    initialEnv <- Sys.getEnv
    (tv, scheme) <- TS.freshTSPair
    
    -- * Infer Expr
    (args', ts, exprEnv) <- inferArgs args
    (expr', et, _)       <- Scope.withLocalEnv exprEnv (inferExpr' expr)
    let t1 = Fold.foldr T.Arr' et ts
    -- *
    
    -- *
    Con.unify tv t1
    Con.unifySignature t1 sig
    -- *
    
    -- *
    let newEnv = Env.extend initialEnv (CID.ident sym, scheme)
    -- *
    
    -- *
    overloadFlag <- Scope.isOverloaded sym
    case overloadFlag of
        Just{}  ->
            enter  (Decl.OpDecl sym args' expr' sig meta) tv

        Nothing ->
            binder (Decl.OpDecl sym args' expr' sig meta) tv newEnv










-- *
-- | Internal Helpers
-- *

inferExpr' = E.inferExpr inferDecl

-- TODO: Account for local scoped names…
--
isRec :: Decl.Function -> Bool
isRec (Decl.FnDecl name _ body _ _) =
    let xs = [y | (E.Var y _) <- Uni.universe body]
    in
        name `List.elem` xs

isRec (Decl.OpDecl sym _ body _ _) =
    error "TODO - Not yet supported: inferring recursive operator function declarations."


inferRecDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferRecDecl (Decl.FnDecl name args expr sig meta) = do
    initialEnv <- Sys.getEnv
    -- *
    
    -- *
    (args', ts, env) <- inferArgs (name : args)
    (expr', et, _)   <- Scope.withLocalEnv env (inferExpr' expr)
    -- *
    
    -- *
    let (T.Arr' t1 t2) = Fold.foldr T.Arr' et ts
    -- *
    
    -- *
    tv0 <- TS.freshType
    -- *
    
    -- *
    Con.unify t1 t2
    Con.unify (tv0 `T.Arr'` tv0) t2
    -- *
    
    -- *
    Con.unifySignature t1 sig
    -- *
    
    -- *
    (returnType, scheme) <- TS.freshTSPair
    Con.unify t1 returnType
    -- *
    
    -- *
    let newEnv = Env.extend initialEnv (CID.ident name, scheme)
    -- *
    
    
    -- *
    overloadFlag <- Scope.isOverloaded name
    case overloadFlag of
        Just{} ->
            error
                "TODO - Not yet supported: inferring **recursive overloaded** function declarations."
        
        Nothing ->
            binder (Decl.FnDecl name args expr' sig meta) returnType newEnv



inferDeclList :: [Decl.Function] -> Sys.State ([Decl.Function], [T.Type], Env.Env)
inferDeclList = inferList inferDecl



inferArgs :: [ID.Low] -> Sys.State ([ID.Low], [T.Type], Env.Env)
inferArgs = inferList inferArg


inferArg :: ID.Low -> Sys.Syntax ID.Low
inferArg name = do
    env <- Sys.getEnv
    (tv, scheme) <- TS.freshTSPair

    let env' = Env.extend env (CID.ident name, scheme)

    binder name tv env'






