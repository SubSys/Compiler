{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmCore.Core.TypeCheck.Syntax.Decl (
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

-- ~ Special - Misc. Helpers
import HLIR.HelmCore.Core.TypeCheck.Data.System.Syntax.Helpers (enter, binder)
import HLIR.HelmCore.Core.TypeCheck.Syntax.Misc.Utils          (inferList)
import HLIR.HelmCore.Core.TypeCheck.Sudo.FFI.Helpers           (ifSudoFFI, ignoreDecl)

-- ~ Sub Infers
import qualified HLIR.HelmCore.Core.TypeCheck.Syntax.Expr as E
-- *



{-# ANN module ("HLint: ignore" :: String) #-}





inferDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferDecl fn@(ifSudoFFI -> True)  = ignoreDecl fn
inferDecl fn@(isRec -> True)      = inferRecDecl fn


inferDecl (Decl.Function name expr sig) = do
    initialEnv <- M.ask
    (tv, scheme) <- TS.freshTSPair
    -- *
    
    -- * Infer Expr
    (expr', et, _)       <- inferExpr' expr
    -- *
    
    -- *
    Con.unify tv et
    -- Con.unify tv t1
    -- Con.unifySignature t1 sig
    -- *
    
    -- *
    let newEnv = Env.extend initialEnv (CID.ident name, scheme)
    -- *
    
    binder (Decl.Function name expr' sig) tv newEnv










-- *
-- | Internal Helpers
-- *

inferExpr' = E.inferExpr inferDecl

-- TODO: Account for local scoped names…
--
isRec :: Decl.Function -> Bool
isRec (Decl.Function name body _) =
    let xs = [y | (E.Var y) <- Uni.universe body]
        ys = CID.idents xs
    in
        CID.ident name `List.elem` ys


inferRecDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferRecDecl (Decl.Function name expr sig) = do
    initialEnv <- M.ask
    -- *
    
    -- *
    (args', ts, env) <- inferArgs [name]
    (expr', et, _)   <- Scope.withLocalEnv env (inferExpr' expr)
    -- *
    
    -- *
    let (T.Arr t1 t2) = Fold.foldr T.Arr et ts
    -- *
    
    -- *
    tv0 <- TS.freshType
    -- *
    
    -- *
    Con.unify t1 t2
    Con.unify (tv0 `T.Arr` tv0) t2
    -- *
    
    -- *
    -- Con.unifySignature t1 sig
    -- *
    
    -- *
    (returnType, scheme) <- TS.freshTSPair
    Con.unify t1 returnType
    -- *
    
    -- *
    let newEnv = Env.extend initialEnv (CID.ident name, scheme)
    -- *
    
    -- *
    binder (Decl.Function name expr' sig) returnType newEnv



inferDeclList :: [Decl.Function] -> Sys.State ([Decl.Function], [T.Type], Env.Env)
inferDeclList = inferList inferDecl



inferArgs :: [ID.Binder] -> Sys.State ([ID.Binder], [T.Type], Env.Env)
inferArgs = inferList inferArg


inferArg :: ID.Binder -> Sys.Syntax ID.Binder
inferArg name = do
    env <- M.ask
    (tv, scheme) <- TS.freshTSPair

    let env' = Env.extend env (CID.ident name, scheme)

    binder name tv env'






