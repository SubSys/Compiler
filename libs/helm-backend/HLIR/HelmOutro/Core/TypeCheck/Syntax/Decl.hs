{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmOutro.Core.TypeCheck.Syntax.Decl (
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

-- ~ Special - Misc. Helpers
import HLIR.HelmOutro.Core.TypeCheck.Data.System.Syntax.Helpers (enter, binder)
import HLIR.HelmOutro.Core.TypeCheck.Syntax.Misc.Utils          (inferList, inferArgs)
import HLIR.HelmOutro.Core.TypeCheck.Sudo.FFI.Helpers           (ifSudoFFI, ignoreDecl)

-- ~ Sub Infers
import qualified HLIR.HelmOutro.Core.TypeCheck.Syntax.Expr as E
-- *




{-# ANN module ("HLint: ignore" :: String) #-}




inferDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferDecl fn@(ifSudoFFI -> True)  = ignoreDecl fn
inferDecl fn@(isRec -> True)      = inferRecDecl fn

inferDecl (Decl.Function name args expr sig) = do
    initialEnv <- M.ask
    (tv, scheme) <- TS.freshTSPair
    -- *
    
    -- * Infer Expr
    (args', ts, exprEnv)      <- inferArgs args
    (expr', et, resEnv)       <- Scope.withLocalEnv exprEnv (inferExpr' expr)
    let t1 = Fold.foldr T.Arr et ts
    -- *
    
    -- *
    Con.unify tv t1
    -- Con.unifySignature t1 sig
    -- *
    
    -- *
    let newEnv = Env.extend initialEnv (CID.ident name, scheme)
    -- let newEnv = Env.extend resEnv (CID.ident name, scheme)
    -- *
    
    binder (Decl.Function name args' expr' sig) tv (Env.merge newEnv resEnv)







-- *
-- | Internal Helpers
-- *

inferExpr' = E.inferExpr inferDecl

-- TODO: Account for local scoped names…
--
isRec :: Decl.Function -> Bool
isRec (Decl.Function name args body _) =
    let xs = CID.idents [ref | (E.Var ref)     <- Uni.universe body]
        ys = CID.idents [ref | (E.FunCall ref _) <- Uni.universe body]
        
        -- Finish
        check1 = CID.ident name `List.elem` xs
        check2 = CID.ident name `List.elem` ys
    in
        List.any (== True) [check1, check2]


inferRecDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferRecDecl (Decl.Function name args expr sig) = do
    initialEnv <- M.ask
    -- *
    
    -- *
    (args', ts, env) <- inferArgs (toArg name : args)
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
    binder (Decl.Function name args expr' sig) returnType newEnv
    
    where
        toArg :: ID.Binder -> Etc.Arg
        toArg b = Etc.Arg b Nothing



inferDeclList :: [Decl.Function] -> Sys.State ([Decl.Function], [T.Type], Env.Env)
inferDeclList = inferList inferDecl
