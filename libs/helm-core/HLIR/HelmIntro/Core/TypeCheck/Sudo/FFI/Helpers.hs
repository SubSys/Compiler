{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module HLIR.HelmIntro.Core.TypeCheck.Sudo.FFI.Helpers (
      ifSudoFFI
    , ignoreDecl
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
import qualified HLIR.HelmIntro.AST.Data.Base.Etc    as Etc
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Fixities  as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl

--- Local
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Interface.TypesEnv     as TI
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident              as CID
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.System.Constraints     as Con
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.System.Scope           as Scope

-- ~ Special - Misc. Helpers
import HLIR.HelmIntro.Core.TypeCheck.Data.System.Syntax.Helpers (enter, binder)
import HLIR.HelmIntro.Core.TypeCheck.Syntax.Misc.Utils          (inferList)
-- *



{-# ANN module ("HLint: ignore" :: String) #-}




ifSudoFFI :: Decl.Function -> Bool
ifSudoFFI (Decl.FnDecl _ _ expr (Just (Etc.Unresolved _ _)) _) =
    checkExpr expr

ifSudoFFI (Decl.OpDecl _ _ expr (Just (Etc.Unresolved _ _)) _) =
    checkExpr expr

ifSudoFFI _ = False


checkExpr :: E.Expr -> Bool
checkExpr (E.Var (ID.Low _ (Just (ID.Namespace ("Sudo":"Helm":"Native":_))) _) _) =
    True
checkExpr (E.App e _ _) =
    checkExpr e

checkExpr _ = False




ignoreDecl :: Decl.Function -> Sys.Syntax Decl.Function

ignoreDecl fn@(Decl.OpDecl name args expr (Just (Etc.Unresolved ty _)) meta) = do
    initialEnv <- Sys.getEnv
    -- *

    -- *
    let scheme = TS.closeOver ty
    t         <- TS.instantiate scheme
    -- *

    -- *
    let newEnv = TI.extend initialEnv (CID.ident name, scheme)
    -- *
    
    
    -- *
    overloadFlag <- Scope.isOverloaded name
    case overloadFlag of
        Just{} ->
            enter fn t
        
        Nothing ->
            binder fn t newEnv


ignoreDecl fn@(Decl.FnDecl name args expr (Just (Etc.Unresolved ty _)) meta) = do
    initialEnv <- Sys.getEnv
    -- *

    -- *
    let scheme = TS.closeOver ty
    t          <- TS.instantiate scheme
    -- *

    -- *
    let newEnv = TI.extend initialEnv (CID.ident name, scheme)
    -- *

    -- *
    overloadFlag <- Scope.isOverloaded name
    case overloadFlag of
        Just{} ->
            enter fn t
        Nothing ->
            binder fn t newEnv


