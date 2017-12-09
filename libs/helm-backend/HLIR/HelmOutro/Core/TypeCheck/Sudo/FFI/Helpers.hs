{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HLIR.HelmOutro.Core.TypeCheck.Sudo.FFI.Helpers (
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
import HLIR.HelmOutro.Core.TypeCheck.Syntax.Misc.Utils          (inferList)
-- *



{-# ANN module ("HLint: ignore" :: String) #-}




ifSudoFFI :: Decl.Function -> Bool
ifSudoFFI (Decl.Function _ _ expr (Just _)) =
    checkExpr expr

ifSudoFFI _ = False


checkExpr :: E.Expr -> Bool
checkExpr (E.Var (ID.Ref _ (Just (ID.Namespace ("Sudo":"Helm":"Native":_))))) =
    True

checkExpr (E.FunCall (ID.Ref _ (Just (ID.Namespace ("Sudo":"Helm":"Native":_)))) _) =
    True

checkExpr (E.ConCall _ params) =
    map checkExpr params
        |> List.any (== True)

checkExpr (E.App e _) =
    checkExpr e

checkExpr (E.Abs _ e) =
    checkExpr e

checkExpr _ = False




ignoreDecl :: Decl.Function -> Sys.Syntax Decl.Function

ignoreDecl fn@(Decl.Function name args expr (Just scheme)) = do
    initialEnv <- M.ask
    -- *

    -- *
    t          <- TS.instantiate scheme
    -- *

    -- *
    let newEnv = Env.extend initialEnv (CID.ident name, scheme)
    -- *

    binder fn t newEnv



