{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HLIR.HelmCore.Core.TypeCheck.Sudo.FFI.Helpers (
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
-- *



{-# ANN module ("HLint: ignore" :: String) #-}




ifSudoFFI :: Decl.Function -> Bool
ifSudoFFI (Decl.Function _ expr _) =
    checkExpr expr



checkExpr :: E.Expr -> Bool
checkExpr (E.Var (ID.Ref _ (Just (ID.Namespace ("Sudo":"Helm":"Native":_))))) =
    True
checkExpr (E.App e _) =
    checkExpr e

checkExpr (E.Abs _ e) =
    checkExpr e

checkExpr _ = False




ignoreDecl :: Decl.Function -> Sys.Syntax Decl.Function

ignoreDecl fn@(Decl.Function name expr scheme) = do
    initialEnv <- M.ask
    -- *

    -- *
    t          <- TS.instantiate scheme
    -- *

    -- *
    let newEnv = Env.extend initialEnv (CID.ident name, scheme)
    -- *

    binder fn t newEnv



