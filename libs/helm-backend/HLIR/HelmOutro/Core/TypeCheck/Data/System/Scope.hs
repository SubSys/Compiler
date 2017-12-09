{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.TypeCheck.Data.System.Scope (
      withLocalBinder
    , withLocalEnv
    , lookupEnv
    , lookupConstr
    , mergeEnvs
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
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Env                    as Env
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Subst                  as Sub
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmOutro.Core.TypeCheck.Data.Canonical.Ident        as CID
-- *



-- | Extend type environment
--
withLocalBinder :: (ID.Binder, T.Scheme) -> Sys.State a -> Sys.State a
withLocalBinder (x, sc) =
    M.local modEnv

    where
        name = CID.ident x

        modEnv e = Env.remove e name `Env.extend` (name, sc)



withLocalEnv :: Env.Env -> Sys.State a -> Sys.State a
withLocalEnv env1 =
    M.local modEnv

    where
        modEnv = Env.merge env1



-- | Lookup type in the environment
lookupEnv :: ID.Ref -> Sys.State T.Type
lookupEnv name = do
    env <- M.ask

    case Env.lookup (CID.ident name) env of
        Nothing ->
            M.throwError
                $ Report.UnboundVariable
                $ Text.pack
                $ show name

        Just s -> TS.instantiate s


lookupConstr :: ID.Big -> Sys.State T.Type
lookupConstr name = do
    env <- M.ask

    case Env.lookup (CID.ident name) env of
        Nothing   -> 
            M.throwError
                $ Report.UnboundConstructor
                $ Text.pack
                $ show name

        Just s    ->
            TS.instantiate s






mergeEnvs :: [Env.Env] -> Sys.State Env.Env
mergeEnvs [] = M.ask
mergeEnvs es =
    return $ Env.mergeEnvs es
