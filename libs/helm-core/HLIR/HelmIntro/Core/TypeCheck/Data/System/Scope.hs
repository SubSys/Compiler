{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.TypeCheck.Data.System.Scope (
      withLocalBinder
    , withLocalEnv
    , lookupEnv
    , lookupConstr
    , lookupSym
    , mergeEnvs
    , isOverloaded
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
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Unification.Constraint as Con
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Interface.TypesEnv     as TI
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.Report                 as Report
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.System                 as Sys
import qualified HLIR.HelmIntro.Core.TypeCheck.Data.TypeSystem             as TS
import qualified HLIR.HelmIntro.AST.Auxiliary.Canonical.Ident              as CID
-- *



-- | Extend type environment
--
withLocalBinder :: (ID.Low, T.Scheme) -> Sys.State a -> Sys.State a
withLocalBinder (x, sc) =
    M.local modEnv

    where
        name = CID.ident x

        modEnv (e, os) = (TI.remove e name `TI.extend` (name, sc), os)



withLocalEnv :: TI.Env -> Sys.State a -> Sys.State a
withLocalEnv env1 =
    M.local modEnv

    where
        modEnv (env2, os) = (TI.merge env1 env2, os)



-- | Lookup type in the environment
lookupEnv :: ID.Low -> Sys.State T.Type
lookupEnv name = do
    env <- Sys.getEnv

    case TI.lookup (CID.ident name) env of
        Nothing ->
            M.throwError
                $ Report.UnboundVariable
                $ Text.pack
                $ show name

        Just s -> TS.instantiate s


lookupConstr :: ID.Big -> Sys.State T.Type
lookupConstr name = do
    env <- Sys.getEnv

    case TI.lookup (CID.ident name) env of
        Nothing   -> 
            M.throwError
                $ Report.UnboundConstructor
                $ Text.pack
                $ show name

        Just s    ->
            TS.instantiate s




lookupSym :: ID.Sym -> Sys.State T.Type
lookupSym name = do
    env <- Sys.getEnv

    case TI.lookup (CID.ident name) env of
        Nothing   -> 
            M.throwError
                $ Report.UnboundVariable
                $ Text.pack
                $ show name

        Just s    ->
            TS.instantiate s





mergeEnvs :: [TI.Env] -> Sys.State TI.Env
mergeEnvs [] = Sys.getEnv
mergeEnvs es =
    return $ TI.mergeEnvs es




isOverloaded :: CID.Identifiable a => a -> Sys.State (Maybe [T.Type])
isOverloaded x =
    let name = CID.ident x
    in do
        os <- Sys.getOverloads
        
        case Map.lookup name os of
            Nothing -> return   Nothing
            Just ss -> do
                ts <- M.mapM TS.instantiate ss
                return $ Just ts




