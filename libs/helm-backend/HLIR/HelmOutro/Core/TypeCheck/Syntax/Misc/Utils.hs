{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.TypeCheck.Syntax.Misc.Utils (
    inferList
  , inferMaybe
  , inferArgs
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

-- ~ Sub Infers
import qualified HLIR.HelmOutro.Core.TypeCheck.Syntax.Base.Values as V
import qualified HLIR.HelmOutro.Core.TypeCheck.Syntax.Base.Etc    as Etc
-- *






inferList :: (a -> Sys.Syntax a) -> [a] -> Sys.State ([a], [T.Type], Env.Env)
inferList f []     = do
    env <- M.ask
    
    return ([], [], env)


inferList f (x:xs) = do
    (x', t, e) <- f x
    (xs', ts, env') <- Scope.withLocalEnv e (inferList f xs)
    
    return (x' : xs', t : ts, env')


inferMaybe :: (a -> Sys.Syntax a) -> T.Type -> Maybe a -> Sys.Syntax (Maybe a)
inferMaybe f defaultType Nothing = enter Nothing defaultType
inferMaybe f defaultType (Just x) = do
    (x', t, e) <- f x

    return (Just x', t, e)





inferArgs :: [Etc.Arg] -> Sys.State ([Etc.Arg], [T.Type], Env.Env)
inferArgs = inferList Etc.inferArg















