{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmIntro.Core.TypeCheck.Syntax.Misc.Utils (
    inferList
  , inferMaybe
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
import HLIR.HelmIntro.Core.TypeCheck.Data.System.Syntax.Helpers (enter)

-- ~ Sub Infers
import qualified HLIR.HelmIntro.Core.TypeCheck.Syntax.Base.Values as V
-- *






inferList :: (a -> Sys.Syntax a) -> [a] -> Sys.State ([a], [T.Type], TI.Env)
inferList f []     = do
    env <- Sys.getEnv
    
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



















