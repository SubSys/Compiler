{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmOutro.Core.PreLift.Utils.InitEnv (
    initEnv
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

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
import qualified Text.Show.Prettyprint as PP


--- Local Deps
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
import qualified HLIR.HelmOutro.Core.PreLift.Data.Canonical.Ident as CID
import qualified HLIR.HelmOutro.Core.PreLift.Data.Env             as Env
-- *



initEnv :: [Decl.Function] -> Env.Env
initEnv = ingestDecls



-- *
-- | Internal Helpers
-- *

ingestDecls :: [Decl.Function] -> Env.Env
ingestDecls xs =
    let env1 = Env.mergeEnvs [env | (ingestDecl -> env) <- Uni.universeBi xs]
        env2 = Env.mergeEnvs [env | (ingestArg -> env) <- Uni.universeBi xs]
    in
        Env.merge env1 env2


ingestDecl :: Decl.Function -> Env.Env
ingestDecl (Decl.Function name args expr (Just (T.Forall _ ty))) =
    Env.singleton (CID.ident name) ty

ingestDecl x = error errorMsg


ingestArg :: Etc.Arg -> Env.Env
ingestArg (Etc.Arg name (Just ty)) =
    Env.singleton (CID.ident name) ty

ingestArg x = error errorMsg


errorMsg =
    "Internal compiler failure. Prelift expects all binders to be annotated with it’s type!"

