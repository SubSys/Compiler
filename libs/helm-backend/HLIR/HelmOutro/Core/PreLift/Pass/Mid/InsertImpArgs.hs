{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
-- | Insert implicit arguments
-- Some form of closure conversion (I Think?).
--
module HLIR.HelmOutro.Core.PreLift.Pass.Mid.InsertImpArgs (
    insertImpArgs
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
import qualified HLIR.HelmOutro.Core.PreLift.Utils.Args           as Util
import qualified HLIR.HelmOutro.Core.PreLift.Utils.InitEnv        as Util
import qualified HLIR.HelmOutro.Core.PreLift.Utils.App            as Util
-- *




insertImpArgs :: Env.Env -> [Decl.Function] -> [Decl.Function]
insertImpArgs env = Uni.transformBi (declCC env)



declCC :: Env.Env -> Decl.Function -> Decl.Function
declCC env (Decl.Function name args expr scheme) =
    let expr' = exprCC env expr
    in
        Decl.Function name args expr' scheme


exprCC :: Env.Env -> E.Expr -> E.Expr
exprCC env =
    Uni.transform (exprCC' env)




exprCC' env (E.FunCall ref params)
    | Just ty <- Env.lookup (CID.ident ref) env
    , Util.getArity ty > List.length params =
        let
            (args, vars) = Util.genFreshAVPair (Util.getArity ty - List.length params)
        in
            Fold.foldr absTerm (E.FunCall ref $ params ++ vars) args
    
    where
        absTerm :: Etc.Arg -> E.Expr -> E.Expr
        absTerm arg =
            E.Abs [arg]



exprCC' env x = x



