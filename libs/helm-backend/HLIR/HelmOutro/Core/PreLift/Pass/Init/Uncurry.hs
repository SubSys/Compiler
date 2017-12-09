{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module HLIR.HelmOutro.Core.PreLift.Pass.Init.Uncurry (
    uncurryFuns
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


-- |
-- FYI:
-- * CC = Conversion.



uncurryFuns :: [Decl.Function] -> [Decl.Function]
uncurryFuns = Uni.transformBi declCC



declCC :: Decl.Function -> Decl.Function
declCC (Decl.Function name args expr scheme) =
    let expr' = exprCC expr
    in
        Decl.Function name args expr' scheme



exprCC :: E.Expr -> E.Expr
exprCC =
    Uni.transform f
    where
        f (E.App e1@(getCallee -> Just (Right ref)) e2) =
            E.FunCall ref $ getParams e1 e2


        f (E.App e1@(getCallee -> Just (Left con)) e2) =
            E.ConCall con $ getParams e1 e2
        
        -- | Uncurry sub call...
        --
        f (E.App (E.FunCall ref params) e2) =
            E.FunCall ref $ params ++ [e2]
        
        f (E.App (E.ConCall con params) e2) =
            E.ConCall con $ params ++ [e2]
        
        f x = x



-- *
-- | Internal Utils
-- *



getCallee :: E.Expr -> Maybe (Either ID.Big ID.Ref)
getCallee (E.App e1 _) = getCallee e1

getCallee (E.Var ref) = Just $ Right ref
getCallee (E.Con con) = Just $ Left con

getCallee _ = Nothing


uncurrySubCall :: E.Expr -> Maybe E.Expr
uncurrySubCall (E.FunCall ref expr) = Just (E.FunCall ref expr)
uncurrySubCall _ = Nothing

getParams :: E.Expr -> E.Expr -> [E.Expr]
getParams e1 e2
    | (_:xs) <- Util.flattenApps (E.App e1 e2) =
        xs







