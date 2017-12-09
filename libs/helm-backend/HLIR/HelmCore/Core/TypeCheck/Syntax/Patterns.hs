{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core.TypeCheck.Syntax.Patterns (
    inferCaseAlts
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
import qualified Data.String   as String

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
import HLIR.HelmCore.Core.TypeCheck.Syntax.Misc.Utils          (inferList, inferMaybe)

-- ~ Sub Infers
import qualified HLIR.HelmCore.Core.TypeCheck.Syntax.Base.Values as V
-- *


-- |
-- NOTE:
-- * Here, this function requires a special (2 component)
--   tuple of types, called intro and outro respectively.
--
--   * ‘Intro’ unifies with the inferred pattern type
--   * ’outro’ unifies with the inferred expression type, I.e the expression body of the case alt branch.
--
--   * All together, this will ensure that every branch is of the same type.
--
inferCaseAlt :: (E.Expr -> Sys.Syntax E.Expr)
             -> (T.Type, T.Type)
             -> P.CaseAlt
             -> Sys.Syntax P.CaseAlt
inferCaseAlt f (intro, outro) (P.CaseAlt p e) = do
    (p', pt, env) <- inferPattern f p
    (e', et, _)   <- Scope.withLocalEnv env (f e)
    -- *
    
    -- *
    Con.unify pt intro
    Con.unify et outro
    -- *
    
    -- *
    enter (P.CaseAlt p' e') et



-- |
-- NOTE: Ensure that the inferred type of the Case ‘condition expression’
--       is passed in, so that such can unify with the inferred pattern types.
--
inferCaseAlts :: (E.Expr -> Sys.Syntax E.Expr)
                 -> T.Type
                 -> [P.CaseAlt]
                 -> Sys.State ([P.CaseAlt], T.Type)

inferCaseAlts f conT ps = do
    intro <- TS.freshType
    outro <- TS.freshType
    -- *
    
    -- *
    (ps, ts, _) <- inferList (inferCaseAlt f (intro, outro)) ps
    -- *
    
    -- *
    Con.unify intro conT
    -- *
    
    -- *
    return (ps, outro)




-- *
-- | # Patterns
-- *

inferPattern :: (E.Expr -> Sys.Syntax E.Expr)
             -> P.Pattern
             -> Sys.Syntax P.Pattern

inferPattern f (P.Lit lit) = do
    (lit', t, _) <- V.inferLit lit
    -- *
    
    -- *
    enter (P.Lit lit') t


inferPattern f (P.Record vars) =
    error "TODO - Not yet supported: 'inferPattern Record'…"


inferPattern f (P.List xs) = do
    tv <- TS.freshType
    (xs', ts, env)  <- inferPatternList f xs
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    binder (P.List xs') tv env


inferPattern f (P.Cons xs rest) = do
    tv              <- TS.freshType
    (xs', ts, env)  <- inferPatternList f xs
    (rest', t, env) <- inferMaybe (inferPattern f) tv rest
    -- *
    
    -- *
    Con.unify tv t
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    binder (P.Cons xs' rest') tv env



inferPattern f (P.Tuple items) = do
    (items', ts, env) <- inferPatternList f items
    -- *
    
    -- *
    let t = T.Tuple ts
    -- *
    
    -- *
    binder (P.Tuple items') t env



inferPattern f (P.Con name args) = do
    t1 <- Scope.lookupConstr name
    ut <- extractUnionType t1
    (args', ts, env) <- inferPatternList f args
    -- *
    
    -- *
    let t2 = Fold.foldr T.Arr ut ts
    -- *
    
    -- *
    Con.unify t1 t2
    -- *
    
    
    -- * 
    binder (P.Con name args') ut env
    ------
    -- ^ - Since ‘args’ param may contain binders


inferPattern f (P.Var name) = do
    env <- M.ask
    (tv, scheme) <- TS.freshTSPair
    let env' = Env.extend env (CID.ident name, scheme)
    -- *
    
    -- *
    binder (P.Var name) tv env'


inferPattern f P.Wildcard = do
    tv <- TS.freshType
    -- *
    
    -- *
    binder P.Wildcard tv Env.empty




-- *
-- | Internal Helpers
-- *

extractUnionType :: T.Type -> Sys.State T.Type
extractUnionType (T.Arr t1 t2) = extractUnionType t2
extractUnionType ut@T.Union{}    = return ut
extractUnionType x =
    error $ String.unlines
        [ "Internal Compiler Failure:"
        , "\tInfer Pattern Constructor: 'extractUnionType' failed to return a union."
        ]


inferPatternList :: (E.Expr -> Sys.Syntax E.Expr)
              -> [P.Pattern]
              -> Sys.State ([P.Pattern], [T.Type], Env.Env)
inferPatternList f =
    inferList (inferPattern f)



