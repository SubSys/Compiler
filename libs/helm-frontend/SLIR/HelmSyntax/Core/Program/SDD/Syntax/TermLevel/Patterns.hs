{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Core.Program.SDD.Syntax.TermLevel.Patterns (
    inferCaseAlts
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid
import qualified Data.String   as String

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Auxiliary Nodes
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID


--- Local Prelude
import SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Base (enter, binder)


--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env               as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                      as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.System            as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Scope     as Scope
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem       as TS
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Syntax.Constrain as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.Auxiliary        as Aux

-- ~ Sub Inferers
import qualified SLIR.HelmSyntax.Core.Program.SDD.Syntax.Base.Values as V
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
inferCaseAlt f (intro, outro) (P.CaseAlt p e meta) = do
    (p', pt, env) <- inferPattern p
    (e', et, _)   <- Scope.withLocalEnv env (f e)
    -- *
    
    -- *
    Con.unify pt intro
    Con.unify et outro
    -- *
    
    -- *
    enter (P.CaseAlt p' e' meta) et



-- |
-- NOTE: Ensure that the inferred type of the Case ‘condition expression’
--       is passed in, so that such can unify with the inferred pattern types.
--
inferCaseAlts :: (E.Expr -> Sys.Syntax E.Expr)
                 -> T.Type
                 -> [P.CaseAlt]
                 -> Sys.Infer ([P.CaseAlt], T.Type)

inferCaseAlts f conT ps = do
    intro <- TS.freshType
    outro <- TS.freshType
    -- *
    
    -- *
    (ps, ts, _) <- Aux.inferList (inferCaseAlt f (intro, outro)) ps
    -- *
    
    -- *
    Con.unify intro conT
    -- *
    
    -- *
    return (ps, outro)





-- *
-- | # Patterns
-- *

inferPattern :: P.Pattern
             -> Sys.Syntax P.Pattern

inferPattern (P.Lit lit metaOpt) = do
    (lit', t, _) <- V.inferLit lit
    -- *
    
    -- *
    enter (P.Lit lit' metaOpt) t


inferPattern (P.Record vars metaOpt) =
    error "TODO - Not yet supported: 'inferPattern Record'…"


inferPattern (P.List xs metaOpt) = do
    tv <- TS.freshType
    (xs', ts, env)  <- Aux.inferList inferPattern xs
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    binder (P.List xs' metaOpt) tv env


inferPattern (P.Cons xs rest metaOpt) = do
    tv              <- TS.freshType
    (xs', ts, env)  <- Aux.inferList inferPattern xs
    (rest', t, env) <- Aux.inferMaybe inferPattern tv rest
    -- *
    
    -- *
    Con.unify tv t
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    binder (P.Cons xs' rest' metaOpt) tv env



inferPattern (P.Tuple items metaOpt) = do
    (items', ts, env) <- Aux.inferList inferPattern items
    -- *
    
    -- *
    let t = T.Tuple' ts
    -- *
    
    -- *
    binder (P.Tuple items' metaOpt) t env



inferPattern (P.Con name args metaOpt) = do
    t1 <- Scope.lookupEnv' name
    ut <- extractUnionType t1
    (args', ts, env) <- Aux.inferList inferPattern args
    -- *
    
    -- *
    let t2 = Fold.foldr T.Arr' ut ts
    -- *
    
    -- *
    Con.unify t1 t2
    -- *
    
    
    -- * 
    binder (P.Con name args' metaOpt) ut env
    ------
    -- ^ - Since ‘args’ param may contain binders


inferPattern (P.Var name metaOpt) = do
    env <- M.ask
    (tv, scheme) <- TS.freshTSPair
    let env' = Map.insert (CID.ident name) (Env.Normal scheme) env
    -- *
    
    -- *
    binder (P.Var name metaOpt) tv env'


inferPattern (P.Wildcard metaOpt) = do
    tv <- TS.freshType
    -- *
    
    -- *
    binder (P.Wildcard metaOpt) tv Map.empty




-- *
-- | Internal Helpers
-- *

extractUnionType :: T.Type -> Sys.Infer T.Type
extractUnionType (T.Arr t1 t2 _) = extractUnionType t2
extractUnionType ut@T.Union{}    = return ut
extractUnionType x =
    error $ String.unlines
        [ "Internal Compiler Failure:"
        , "\tInfer Pattern Constructor: 'extractUnionType' failed to return a union."
        ]






