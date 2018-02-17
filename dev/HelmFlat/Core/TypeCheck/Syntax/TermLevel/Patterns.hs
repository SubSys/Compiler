{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmFlat.Core.TypeCheck.Syntax.TermLevel.Patterns (
    inferCaseAlts
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun
import qualified Data.String                  as String


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Module.Data.Interface as I

-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Scope           as Scope
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Ident as ID

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local Prelude
import HLIR.HelmFlat.Core.TypeCheck.Inference.Syntax.Base (enter, binder)

-- + Local
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Data.System      as Sys
import qualified HLIR.HelmFlat.Core.TypeCheck.Data.Report                as Report
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Data.Env         as Env
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Utils.TypeSystem as TS
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Syntax.Scope     as Scope
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Syntax.Constrain as Con
import qualified HLIR.HelmFlat.Core.TypeCheck.Inference.Utils.General    as Util


-- ++ Sub Infers
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.Base.Values   as V
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.Base.Etc      as Etc
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.Base.Metadata as Meta
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
    meta' <- Meta.recordType et meta
    -- *
    
    -- *
    enter (P.CaseAlt p' e' meta') et



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
    (ps, ts, _) <- Util.inferList (inferCaseAlt f (intro, outro)) ps
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

inferPattern (P.Lit lit meta) = do
    (lit', t, _) <- V.inferLit lit
    -- *
    
    -- *
    meta' <- Meta.recordType t meta
    -- *
    
    -- *
    enter (P.Lit lit' meta') t


inferPattern (P.List xs meta) = do
    tv <- TS.freshType
    (xs', ts, env)  <- Util.inferList inferPattern xs
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    meta' <- Meta.recordType tv meta
    -- *
    
    -- *
    binder (P.List xs' meta') tv env


inferPattern (P.ListCons xs rest meta) = do
    tv              <- TS.freshType
    (xs', ts, env)  <- Util.inferList inferPattern xs
    (rest', t, env) <- Util.inferMaybe inferPattern tv rest
    -- *
    
    -- *
    Con.unify tv t
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    meta' <- Meta.recordType tv meta
    -- *
    
    -- *
    binder (P.ListCons xs' rest' meta') tv env



inferPattern (P.Tuple items meta) = do
    (items', ts, env) <- Util.inferList inferPattern items
    -- *
    
    -- *
    let t = T.Tuple' ts
    -- *
    
    -- *
    meta' <- Meta.recordType t meta
    -- *
    
    -- *
    binder (P.Tuple items' meta') t env



inferPattern (P.Constr name args meta) = do
    t1 <- Scope.lookupEnv name
    ut <- extractUnionType t1
    (args', ts, env) <- Util.inferList inferPattern args
    -- *
    
    -- *
    let t2 = Fold.foldr T.Arr' ut ts
    -- *
    
    -- *
    Con.unify t1 t2
    -- *
    
    -- *
    meta' <- Meta.recordType ut meta
    -- *
    
    
    -- * 
    binder (P.Constr name args' meta') ut env
    ------
    -- ^ - Since ‘args’ param may contain binders


inferPattern (P.Var arg meta) = do
    env <- Scope.getTypes
    (tv, scheme) <- TS.freshTSPair
    let env' = Map.insert (ID.get arg) scheme env
    -- *
    
    -- *
    (arg', argTy, _) <- Etc.inferBinder arg
    Con.unify argTy tv
    -- *
    
    -- *
    meta' <- Meta.recordType tv meta
    -- *
    
    -- *
    binder (P.Var arg' meta') tv env'


inferPattern (P.Wildcard meta) = do
    tv <- TS.freshType
    -- *
    
    -- *
    binder (P.Wildcard meta) tv Map.empty




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















