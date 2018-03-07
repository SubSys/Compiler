{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.HelmLL.Core.TypeCheck.Syntax.TermLevel.Patterns (
    inferCaseAlts
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
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
import qualified Data.Data                    as Data
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP




-- + HelmLL Module Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fn
import qualified LLIR.HelmLL.AST.Utils.Generic.SudoFFI     as SudoFFI

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as T
import qualified LLIR.HelmLL.AST.Data.Base.Literals   as V

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as Decl

-- + Local Prelude
import LLIR.HelmLL.Core.TypeCheck.Inference.Syntax.Base (enter, binder)

-- + Local
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.System      as Sys
import qualified LLIR.HelmLL.Core.TypeCheck.Data.Report                as Report
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Data.Env         as Env
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Utils.TypeSystem as TS
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Syntax.Scope     as Scope
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Syntax.Constrain as Con
import qualified LLIR.HelmLL.Core.TypeCheck.Inference.Utils.General    as Util


-- ++ Sub Infers
import qualified LLIR.HelmLL.Core.TypeCheck.Syntax.Base.Values   as V
import qualified LLIR.HelmLL.Core.TypeCheck.Syntax.Base.Etc      as Etc
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



inferCaseAlt :: (S.Block -> Sys.Syntax S.Block)
             -> (T.Type, T.Type)
             -> P.CaseAlt
             -> Sys.Syntax P.CaseAlt
inferCaseAlt f (intro, outro) (P.CaseAlt p e) = do
    (p', pt, env) <- inferPattern p
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
inferCaseAlts :: (S.Block -> Sys.Syntax S.Block)
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

inferPattern (P.Lit lit) = do
    (lit', t, _) <- V.inferLit lit
    -- *
    
    -- *
    enter (P.Lit lit') t


inferPattern (P.List xs) = do
    tv <- TS.freshType
    (xs', ts, env)  <- Util.inferList inferPattern xs
    -- *
    
    -- *
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    binder (P.List xs') tv env


inferPattern (P.ListCons xs rest) = do
    tv              <- TS.freshType
    (xs', ts, env)  <- Util.inferList inferPattern xs
    (rest', t, env) <- Util.inferMaybe inferPattern tv rest
    -- *
    
    -- *
    Con.unify tv t
    M.mapM_ (Con.unify tv) ts
    -- *
    
    -- *
    binder (P.ListCons xs' rest') tv env



inferPattern (P.Tuple items) = do
    (items', ts, env) <- Util.inferList inferPattern items
    -- *
    
    -- *
    let t = T.Tuple ts
    -- *
    
    -- *
    binder (P.Tuple items') t env



inferPattern (P.Constr name args) = do
    t1 <- Scope.lookupEnv name
    ut <- extractUnionType t1
    (args', ts, env) <- Util.inferList inferPattern args
    -- *
    
    -- *
    let t2 = Fold.foldr T.Arr ut ts
    -- *
    
    -- *
    Con.unify t1 t2
    -- *
    
    
    -- * 
    binder (P.Constr name args') ut env
    ------
    -- ^ - Since ‘args’ param may contain binders


inferPattern (P.Var arg) = do
    env <- Scope.getTypes
    (tv, scheme) <- TS.freshTSPair
    let env' = Map.insert (ID.get arg) scheme env
    -- *
    
    -- *
    (arg', argTy, _) <- Etc.inferBinder arg
    Con.unify argTy tv
    -- *
    
    -- *
    binder (P.Var arg') tv env'


inferPattern P.Wildcard = do
    tv <- TS.freshType
    -- *
    
    -- *
    binder P.Wildcard tv Map.empty




-- *
-- | Internal Helpers
-- *

extractUnionType :: T.Type -> Sys.Infer T.Type
extractUnionType (T.Arr t1 t2) = extractUnionType t2
extractUnionType ut@T.Union{}    = return ut
extractUnionType x =
    error $ String.unlines
        [ "Internal Compiler Failure:"
        , "\tInfer Pattern Constructor: 'extractUnionType' failed to return a union."
        ]















