{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

-- | Helpers for `SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Syntax.Constrain`
--
module SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Syntax.Constrain.Overloaded (
    synthType
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


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni


-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope           as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident as ID
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Type  as T

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local Prelude
import SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Syntax.Base (enter)

-- + Local
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.System      as Sys
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Solver.Data.Constraint     as Con
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Data.Report                as Report
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Data.Env         as Env
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Subst.Types                as TySub
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Syntax.Scope     as Scope
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Inference.Utils.TypeSystem as TS
-- *




{-# ANN module ("HLint: ignore" :: String) #-}





-- | NOTE: (Currently) A superposed type cannot merely be unified with a type variable, it must have more structure.
-- 
-- 
-- As in, given a superposed type of
-- `Int -> Int -> Int/Float -> Float -> Float`,
-- we need to generate a polymorphic type of
-- `a -> a -> a` for the current unifier implementation. 
-- 
-- For example, this detail is fine for Infix applications,
-- I.e. unify ‘x’ with `t1 `T.Arr'` (t2 `T.Arr'` tvar)`, is
-- the default for any type. Yet for type variables, where
-- we usually, and simply lookup a type and emit such will
-- fail for superposed types. Such that, if a is a
-- superposed type, we must generate a type that mimics the
-- structure of the eventual unified result. (See `baseTypeRule`)






-- | Synthesize Superposed Type
--

synthType :: [T.Scheme] -> Sys.Infer T.Type
synthType ss = do
    ts <- M.mapM TS.instantiate ss
    
    (tv : tvs) <- M.replicateM (List.length ts + 1) TS.freshType
    
    
    (T.Var' con) <- TS.freshType
    
    
    t1 <- baseTypeRule ts
    
    M.tell [(t1, T.Superposed [con] ts)]
    
    
    return t1
    
    where
        extract :: T.Type -> ID.Ident
        extract (T.Var' ident) = ident

    

flattenTypes :: [T.Type] -> [[T.Type]]
flattenTypes = map T.flatten




baseTypeRule :: [T.Type] -> Sys.Infer T.Type
baseTypeRule xs@(flattenTypes -> ts)
    | not (null ts) && arityRule = do
        (tv:tvs) <- M.replicateM arity TS.freshType
        
        return $ Fold.foldr T.Arr' tv tvs
    
    
    | not (null ts) && not arityRule =
        M.throwError (Report.ConflictingOverloadedTypeArity xs)
    
    where
        arityRule = List.all ((==) arity) arities
        
        x = List.head ts
        (arity : arities) = map List.length ts






