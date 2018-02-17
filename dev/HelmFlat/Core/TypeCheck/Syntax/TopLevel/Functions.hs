{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmFlat.Core.TypeCheck.Syntax.TopLevel.Functions (
    inferDecl
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

-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Module.Data.Interface as I

-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Scope                         as Scope
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Ident               as ID
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Functions.SudoFFI   as SudoFFI
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Functions.Recursive as Rec
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Type                as T

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
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.TermLevel.Expr as E
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.Base.Etc       as Etc
import qualified HLIR.HelmFlat.Core.TypeCheck.Syntax.Base.Metadata  as Meta
-- *












{-# ANN module "HLint: ignore" #-}





inferDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferDecl fn@(SudoFFI.ifSudoDeclGetTy -> (Just (Left  ty))) = do
    initialEnv <- Scope.getTypes
    -- *

    -- *
    let scheme = TS.closeOver ty
    t         <- TS.instantiate scheme
    -- *

    -- *
    let newEnv = Map.insert (ID.get fn) scheme initialEnv
    -- *


    -- *
    binder fn t initialEnv


inferDecl fn@(SudoFFI.ifSudoDeclGetTy -> (Just (Right  scheme))) = do
    initialEnv <- Scope.getTypes
    -- *

    -- *
    t  <- TS.instantiate $ TS.normalize scheme
    -- *

    -- *
    let newEnv = Map.insert (ID.get fn) (TS.normalize scheme) initialEnv
    -- *


    -- *
    binder fn t initialEnv


inferDecl fn@(Rec.isRecDecl -> True) = inferRecDecl fn



-- | Default branch
--
inferDecl (Decl.Function name args expr sig meta) = do
    initialEnv <- Scope.getTypes
    (tv, scheme) <- TS.freshTSPair
    -- *

    -- * Infer Expr
    (args', ts, exprEnv) <- Util.inferList Etc.inferBinder args
    (expr', et, _)       <- Scope.withLocalEnv exprEnv (E.inferExpr inferDecl expr)
    let t1 = Fold.foldr T.Arr' et ts
    -- *

    -- *
    Con.unify tv t1
    Con.unifySignature t1 sig
    -- *

    -- *
    let newEnv = Map.insert (ID.get name) (TS.normalize scheme) initialEnv
    -- *
    
    
    -- *
    -- (name', nameTy, _) <- Etc.inferBinder name
    -- Con.unify nameTy tv
    -- *
    
    -- *
    meta' <- Meta.recordType tv meta
    -- *


    -- *
    binder (Decl.Function name args' expr' sig meta') tv newEnv




-- *
-- | Internal Helpers
-- *


inferRecDecl :: Decl.Function -> Sys.Syntax Decl.Function
inferRecDecl (Decl.Function name args expr sig meta) = do
    initialEnv <- Scope.getTypes
    -- *
    
    -- *
    (name' : args', ts, env) <- Util.inferList Etc.inferBinder (name : args)
    (expr', et, _)   <- Scope.withLocalEnv env (E.inferExpr inferDecl expr)
    -- *
    
    -- *
    let (T.Arr' t1 t2) = Fold.foldr T.Arr' et ts
    -- *
    
    -- *
    tv0 <- TS.freshType
    -- *
    
    -- *
    Con.unify t1 t2
    Con.unify (tv0 `T.Arr'` tv0) t2
    -- *
    
    
    -- *
    (returnType, scheme) <- TS.freshTSPair
    Con.unify t1 returnType
    -- *
    
    -- *
    let newEnv = Map.insert (ID.get name) scheme initialEnv
    -- *
    
    -- *
    Con.unifySignature t1 sig
    -- *
    
    
    -- *
    -- let (name_ : args_) = args'
    -- let (nameTy_ : argsTs_) = ts
    -- Con.unify nameTy_ returnType
    -- *
    
    -- *
    meta' <- Meta.recordType returnType meta
    -- *
    
    -- *
    binder (Decl.Function name' args' expr' sig meta') returnType newEnv


























