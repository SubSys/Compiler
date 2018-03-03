{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.RustCG.Core.Index.Syntax.BlockLevel.Patterns (
    indexArm
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Prelude
    (return
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

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + RustCG AST Interface
import qualified CGIR.RustCG.Data.Interface as I

-- + RustCG AST
-- ++ Base
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Ident                 as ID
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Literals              as Lit
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Types                 as T
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.RustCG.AST.Data.Semantic.BlockLevel.Stmt            as S
import qualified CGIR.RustCG.AST.Data.Semantic.BlockLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified CGIR.RustCG.AST.Data.Semantic.DeclLevel.Enums.Variants   as Decl
import qualified CGIR.RustCG.AST.Data.Semantic.DeclLevel.Enums            as Decl
import qualified CGIR.RustCG.AST.Data.Semantic.DeclLevel.Functions        as Decl

-- + Local Prelude
import CGIR.RustCG.Core.Index.Data.System (enter, binder)

-- + Local
import qualified CGIR.RustCG.Core.Index.Data.System     as Sys
import qualified CGIR.RustCG.Core.Index.Scope.Bindable  as Scope
import qualified CGIR.RustCG.Core.Index.Scope.Referable as Scope
import qualified CGIR.RustCG.Core.Index.Scope.Utils     as Scope
-- *



indexArm :: (S.Stmt -> Sys.Index S.Stmt) -> P.Arm -> Sys.Index P.Arm
indexArm f (P.Arm patrn stmts) = do
    (patrn', subs) <- indexPattern patrn
    (stmts', _)    <- Scope.withLocalSubst subs (f stmts)
    
    enter (P.Arm patrn' stmts')

indexPattern :: P.Pattern -> Sys.Index P.Pattern
indexPattern (P.Var ident) = do
    (ident', subs) <- Scope.bindable ident
    
    binder
        (P.Var ident')
        subs
    
indexPattern (P.Lit val) =
    binder
        (P.Lit val)
        Map.empty

indexPattern (P.List xs) = do
    (xs', subs) <- List.unzip <$> M.mapM indexPattern xs
    
    binder
        (P.List xs')
        (Map.unions subs)

indexPattern (P.ListCons xs (Just rest)) = do
    (xs', subs1) <- List.unzip <$> M.mapM indexPattern xs
    (rest', subs2) <- indexPattern rest
    
    binder
        (P.ListCons xs' (Just rest'))
        (Map.union (Map.unions subs1) subs2)

indexPattern (P.ListCons xs Nothing) = do
    (xs', subs) <- List.unzip <$> M.mapM indexPattern xs
    
    binder
        (P.ListCons xs' Nothing)
        (Map.unions subs)

indexPattern (P.Tuple items) = do
    (items', subs) <- List.unzip <$> M.mapM indexPattern items
    
    binder
        (P.Tuple items')
        (Map.unions subs)

indexPattern (P.Variant path args) = do
    (args', subs) <- List.unzip <$> M.mapM indexPattern args
    
    binder
        (P.Variant path args')
        (Map.unions subs)


indexPattern P.Wildcard =
    binder P.Wildcard Map.empty


