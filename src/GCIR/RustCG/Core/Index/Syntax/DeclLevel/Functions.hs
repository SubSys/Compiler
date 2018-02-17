{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module GCIR.RustCG.Core.Index.Syntax.DeclLevel.Functions (
    traverseDecls
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
import qualified GCIR.RustCG.Data.Interface as I

-- + RustCG AST Utils
import qualified GCIR.RustCG.AST.Utils.Functions       as Decl
import qualified GCIR.RustCG.AST.Utils.Generic.SudoFFI as SudoFFI

-- + RustCG AST
-- ++ Base
import qualified GCIR.RustCG.AST.Data.Semantic.Base.Ident                 as ID
import qualified GCIR.RustCG.AST.Data.Semantic.Base.Literals              as Lit
import qualified GCIR.RustCG.AST.Data.Semantic.Base.Types                 as T
import qualified GCIR.RustCG.AST.Data.Semantic.Base.Etc                   as Etc
-- ++ Block Level
import qualified GCIR.RustCG.AST.Data.Semantic.BlockLevel.Stmt            as S
import qualified GCIR.RustCG.AST.Data.Semantic.BlockLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified GCIR.RustCG.AST.Data.Semantic.DeclLevel.Enums.Variants   as Decl
import qualified GCIR.RustCG.AST.Data.Semantic.DeclLevel.Enums            as Decl
import qualified GCIR.RustCG.AST.Data.Semantic.DeclLevel.Functions        as Decl

-- + Local Prelude
import GCIR.RustCG.Core.Index.Data.System (enter, binder)

-- + Local
import qualified GCIR.RustCG.Core.Index.Data.System            as Sys
import qualified GCIR.RustCG.Core.Index.Scope.Bindable         as Scope
import qualified GCIR.RustCG.Core.Index.Scope.Referable        as Scope
import qualified GCIR.RustCG.Core.Index.Scope.Utils            as Scope
import qualified GCIR.RustCG.Core.Index.Syntax.BlockLevel.Stmt as S
import qualified GCIR.RustCG.Core.Index.Syntax.Base.Types      as T
import qualified GCIR.RustCG.Core.Index.Scope.Generic          as Scope
-- *


{-# ANN module ("HLint: ignore" :: String) #-}


traverseDecls :: [Decl.Function] -> Sys.Index [Decl.Function]
traverseDecls []  = return ([], Map.empty)

traverseDecls (fn:fns) = do
    (fn', s1) <- indexFunction fn
    (fns', s2) <- Scope.withLocalSubst s1 (traverseDecls fns)
    
    return
        (fn' : fns', Map.union s2 s1)




indexFunction :: Decl.Function -> Sys.Index Decl.Function
indexFunction (Decl.isRecFunction' -> (Just (Decl.Function name gs args out body))) = do
    (name', s1) <- Scope.bindable name
    (gs',   gs) <- List.unzip <$> M.mapM indexGeneric gs
    (args', ss) <- List.unzip <$> M.mapM (indexInput (Map.unions gs)) args
    
    (body', _) <- Scope.withLocalSubst (Map.union s1 (Map.unions ss)) (S.indexBlock body)
    
    (out', _) <- indexOutput (Map.unions gs) out
    
    binder
        (Decl.Function name' gs' args' out' body')
        s1
    
indexFunction (Decl.Function name gs args out body) = do
    (name', s1) <- Scope.bindable name
    (gs',   gs) <- List.unzip <$> M.mapM indexGeneric gs
    (args', ss) <- List.unzip <$> M.mapM (indexInput (Map.unions gs)) args
    
    
    (body', _) <- Scope.withLocalSubst (Map.unions ss) (S.indexBlock body)
    
    (out', _) <- indexOutput (Map.unions gs) out
    
    binder
        (Decl.Function name' gs' args' out' body')
        s1



indexGeneric :: Etc.Generic -> Sys.Index Etc.Generic
indexGeneric gen = do
    (gen', subs) <- Scope.genericDecl gen
    
    binder gen' subs


indexInput :: Sys.Subst -> Etc.Input -> Sys.Index Etc.Input
indexInput gSubs (Etc.Input ident ty) = do
    (ident', subs) <- Scope.bindable ident
    (ty', _) <- Scope.withLocalSubst gSubs (T.indexType ty)
    
    binder
        (Etc.Input ident' ty')
        subs


indexOutput :: Sys.Subst -> Etc.Output -> Sys.Index Etc.Output
indexOutput gSubs (Etc.Output ty) = do
    (ty', _) <- Scope.withLocalSubst gSubs (T.indexType ty)
    
    enter (Etc.Output ty')


