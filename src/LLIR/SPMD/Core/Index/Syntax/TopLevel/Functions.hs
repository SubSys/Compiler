{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module LLIR.SPMD.Core.Index.Syntax.TopLevel.Functions (
    traverseDecls
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

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + SPMD AST Interface
import qualified LLIR.SPMD.Data.Interface as I

-- + SPMD AST Utils
import qualified LLIR.SPMD.AST.Utils.Auxiliary.Functions as Fun

-- + SPMD AST
-- ++ Base
import qualified LLIR.SPMD.AST.Data.Base.Ident                 as ID
import qualified LLIR.SPMD.AST.Data.Base.Literals              as Lit
import qualified LLIR.SPMD.AST.Data.Base.Types                 as T
import qualified LLIR.SPMD.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified LLIR.SPMD.AST.Data.BlockLevel.Stmt            as S
-- ++ Decl/Top Level
import qualified LLIR.SPMD.AST.Data.TopLevel.Functions         as Decl
import qualified LLIR.SPMD.AST.Data.TopLevel.Globals           as Decl

-- + Local Prelude
import LLIR.SPMD.Core.Index.Data.System (enter)

-- + Local
import qualified LLIR.SPMD.Core.Index.Data.System            as Sys
import qualified LLIR.SPMD.Core.Index.Scope.Bindable         as Scope
import qualified LLIR.SPMD.Core.Index.Scope.Referable        as Scope
import qualified LLIR.SPMD.Core.Index.Scope.Utils            as Scope
import qualified LLIR.SPMD.Core.Index.Syntax.BlockLevel.Stmt as S
-- *





{-# ANN module ("HLint: ignore" :: String) #-}


traverseDecls :: [Decl.Function] -> Sys.Index [Decl.Function]
traverseDecls = Scope.indexList indexFunction



indexFunction :: Decl.Function -> Sys.Index Decl.Function
indexFunction (Fun.isRecFunction -> True) = error "Support recursive functions at this stage?"
    
indexFunction (Decl.Function out name args body) = do
    (name', s1) <- Scope.bindable name
    (args', ss) <- List.unzip <$> M.mapM indexInput args
    
    (body', _) <- Scope.withLocalSubst (Map.unions ss) (S.indexBlock body)
    
    enter
        (Decl.Function out name' args' body')
        s1


indexInput :: Etc.Input -> Sys.Index Etc.Input
indexInput (Etc.Input qual ty ident) = do
    (ident', subs) <- Scope.bindable ident
    
    enter
        (Etc.Input qual ty ident')
        subs




