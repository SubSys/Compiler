{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ViewPatterns #-}
module LLIR.HelmLL.Core.Index.Syntax.TopLevel.Unions (
    traverseUnions
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
import LLIR.HelmLL.Core.Index.Data.System (enter)

-- + Local
import qualified LLIR.HelmLL.Core.Index.Data.System     as Sys
import qualified LLIR.HelmLL.Core.Index.Scope.Referable as Scope
import qualified LLIR.HelmLL.Core.Index.Scope.Utils     as Scope

-- ++ Sub-Indexers
import qualified LLIR.HelmLL.Core.Index.Syntax.Base.Etc       as Etc
import qualified LLIR.HelmLL.Core.Index.Syntax.Base.Type      as T
-- *


traverseUnions :: [Decl.Union] -> Sys.State [Decl.Union]
traverseUnions = M.mapM indexUnion


indexUnion :: Decl.Union -> Sys.State Decl.Union
indexUnion (Decl.Union name as cs) = do
    Sys.resetTypeCounter
    -- *
    
    -- *
    (as', subs) <- List.unzip <$> M.mapM T.typeBindable as
    cs' <- Scope.withLocalSubst (Map.unions subs) (M.mapM indexConstr cs)
    -- *
    
    -- *
    return
        (Decl.Union name as' cs')


indexConstr :: Decl.Constr -> Sys.State Decl.Constr
indexConstr (Decl.Constr ident args) = do
    args' <- M.mapM T.indexType args
    
    return
        (Decl.Constr ident args')
        




