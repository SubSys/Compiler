{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.HelmLL.Core.Index.Syntax.TermLevel.Patterns (
    indexCaseAlt
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
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fun
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

-- + Sub-Indexers
import qualified LLIR.HelmLL.Core.Index.Syntax.Base.Etc as Etc
-- *




indexCaseAlt :: (S.Block -> Sys.Index S.Block) -> P.CaseAlt -> Sys.Index P.CaseAlt
indexCaseAlt f (P.CaseAlt patrn expr) = do
    (patrn', subs) <- indexPattern patrn
    (expr', _) <- Scope.withLocalSubst subs (f expr)

    enter
        (P.CaseAlt patrn' expr')
        Map.empty


indexPattern :: P.Pattern -> Sys.Index P.Pattern
indexPattern P.Wildcard  = enter P.Wildcard Map.empty
indexPattern (P.Lit lit) = enter (P.Lit lit) Map.empty

indexPattern (P.List xs) = do
    (xs', subs) <- List.unzip <$> M.mapM indexPattern xs
    
    enter
        (P.List xs')
        (Map.unions subs)


indexPattern (P.ListCons xs end) = do
    (xs', subs1) <- List.unzip <$> M.mapM indexPattern xs
    (end', subs2) <- Scope.indexMaybe indexPattern end

    enter
        (P.ListCons xs' end')
        (Map.unions subs1 `Map.union` subs2)

indexPattern (P.Tuple items) = do
    (items', subs) <- List.unzip <$> M.mapM indexPattern items
    
    enter
        (P.Tuple items')
        (Map.unions subs)

indexPattern (P.Constr ident args) = do
    (args', subs) <- List.unzip <$> M.mapM indexPattern args
    
    enter
        (P.Constr ident args')
        (Map.unions subs)

indexPattern (P.Var ident) = do
    (ident', subs) <- Etc.indexBinder ident
    
    enter
        (P.Var ident')
        subs



