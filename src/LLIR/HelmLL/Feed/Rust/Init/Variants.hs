{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module LLIR.HelmLL.Feed.Rust.Init.Variants (
    setVariantPaths
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
import qualified Data.Data                    as Data
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP





-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Type      as Ty
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fn
import qualified LLIR.HelmLL.AST.Utils.Generic.SudoFFI     as SudoFFI
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Unions    as Union

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc           as Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident         as ID
import qualified LLIR.HelmLL.AST.Data.Base.Types         as T
import qualified LLIR.HelmLL.AST.Data.Base.Literals      as Lit

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as Decl

-- + Local
-- *





setVariantPaths :: (Data.Data a, Data.Typeable a) => [Decl.Union] -> a -> a
setVariantPaths uns input =
    input   |> Uni.transformBi (exprConstrs uns)
            |> Uni.transformBi (patrnConstrs uns)


-- | Internal Helpers
--

exprConstrs :: [Decl.Union] -> S.Stmt -> S.Stmt
exprConstrs uns (S.ConCall ident [])
    | Just (unionName2NS -> unionName) <- Union.lookupUnionName ident uns =
        S.ConCall (updateNamespace unionName ident) []

exprConstrs uns (S.ConCall ident args)
    | Just (unionName2NS -> unionName) <- Union.lookupUnionName ident uns =
        S.ConCall (updateNamespace unionName ident) args


exprConstrs _ e = e


patrnConstrs :: [Decl.Union] -> P.Pattern -> P.Pattern
patrnConstrs uns (P.Constr ident args)
    | Just (unionName2NS -> unionName) <- Union.lookupUnionName ident uns =
        P.Constr (updateNamespace unionName ident) args


patrnConstrs _ p = p




unionName2NS :: ID.Ident -> ID.Namespace
unionName2NS (ID.Ident txt Nothing) =
    ID.Namespace [txt]

unionName2NS (ID.Ident txt (Just (ID.Namespace segs))) =
    ID.Namespace (segs ++ [txt])

updateNamespace :: ID.Namespace -> ID.Ident -> ID.Ident
updateNamespace (ID.Namespace segs2) (ID.Ident txt (Just (ID.Namespace segs1))) =
    ID.Ident txt (Just $ ID.Namespace (segs1 ++ segs2))

updateNamespace ns (ID.Ident txt Nothing) =
    ID.Ident txt (Just ns)