{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
module HLIR.HelmFlat.Feed.RustCG.Init.Variants (
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

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + HelmFlat AST Interface
import qualified HLIR.HelmFlat.Data.Interface as I

-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Types                    as Type
import qualified HLIR.HelmFlat.AST.Utils.Generic.SudoFFI          as SudoFFI
import qualified HLIR.HelmFlat.AST.Utils.Generic.TypesEnv         as TyEnv
import qualified HLIR.HelmFlat.AST.Utils.Generic.TypesEnv.Helpers as TyEnv
import qualified HLIR.HelmFlat.AST.Utils.Unions                   as Union

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc           as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident         as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types         as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values        as V
-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P
-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl
-- *



setVariantPaths :: (Data.Data a, Data.Typeable a) => [Decl.Union] -> a -> a
setVariantPaths uns input =
    input   |> Uni.transformBi (exprConstrs uns)
            |> Uni.transformBi (patrnConstrs uns)


-- | Internal Helpers
--

exprConstrs :: [Decl.Union] -> E.Expr -> E.Expr
exprConstrs uns (E.Constr ident)
    | Just (unionName2NS -> unionName) <- Union.lookupUnionName ident uns =
        E.Constr $ updateNamespace unionName ident

exprConstrs uns (E.ConCall ident args)
    | Just (unionName2NS -> unionName) <- Union.lookupUnionName ident uns =
        E.ConCall (updateNamespace unionName ident) args


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
    
