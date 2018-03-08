{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.HelmLL.AST.Tailored.GLSL.TermLevel.Stmt (
    pattern Vec2Float
  , pattern Vec3Float
  , pattern Vec4Float
  , pattern Vec2Int
  , pattern Vec3Int
  , pattern Vec4Int
  , pattern Vec2Bool
  , pattern Vec3Bool
  , pattern Vec4Bool
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



-- + Upstream IRs
import qualified SLIR.HelmSyntax.Pipeline as HelmSyntax
import qualified HLIR.HelmFlat.Pipeline   as HelmFlat

-- + HelmLL Syntax Renderer
import qualified LLIR.HelmLL.AST.Render.Syntax.Driver as Syntax

-- + HelmLL AST Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as T
import qualified LLIR.HelmLL.AST.Data.Base.Literals as Lit

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as Decl
-- *



pattern Vec2Float :: Double -> Double -> S.Stmt
pattern Vec2Float x1 x2 =
    S.Tuple [S.Lit (Lit.Float x1), S.Lit (Lit.Float x2)]


pattern Vec3Float :: Double -> Double -> Double -> S.Stmt
pattern Vec3Float x1 x2 x3 =
    S.Tuple [S.Lit (Lit.Float x1), S.Lit (Lit.Float x2), S.Lit (Lit.Float x3)]


pattern Vec4Float :: Double -> Double -> Double -> Double -> S.Stmt
pattern Vec4Float x1 x2 x3 x4 =
    S.Tuple [S.Lit (Lit.Float x1), S.Lit (Lit.Float x2), S.Lit (Lit.Float x3), S.Lit (Lit.Float x4)]



pattern Vec2Int :: Int -> Int -> S.Stmt
pattern Vec2Int x1 x2 =
    S.Tuple [S.Lit (Lit.Int x1), S.Lit (Lit.Int x2)]


pattern Vec3Int :: Int -> Int -> Int -> S.Stmt
pattern Vec3Int x1 x2 x3 =
    S.Tuple [S.Lit (Lit.Int x1), S.Lit (Lit.Int x2), S.Lit (Lit.Int x3)]


pattern Vec4Int :: Int -> Int -> Int -> Int -> S.Stmt
pattern Vec4Int x1 x2 x3 x4 =
    S.Tuple [S.Lit (Lit.Int x1), S.Lit (Lit.Int x2), S.Lit (Lit.Int x3), S.Lit (Lit.Int x4)]



pattern Vec2Bool :: Bool -> Bool -> S.Stmt
pattern Vec2Bool x1 x2 =
    S.Tuple [S.Lit (Lit.Bool x1), S.Lit (Lit.Bool x2)]


pattern Vec3Bool :: Bool -> Bool -> Bool -> S.Stmt
pattern Vec3Bool x1 x2 x3 =
    S.Tuple [S.Lit (Lit.Bool x1), S.Lit (Lit.Bool x2), S.Lit (Lit.Bool x3)]


pattern Vec4Bool :: Bool -> Bool -> Bool -> Bool -> S.Stmt
pattern Vec4Bool x1 x2 x3 x4 =
    S.Tuple [S.Lit (Lit.Bool x1), S.Lit (Lit.Bool x2), S.Lit (Lit.Bool x3), S.Lit (Lit.Bool x4)]



