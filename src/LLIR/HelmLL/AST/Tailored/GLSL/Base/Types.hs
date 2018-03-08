{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.HelmLL.AST.Tailored.GLSL.Base.Types (
    pattern Void
  , pattern Vec2Float
  , pattern Vec3Float
  , pattern Vec4Float
  , pattern Vec2Bool
  , pattern Vec3Bool
  , pattern Vec4Bool
  , pattern Vec2Int
  , pattern Vec3Int
  , pattern Vec4Int
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



pattern Void :: T.Type
pattern Void = T.Union (ID.Ident' "Void") []


pattern Vec2Float :: T.Type
pattern Vec2Float = T.Tuple [T.Float, T.Float]

pattern Vec3Float :: T.Type
pattern Vec3Float = T.Tuple [T.Float, T.Float, T.Float]

pattern Vec4Float :: T.Type
pattern Vec4Float = T.Tuple [T.Float, T.Float, T.Float, T.Float]


pattern Vec2Bool :: T.Type
pattern Vec2Bool = T.Tuple [T.Bool, T.Bool]

pattern Vec3Bool :: T.Type
pattern Vec3Bool = T.Tuple [T.Bool, T.Bool, T.Bool]

pattern Vec4Bool :: T.Type
pattern Vec4Bool = T.Tuple [T.Bool, T.Bool, T.Bool, T.Bool]


pattern Vec2Int :: T.Type
pattern Vec2Int = T.Tuple [T.Int, T.Int]

pattern Vec3Int :: T.Type
pattern Vec3Int = T.Tuple [T.Int, T.Int, T.Int]

pattern Vec4Int :: T.Type
pattern Vec4Int = T.Tuple [T.Int, T.Int, T.Int, T.Int]





