{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ViewPatterns #-}
module LLIR.HelmLL.Feed.GLSL.Syntax.Base.Types (
    dropType
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




-- + HelmLL AST Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as       LL.Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as       LL.ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as       LL.T
import qualified LLIR.HelmLL.AST.Data.Base.Literals as       LL.Lit

import qualified LLIR.HelmLL.AST.Tailored.GLSL.Base.Types as LL.T

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as LL.E
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as LL.P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as LL.Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as LL.Decl

-- + GLSL AST
-- ++ Base
import qualified CGIR.GLSL.AST.Data.Base.Ident                 as CG.ID
import qualified CGIR.GLSL.AST.Data.Base.Literals              as CG.Lit
import qualified CGIR.GLSL.AST.Data.Base.Types                 as CG.T
import qualified CGIR.GLSL.AST.Data.Base.Etc                   as CG.Etc
-- ++ Block Level
import qualified CGIR.GLSL.AST.Data.TermLevel.Stmt             as CG.S
-- ++ Decl/Top Level
import qualified CGIR.GLSL.AST.Data.TopLevel.Functions         as CG.Decl
import qualified CGIR.GLSL.AST.Data.TopLevel.Globals           as CG.Decl


-- + Local
import qualified LLIR.HelmLL.Feed.GLSL.Utils.Error       as Error
import qualified LLIR.HelmLL.Feed.GLSL.Syntax.Base.Ident as ID
-- *



-- | Drop Types
--
dropType :: LL.T.Type -> CG.T.Type
dropType LL.T.Void = CG.T.Void

dropType LL.T.Vec2Float = CG.T.Vec2
dropType LL.T.Vec3Float = CG.T.Vec3
dropType LL.T.Vec4Float = CG.T.Vec4

dropType LL.T.Vec2Bool = CG.T.BVec2
dropType LL.T.Vec3Bool = CG.T.BVec3
dropType LL.T.Vec4Bool = CG.T.BVec4

dropType LL.T.Vec2Int = CG.T.IVec2
dropType LL.T.Vec3Int = CG.T.IVec3
dropType LL.T.Vec4Int = CG.T.IVec4

dropType LL.T.Int    = CG.T.Int
dropType LL.T.Float  = CG.T.Float
dropType LL.T.Bool   = CG.T.Bool

dropType (LL.T.List ty)         = error "Not yet implemented"
dropType (LL.T.Tuple ts)        = error "Not yet implemented"
dropType (LL.T.Union name args) = error "Not yet implemented"

dropType LL.T.String    = Error.unsupported "String"
dropType LL.T.Char      = Error.unsupported "Char"
dropType (LL.T.Var id') = Error.unsupported "Type variables. Internal error, should have been removed by now!"
dropType ty@LL.T.Arr{}  = Error.unsupported "Fn Types. Internal error, should have been removed by now!"



-- -- TODO: ...
-- builtin :: LL.T.Type -> Maybe CG.T.Type
-- 
-- -- Tuples to Vectors
-- builtin (LL.T.Tuple [LL.T.Float, LL.T.Float])                       = Just CG.T.Vec2
-- builtin (LL.T.Tuple [LL.T.Float, LL.T.Float, LL.T.Float])            = Just CG.T.Vec3
-- builtin (LL.T.Tuple [LL.T.Float, LL.T.Float, LL.T.Float, LL.T.Float]) = Just CG.T.Vec4
-- 
-- -- ?
-- builtin (LL.T.Union (LL.ID.Ident' "Void") []) = Just CG.T.Void
-- 
-- builtin _ = Nothing

