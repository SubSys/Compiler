{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.GLSL.AST.Render.Syntax.Base.Types (
    renderType
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

-- + Frameworks
import Framework.Text.Renderer
import qualified Framework.Text.Renderer.Utils as Util

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + GLSL AST Interface
import qualified CGIR.GLSL.Data.Interface as I

-- + GLSL AST
-- ++ Base
import qualified CGIR.GLSL.AST.Data.Base.Ident                 as ID
import qualified CGIR.GLSL.AST.Data.Base.Literals              as Lit
import qualified CGIR.GLSL.AST.Data.Base.Types                 as T
import qualified CGIR.GLSL.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.GLSL.AST.Data.BlockLevel.Stmt            as S
-- ++ Decl/Top Level
import qualified CGIR.GLSL.AST.Data.TopLevel.Functions         as Decl
import qualified CGIR.GLSL.AST.Data.TopLevel.Globals           as Decl

-- + Local
import qualified CGIR.GLSL.AST.Render.Syntax.Base.Ident as ID
-- *



{-# ANN module ("HLint: ignore" :: String) #-}





renderType :: T.Type -> Doc

renderType T.Void = "void"
renderType T.Bool = "bool"
renderType T.Int = "int"
renderType T.UInt = "uint"
renderType T.Float = "float"
renderType T.Vec2 = "vec2"
renderType T.Vec3 = "vec3"
renderType T.Vec4 = "vec4"
renderType T.BVec2 = "bvec2"
renderType T.BVec3 = "bvec3"
renderType T.BVec4 = "bvec4"
renderType T.IVec2 = "ivec2"
renderType T.IVec3 = "ivec3"
renderType T.IVec4 = "ivec4"
renderType T.UVec2 = "uvec2"
renderType T.UVec3 = "uvec3"
renderType T.UVec4 = "uvec4"
renderType T.Mat2x2 = "mat2x2"
renderType T.Mat2x3 = "mat2x3"
renderType T.Mat2x4 = "mat2x4"
renderType T.Mat3x2 = "mat3x2"
renderType T.Mat3x3 = "mat3x3"
renderType T.Mat3x4 = "mat3x4"
renderType T.Mat4x2 = "mat4x2"
renderType T.Mat4x3 = "mat4x3"
renderType T.Mat4x4 = "mat4x4"
renderType T.Sampler1D = "sampler1D"
renderType T.Sampler2D = "sampler2D"
renderType T.Sampler3D = "sampler3D"
renderType T.SamplerCube = "samplerCube"
renderType T.Sampler2DRect = "sampler2DRect"
renderType T.Sampler1DShadow = "sampler1DShadow"
renderType T.Sampler2DShadow = "sampler2DShadow"
renderType T.Sampler2DRectShadow = "sampler2DRectShadow"
renderType T.Sampler1DArray = "sampler1DArray"
renderType T.Sampler2DArray = "sampler2DArray"
renderType T.Sampler1DArrayShadow = "sampler1DArrayShadow"
renderType T.Sampler2DArrayShadow = "sampler2DArrayShadow"
renderType T.SamplerBuffer = "samplerBuffer"
renderType T.Sampler2DMS = "sampler2DMS"
renderType T.Sampler2DMSArray = "sampler2DMSArray"
renderType T.Isampler1D = "isampler1D"
renderType T.Isampler2D = "isampler2D"
renderType T.Isampler3D = "isampler3D"
renderType T.IsamplerCube = "isamplerCube"
renderType T.Isampler2DRect = "isampler2DRect"
renderType T.Isampler1DArray = "isampler1DArray"
renderType T.Isampler2DArray = "isampler2DArray"
renderType T.IsamplerBuffer = "isamplerBuffer"
renderType T.Isampler2DMS = "isampler2DMS"
renderType T.Isampler2DMSArray = "isampler2DMSArray"
renderType T.Usampler1D = "usampler1D"
renderType T.Usampler2D = "usampler2D"
renderType T.Usampler3D = "usampler3D"
renderType T.UsamplerCube = "usamplerCube"
renderType T.Usampler2DRect = "usampler2DRect"
renderType T.Usampler1DArray = "usampler1DArray"
renderType T.Usampler2DArray = "usampler2DArray"
renderType T.UsamplerBuffer = "usamplerBuffer"
renderType T.Usampler2DMS = "usampler2DMS"
renderType T.Usampler2DMSArray = "usampler2DMSArray"




