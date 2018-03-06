{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.GLSL.AST.Data.Base.Types (
    IR.Type

  , pattern Void
  , pattern Bool
  , pattern Int
  , pattern UInt
  , pattern Float
  , pattern Vec2
  , pattern Vec3
  , pattern Vec4
  , pattern BVec2
  , pattern BVec3
  , pattern BVec4
  , pattern IVec2
  , pattern IVec3
  , pattern IVec4
  , pattern UVec2
  , pattern UVec3
  , pattern UVec4
  , pattern Mat2x2
  , pattern Mat2x3
  , pattern Mat2x4
  , pattern Mat3x2
  , pattern Mat3x3
  , pattern Mat3x4
  , pattern Mat4x2
  , pattern Mat4x3
  , pattern Mat4x4
  , pattern Sampler1D
  , pattern Sampler2D
  , pattern Sampler3D
  , pattern SamplerCube
  , pattern Sampler2DRect
  , pattern Sampler1DShadow
  , pattern Sampler2DShadow
  , pattern Sampler2DRectShadow
  , pattern Sampler1DArray
  , pattern Sampler2DArray
  , pattern Sampler1DArrayShadow
  , pattern Sampler2DArrayShadow
  , pattern SamplerBuffer
  , pattern Sampler2DMS
  , pattern Sampler2DMSArray
  , pattern Isampler1D
  , pattern Isampler2D
  , pattern Isampler3D
  , pattern IsamplerCube
  , pattern Isampler2DRect
  , pattern Isampler1DArray
  , pattern Isampler2DArray
  , pattern IsamplerBuffer
  , pattern Isampler2DMS
  , pattern Isampler2DMSArray
  , pattern Usampler1D
  , pattern Usampler2D
  , pattern Usampler3D
  , pattern UsamplerCube
  , pattern Usampler2DRect
  , pattern Usampler1DArray
  , pattern Usampler2DArray
  , pattern UsamplerBuffer
  , pattern Usampler2DMS
  , pattern Usampler2DMSArray
) where


-- ~
import Core
import qualified CGIR.GLSL.Internal.AST as IR
-- ~





-- | Basic - Transparent Types
--

pattern Void :: IR.Type
pattern Void = IR.TransparentType IR.VoidType

pattern Bool :: IR.Type
pattern Bool = IR.TransparentType IR.BoolType

pattern Int :: IR.Type
pattern Int = IR.TransparentType IR.IntType

pattern UInt :: IR.Type
pattern UInt = IR.TransparentType IR.UIntType

pattern Float :: IR.Type
pattern Float = IR.TransparentType IR.FloatType

pattern Vec2 :: IR.Type
pattern Vec2 = IR.TransparentType IR.Vec2Type

pattern Vec3 :: IR.Type
pattern Vec3 = IR.TransparentType IR.Vec3Type

pattern Vec4 :: IR.Type
pattern Vec4 = IR.TransparentType IR.Vec4Type

pattern BVec2 :: IR.Type
pattern BVec2 = IR.TransparentType IR.BVec2Type

pattern BVec3 :: IR.Type
pattern BVec3 = IR.TransparentType IR.BVec3Type

pattern BVec4 :: IR.Type
pattern BVec4 = IR.TransparentType IR.BVec4Type

pattern IVec2 :: IR.Type
pattern IVec2 = IR.TransparentType IR.IVec2Type

pattern IVec3 :: IR.Type
pattern IVec3 = IR.TransparentType IR.IVec3Type

pattern IVec4 :: IR.Type
pattern IVec4 = IR.TransparentType IR.IVec4Type

pattern UVec2 :: IR.Type
pattern UVec2 = IR.TransparentType IR.UVec2Type

pattern UVec3 :: IR.Type
pattern UVec3 = IR.TransparentType IR.UVec3Type

pattern UVec4 :: IR.Type
pattern UVec4 = IR.TransparentType IR.UVec4Type

pattern Mat2x2 :: IR.Type
pattern Mat2x2 = IR.TransparentType IR.Mat2x2Type

pattern Mat2x3 :: IR.Type
pattern Mat2x3 = IR.TransparentType IR.Mat2x3Type

pattern Mat2x4 :: IR.Type
pattern Mat2x4 = IR.TransparentType IR.Mat2x4Type

pattern Mat3x2 :: IR.Type
pattern Mat3x2 = IR.TransparentType IR.Mat3x2Type

pattern Mat3x3 :: IR.Type
pattern Mat3x3 = IR.TransparentType IR.Mat3x3Type

pattern Mat3x4 :: IR.Type
pattern Mat3x4 = IR.TransparentType IR.Mat3x4Type

pattern Mat4x2 :: IR.Type
pattern Mat4x2 = IR.TransparentType IR.Mat4x2Type

pattern Mat4x3 :: IR.Type
pattern Mat4x3 = IR.TransparentType IR.Mat4x3Type

pattern Mat4x4 :: IR.Type
pattern Mat4x4 = IR.TransparentType IR.Mat4x4Type





-- | Floating Point Samplers
--

pattern Sampler1D :: IR.Type
pattern Sampler1D = IR.FloatingPointSamplerType IR.Sampler1DType

pattern Sampler2D :: IR.Type
pattern Sampler2D = IR.FloatingPointSamplerType IR.Sampler2DType

pattern Sampler3D :: IR.Type
pattern Sampler3D = IR.FloatingPointSamplerType IR.Sampler3DType

pattern SamplerCube :: IR.Type
pattern SamplerCube = IR.FloatingPointSamplerType IR.SamplerCubeType

pattern Sampler2DRect :: IR.Type
pattern Sampler2DRect = IR.FloatingPointSamplerType IR.Sampler2DRectType

pattern Sampler1DShadow :: IR.Type
pattern Sampler1DShadow = IR.FloatingPointSamplerType IR.Sampler1DShadowType

pattern Sampler2DShadow :: IR.Type
pattern Sampler2DShadow = IR.FloatingPointSamplerType IR.Sampler2DShadowType

pattern Sampler2DRectShadow :: IR.Type
pattern Sampler2DRectShadow = IR.FloatingPointSamplerType IR.Sampler2DRectShadowType

pattern Sampler1DArray :: IR.Type
pattern Sampler1DArray = IR.FloatingPointSamplerType IR.Sampler1DArrayType

pattern Sampler2DArray :: IR.Type
pattern Sampler2DArray = IR.FloatingPointSamplerType IR.Sampler2DArrayType

pattern Sampler1DArrayShadow :: IR.Type
pattern Sampler1DArrayShadow = IR.FloatingPointSamplerType IR.Sampler1DArrayShadowType

pattern Sampler2DArrayShadow :: IR.Type
pattern Sampler2DArrayShadow = IR.FloatingPointSamplerType IR.Sampler2DArrayShadowType

pattern SamplerBuffer :: IR.Type
pattern SamplerBuffer = IR.FloatingPointSamplerType IR.SamplerBufferType

pattern Sampler2DMS :: IR.Type
pattern Sampler2DMS = IR.FloatingPointSamplerType IR.Sampler2DMSType

pattern Sampler2DMSArray :: IR.Type
pattern Sampler2DMSArray = IR.FloatingPointSamplerType IR.Sampler2DMSArrayType



-- | Signed Integer Samplers
--

pattern Isampler1D :: IR.Type
pattern Isampler1D = IR.SignedIntegerSamplerType IR.Isampler1DType

pattern Isampler2D :: IR.Type
pattern Isampler2D = IR.SignedIntegerSamplerType IR.Isampler2DType

pattern Isampler3D :: IR.Type
pattern Isampler3D = IR.SignedIntegerSamplerType IR.Isampler3DType

pattern IsamplerCube :: IR.Type
pattern IsamplerCube = IR.SignedIntegerSamplerType IR.IsamplerCubeType

pattern Isampler2DRect :: IR.Type
pattern Isampler2DRect = IR.SignedIntegerSamplerType IR.Isampler2DRectType

pattern Isampler1DArray :: IR.Type
pattern Isampler1DArray = IR.SignedIntegerSamplerType IR.Isampler1DArrayType

pattern Isampler2DArray :: IR.Type
pattern Isampler2DArray = IR.SignedIntegerSamplerType IR.Isampler2DArrayType

pattern IsamplerBuffer :: IR.Type
pattern IsamplerBuffer = IR.SignedIntegerSamplerType IR.IsamplerBufferType

pattern Isampler2DMS :: IR.Type
pattern Isampler2DMS = IR.SignedIntegerSamplerType IR.Isampler2DMSType

pattern Isampler2DMSArray :: IR.Type
pattern Isampler2DMSArray = IR.SignedIntegerSamplerType IR.Isampler2DMSArrayType


-- | Unsigned Integer Samplers
--

pattern Usampler1D :: IR.Type
pattern Usampler1D = IR.UnsignedIntegerSamplerType IR.Usampler1DType

pattern Usampler2D :: IR.Type
pattern Usampler2D = IR.UnsignedIntegerSamplerType IR.Usampler2DType

pattern Usampler3D :: IR.Type
pattern Usampler3D = IR.UnsignedIntegerSamplerType IR.Usampler3DType

pattern UsamplerCube :: IR.Type
pattern UsamplerCube = IR.UnsignedIntegerSamplerType IR.UsamplerCubeType

pattern Usampler2DRect :: IR.Type
pattern Usampler2DRect = IR.UnsignedIntegerSamplerType IR.Usampler2DRectType

pattern Usampler1DArray :: IR.Type
pattern Usampler1DArray = IR.UnsignedIntegerSamplerType IR.Usampler1DArrayType

pattern Usampler2DArray :: IR.Type
pattern Usampler2DArray = IR.UnsignedIntegerSamplerType IR.Usampler2DArrayType

pattern UsamplerBuffer :: IR.Type
pattern UsamplerBuffer = IR.UnsignedIntegerSamplerType IR.UsamplerBufferType

pattern Usampler2DMS :: IR.Type
pattern Usampler2DMS = IR.UnsignedIntegerSamplerType IR.Usampler2DMSType

pattern Usampler2DMSArray :: IR.Type
pattern Usampler2DMSArray = IR.UnsignedIntegerSamplerType IR.Usampler2DMSArrayType









