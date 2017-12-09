{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.Base.Types (
      IR.Type
    , pattern Void
    , pattern Const
    , pattern Constexpr
    , pattern Generic
    , pattern Bool
    , pattern F32
    , pattern F16
    , pattern I32
    , pattern I16
    , pattern I8
    , pattern U32
    , pattern U16
    , pattern U8
    , pattern BoolVec
    , pattern F32Vec
    , pattern F16Vec
    , pattern I32Vec
    , pattern I16Vec
    , pattern I8Vec
    , pattern U32Vec
    , pattern U16Vec
    , pattern U8Vec
    , pattern F32Mat
    , pattern F16Mat
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *



pattern Void :: IR.Type
pattern Void = IR.Void

pattern Const :: IR.Type -> IR.Type
pattern Const ty = IR.ConstType ty

pattern Constexpr :: IR.Type -> IR.Type
pattern Constexpr ty = IR.ConstexprType ty

pattern Generic :: IR.Big -> IR.Type
pattern Generic id_ = IR.GenericType id_

-- *
-- | # Scalar Types
-- *


pattern Bool :: IR.Type
pattern Bool = IR.ScalarType IR.BoolType

pattern F32 :: IR.Type
pattern F32 = IR.ScalarType IR.F32Type

pattern F16 :: IR.Type
pattern F16 = IR.ScalarType IR.F16Type

pattern I32 :: IR.Type
pattern I32 = IR.ScalarType IR.I32Type

pattern I16 :: IR.Type
pattern I16 = IR.ScalarType IR.I16Type

pattern I8 :: IR.Type
pattern I8 = IR.ScalarType IR.I8Type

pattern U32 :: IR.Type
pattern U32 = IR.ScalarType IR.U32Type

pattern U16 :: IR.Type
pattern U16 = IR.ScalarType IR.U16Type

pattern U8 :: IR.Type
pattern U8 = IR.ScalarType IR.U8Type






-- *
-- | # Vector Types
-- *



pattern BoolVec :: IR.IndexType -> IR.Type
pattern BoolVec idx = IR.VectorType (IR.BoolVecType idx)

pattern F32Vec :: IR.IndexType -> IR.Type
pattern F32Vec idx = IR.VectorType (IR.F32VecType idx)

pattern F16Vec :: IR.IndexType -> IR.Type
pattern F16Vec idx = IR.VectorType (IR.F16VecType idx)

pattern I32Vec :: IR.IndexType -> IR.Type
pattern I32Vec idx = IR.VectorType (IR.I32VecType idx)

pattern I16Vec :: IR.IndexType -> IR.Type
pattern I16Vec idx = IR.VectorType (IR.I16VecType idx)

pattern I8Vec :: IR.IndexType -> IR.Type
pattern I8Vec idx = IR.VectorType (IR.I8VecType idx)

pattern U32Vec :: IR.IndexType -> IR.Type
pattern U32Vec idx = IR.VectorType (IR.U32VecType idx)

pattern U16Vec :: IR.IndexType -> IR.Type
pattern U16Vec idx = IR.VectorType (IR.U16VecType idx)

pattern U8Vec :: IR.IndexType -> IR.Type
pattern U8Vec idx = IR.VectorType (IR.U8VecType idx)







-- *
-- | # Matrix Types
-- *
pattern F32Mat :: IR.DimType -> IR.Type
pattern F32Mat dim = IR.MatrixType (IR.F32MatType dim)

pattern F16Mat :: IR.DimType -> IR.Type
pattern F16Mat dim = IR.MatrixType (IR.F16MatType dim)






-- *
-- | Pixel Types
-- *

-- TODO...






-- *
-- | # Address Space Attributes (as Pointer Prefixes)
-- |
-- |   * See 'Metal Shading Language Specification (V2)' section 4.2,
-- | 'Address Space Attributes for Variables and Arguments'.
-- *

-- pattern Device :: IR.Type -> IR.Type
-- pattern Device ty = IR.AddressSpacePtr (IR.DeviceType ty)
-- 
-- pattern Threadgroup :: IR.Type -> IR.Type
-- pattern Threadgroup ty = IR.AddressSpacePtr (IR.ThreadgroupType ty)
-- 
-- pattern ThreadgroupImgBlk :: IR.Type -> IR.Type
-- pattern ThreadgroupImgBlk ty = IR.AddressSpacePtr (IR.ThreadgroupImgBlkType ty)
-- 
-- pattern Constant :: IR.Type -> IR.Type
-- pattern Constant ty = IR.AddressSpacePtr (IR.ConstantType ty)
-- 
-- pattern Thread :: IR.Type -> IR.Type
-- pattern Thread ty = IR.AddressSpacePtr (IR.ThreadType ty)









