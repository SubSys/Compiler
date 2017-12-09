{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.Base.Values (
      IR.ScalarValue
    , IR.MatrixValue
    , IR.VectorValue
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




-- *
-- | Scalar Values
-- *

pattern Bool :: Bool -> IR.ScalarValue
pattern Bool val = IR.BoolValue val

pattern F32 :: Double -> IR.ScalarValue
pattern F32 val = IR.F32Value val

pattern F16 :: Double -> IR.ScalarValue
pattern F16 val = IR.F16Value val

pattern I32 :: Int -> IR.ScalarValue
pattern I32 val = IR.I32Value val

pattern I16 :: Int -> IR.ScalarValue
pattern I16 val = IR.I16Value val

pattern I8 :: Int -> IR.ScalarValue
pattern I8 val = IR.I8Value val

pattern U32 :: Int -> IR.ScalarValue
pattern U32 val = IR.U32Value val

pattern U16 :: Int -> IR.ScalarValue
pattern U16 val = IR.U16Value val

pattern U8 :: Int -> IR.ScalarValue
pattern U8 val = IR.U8Value val



-- *
-- | Vector Values
-- *

pattern BoolVec :: IR.IndexValue Bool -> IR.VectorValue
pattern BoolVec xs = IR.BoolVecValue xs

pattern F32Vec :: IR.IndexValue Double -> IR.VectorValue
pattern F32Vec xs = IR.F32VecValue xs

pattern F16Vec :: IR.IndexValue Double -> IR.VectorValue
pattern F16Vec xs = IR.F16VecValue xs

pattern I32Vec :: IR.IndexValue Int -> IR.VectorValue
pattern I32Vec xs = IR.I32VecValue xs

pattern I16Vec :: IR.IndexValue Int -> IR.VectorValue
pattern I16Vec xs = IR.I16VecValue xs

pattern I8Vec :: IR.IndexValue Int -> IR.VectorValue
pattern I8Vec xs = IR.I8VecValue xs

pattern U32Vec :: IR.IndexValue Int -> IR.VectorValue
pattern U32Vec xs = IR.U32VecValue xs

pattern U16Vec :: IR.IndexValue Int -> IR.VectorValue
pattern U16Vec xs = IR.U16VecValue xs

pattern U8Vec :: IR.IndexValue Int -> IR.VectorValue
pattern U8Vec xs = IR.U8VecValue xs




-- *
-- | Matrix Values
-- *



pattern F32Mat :: IR.DimValue Double -> IR.MatrixValue
pattern F32Mat x = IR.F32MatValue x

pattern F16Mat :: IR.DimValue Double -> IR.MatrixValue
pattern F16Mat x = IR.F16MatValue x
