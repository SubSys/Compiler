{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.Base.Values (
      pattern Char
    , pattern String
    , pattern Bool
    , pattern I8
    , pattern I16
    , pattern I32
    , pattern I64
    , pattern I128
    , pattern U8
    , pattern U16
    , pattern U32
    , pattern U64
    , pattern U128
    , pattern F32
    , pattern F64
    , IR.ScalarValue
) where


-- *
import Core
import Data.Data (Data, Typeable)

--- Local
import qualified CGIR.Rust.Internal.AST        as IR
-- *





-- | Value Constructors
--



pattern Char :: Text -> IR.ScalarValue
pattern Char val = IR.CharValue val

pattern String :: Text -> IR.ScalarValue
pattern String val = IR.StringValue val

pattern Bool :: Bool -> IR.ScalarValue
pattern Bool val = IR.BoolValue val

pattern I8 :: Int -> IR.ScalarValue
pattern I8 val = IR.I8Value val

pattern I16 :: Int -> IR.ScalarValue
pattern I16 val = IR.I16Value val

pattern I32 :: Int -> IR.ScalarValue
pattern I32 val = IR.I32Value val

pattern I64 :: Int -> IR.ScalarValue
pattern I64 val = IR.I64Value val

pattern I128 :: Int -> IR.ScalarValue
pattern I128 val = IR.I128Value val

pattern U8 :: Int -> IR.ScalarValue
pattern U8 val = IR.U8Value val

pattern U16 :: Int -> IR.ScalarValue
pattern U16 val = IR.U16Value val

pattern U32 :: Int -> IR.ScalarValue
pattern U32 val = IR.U32Value val

pattern U64 :: Int -> IR.ScalarValue
pattern U64 val = IR.U64Value val

pattern U128 :: Int -> IR.ScalarValue
pattern U128 val = IR.U128Value val

pattern F32 :: Double -> IR.ScalarValue
pattern F32 val = IR.F32Value val

pattern F64 :: Double -> IR.ScalarValue
pattern F64 val = IR.F64Value val






