{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmFlat.AST.Data.Semantic.Base.Values (
    IR.LiteralValue

  , pattern Char
  , pattern String
  , pattern Int
  , pattern Float
  , pattern Bool
) where


-- *
import Core

--- Local
import qualified HLIR.HelmFlat.Internal.AST as IR
-- *



pattern Char :: Text -> IR.LiteralValue
pattern Char val = IR.CharLit val

pattern String :: Text -> IR.LiteralValue
pattern String val = IR.StringLit val

pattern Int :: Int -> IR.LiteralValue
pattern Int val = IR.IntLit val

pattern Float :: Double -> IR.LiteralValue
pattern Float val = IR.FloatLit val

pattern Bool :: Bool -> IR.LiteralValue
pattern Bool val = IR.BoolLit val
