{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.GLSL.AST.Data.Base.Literals (
    IR.LiteralValue
  , pattern Float
  , pattern Int
  , pattern Bool
) where


-- ~
import Core
import qualified CGIR.GLSL.Internal.AST as IR
-- ~





pattern Float :: Double -> IR.LiteralValue
pattern Float val = IR.Float val

pattern Int :: Int -> IR.LiteralValue
pattern Int val = IR.Int val

pattern Bool :: Bool -> IR.LiteralValue
pattern Bool val = IR.Bool val





















