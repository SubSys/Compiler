{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Semantic.Base.Values (
    IR.LiteralValue

  , pattern Char
  , pattern String
  , pattern Int
  , pattern Float
  , pattern Bool

  , pattern Char'
  , pattern String'
  , pattern Int'
  , pattern Float'
  , pattern Bool'
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *



pattern Char :: Text -> IR.Meta -> IR.LiteralValue
pattern Char val meta = IR.CharLit val meta

pattern String :: Text -> IR.Meta -> IR.LiteralValue
pattern String val meta = IR.StringLit val meta

pattern Int :: Int -> IR.Meta -> IR.LiteralValue
pattern Int val meta = IR.IntLit val meta

pattern Float :: Double -> IR.Meta -> IR.LiteralValue
pattern Float val meta = IR.FloatLit val meta

pattern Bool :: Bool -> IR.Meta -> IR.LiteralValue
pattern Bool val meta = IR.BoolLit val meta


-- | Alternative variations & misc. helpers.
--

pattern Char' :: Text -> IR.LiteralValue
pattern Char' val <- IR.CharLit val _
    where
        Char' val = IR.CharLit val IR.Empty

pattern String' :: Text -> IR.LiteralValue
pattern String' val <- IR.StringLit val _
    where
        String' val = IR.StringLit val IR.Empty

pattern Int' :: Int -> IR.LiteralValue
pattern Int' val <- IR.IntLit val _
    where
        Int' val = IR.IntLit val IR.Empty

pattern Float' :: Double -> IR.LiteralValue
pattern Float' val <- IR.FloatLit val _
    where
        Float' val = IR.FloatLit val IR.Empty

pattern Bool' :: Bool -> IR.LiteralValue
pattern Bool' val <- IR.BoolLit val _
    where
        Bool' val = IR.BoolLit val IR.Empty