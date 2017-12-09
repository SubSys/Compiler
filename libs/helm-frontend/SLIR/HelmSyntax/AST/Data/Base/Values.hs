{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Base.Values (
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

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *




pattern Char :: Text -> Maybe IR.Meta -> IR.LiteralValue
pattern Char val metaOpt = IR.CharLit val metaOpt

pattern String :: Text -> Maybe IR.Meta -> IR.LiteralValue
pattern String val metaOpt = IR.StringLit val metaOpt

pattern Int :: Int -> Maybe IR.Meta -> IR.LiteralValue
pattern Int val metaOpt = IR.IntLit val metaOpt

pattern Float :: Double -> Maybe IR.Meta -> IR.LiteralValue
pattern Float val metaOpt = IR.FloatLit val metaOpt

pattern Bool :: Bool -> Maybe IR.Meta -> IR.LiteralValue
pattern Bool val metaOpt = IR.BoolLit val metaOpt


-- *
-- | Misc.
-- *

pattern Char' :: Text -> IR.LiteralValue
pattern Char' val = IR.CharLit val Nothing

pattern String' :: Text -> IR.LiteralValue
pattern String' val = IR.StringLit val Nothing

pattern Int' :: Int -> IR.LiteralValue
pattern Int' val = IR.IntLit val Nothing

pattern Float' :: Double -> IR.LiteralValue
pattern Float' val = IR.FloatLit val Nothing

pattern Bool' :: Bool -> IR.LiteralValue
pattern Bool' val = IR.BoolLit val Nothing
