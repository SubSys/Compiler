{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.HelmLL.AST.Data.Base.Literals (
    IR.LiteralValue
    
  , pattern Float
  , pattern Int
  , pattern String
  , pattern Char
  , pattern Bool
) where


-- ~
import Core
import Core.Utils as Core
import Core.List.Util (singleton)

import Prelude (error, ($))

import qualified Data.List    as List
import qualified Data.Maybe   as Maybe
import qualified Data.Text    as Text
import qualified Data.String  as String

import qualified LLIR.HelmLL.Internal.AST as IR
-- ~




pattern Float :: Double -> IR.LiteralValue
pattern Float val = IR.Float val

pattern Int :: Int -> IR.LiteralValue
pattern Int val = IR.Int val

pattern String :: Text -> IR.LiteralValue
pattern String val = IR.String val

pattern Char :: Text -> IR.LiteralValue
pattern Char val = IR.Char val

pattern Bool :: Bool -> IR.LiteralValue
pattern Bool val = IR.Bool val







