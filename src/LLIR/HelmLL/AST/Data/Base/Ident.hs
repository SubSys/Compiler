{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module LLIR.HelmLL.AST.Data.Base.Ident (
    IR.Ident
  , IR.Namespace

  , pattern Ident
  , pattern Namespace
  
  , pattern Ident'
  , pattern Ident''
  , pattern Ident_
) where


-- ~
import Core
import Core.Utils as Core
import Core.List.Util (singleton)

import Prelude (error, ($), String)

import qualified Data.List    as List
import qualified Data.Maybe   as Maybe
import qualified Data.Text    as Text
import qualified Data.String  as String

import qualified LLIR.HelmLL.Internal.AST as IR
-- ~


pattern Ident :: Text -> Maybe IR.Namespace -> IR.Ident
pattern Ident txt ns = IR.Ident txt ns


pattern Namespace :: [Text] -> IR.Namespace
pattern Namespace segs = IR.Namespace segs



-- | Alternate variations - convenience helpers
--


pattern Ident' :: String -> IR.Ident
pattern Ident' string <- IR.Ident (Text.unpack -> string) _
    where
        Ident' string = IR.Ident (Text.pack string) Nothing

pattern Ident'' :: String -> IR.Namespace -> IR.Ident
pattern Ident'' string ns <- IR.Ident (Text.unpack -> string) (Just ns)
    where
        Ident'' string ns = IR.Ident (Text.pack string) (Just ns)

pattern Ident_ :: Text -> IR.Ident
pattern Ident_ txt <- IR.Ident txt _
    where
        Ident_ txt = IR.Ident txt Nothing



