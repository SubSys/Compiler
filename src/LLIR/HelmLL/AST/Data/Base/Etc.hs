{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module LLIR.HelmLL.AST.Data.Base.Etc (
    IR.Binder

  , pattern Binder

  , pattern Binder'
  , pattern Binder''
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


pattern Binder :: IR.Ident -> Maybe IR.Type -> IR.Binder
pattern Binder ident ty = IR.Binder ident ty




-- | Alternate variations - convenience helpers
--


pattern Binder' :: String -> IR.Binder
pattern Binder' ident <- IR.Binder (IR.Ident (Text.unpack -> ident) _) _
    where
        Binder' ident = IR.Binder (IR.Ident (Text.pack ident) Nothing) Nothing

pattern Binder'' :: String -> IR.Type -> IR.Binder
pattern Binder'' ident ty <- IR.Binder (IR.Ident (Text.unpack -> ident) _) (Just ty)
    where
        Binder'' ident ty = IR.Binder (IR.Ident (Text.pack ident) Nothing) (Just ty)


