{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Toolbox.Internal.Data.Scope (
      IR.Scope(..)
    , IR.Binder(..)
    , IR.Ref(..)
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Text as Text
import qualified Data.List as List

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *
