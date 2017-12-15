{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Base (Module Header) Types
--
module SLIR.HelmSyntax.AST.Data.Header.Base (
      IR.Entries(..)
    , IR.Entry(..)
    , IR.UnionExposing(..)
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *