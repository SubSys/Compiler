{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Header.Module.Exporting (
      IR.ModuleExporting
    , pattern Explicit
    , pattern Everything
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *






-- | Explicit Exports
-- I.e.
--  * `(Color(..), something)` in `module Sample exposing (Color(..), something)`
--
pattern Explicit :: [IR.Entry] -> IR.ModuleExporting
pattern Explicit entries = IR.ExportExplicit entries


-- | Export Everything
-- I.e.
--  * `(..)` in `module Sample exposing (..)`
--
pattern Everything :: IR.ModuleExporting
pattern Everything = IR.ExportEverything




