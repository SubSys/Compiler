{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Header.Module.Importing (
      IR.ModuleImporting(..)
    , IR.ImportDecl
    , pattern Qualified
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




-- | Module Importing Register
--
pattern Importing :: [IR.ImportDecl] -> IR.ModuleImporting
pattern Importing importItems = IR.ModuleImporting importItems





-- | Various types of `import` declarations:
--


-- | Import Qualified
-- e.g.
--     * import Sample.One as One
--     * import Sample.One
pattern Qualified :: IR.Namespace -> Maybe (IR.Big Text) -> IR.ImportDecl
pattern Qualified namespace optAsName = IR.ImportQualified namespace optAsName


-- | Import Explicit
-- e.g.
--      import Something (Alpha, red, green, Blue(..))
pattern Explicit :: IR.Namespace -> [IR.Entry] -> IR.ImportDecl
pattern Explicit namespace entries = IR.ImportExplicit namespace entries


-- | Import Everything
-- e.g.
--      import Something.Alpha (..)
pattern Everything :: IR.Namespace -> IR.ImportDecl
pattern Everything namespace = IR.ImportEverything namespace


