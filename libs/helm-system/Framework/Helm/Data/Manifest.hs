{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Helm.Data.Manifest (
      SourceDirectories(..)
    , ExposedModules(..)
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import Prelude (mapM_, IO, String, return)

--- Local
-- *




-- *
-- | Package
-- *


-- |
-- E.g. `helm-lang` in `helm-lang/core`.
newtype PackageNamespace = PackageNamespace Text

-- |
-- E.g. `core` in `helm-lang/core`.
newtype PackageName = PackageName Text


-- *
-- | Project
-- *



newtype SourceDirectories = SourceDirectories [FilePath]

newtype ExposedModules = ExposedModules [ModulePath]


-- newtype Dependencies




-- *
-- | Misc.
-- *
type FilePath = Text
type ModulePath = Text






