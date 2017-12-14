{-# LANGUAGE NoImplicitPrelude #-}
module Frontend.Helm.Tool.Crawl where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import Prelude (mapM_, IO, String, return)

--- Local
import qualified Framework.Helm.Data.Manifest as Manifest
import qualified SLIR.HelmSyntax.Data.Payload as Payload
import qualified SLIR.HelmSyntax.Core.Parser.Driver as Driver
-- *




