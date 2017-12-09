{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core (

    -- Internal
      upstream
    , pipeline
) where


-- *
import Core hiding (isSymbol)
import Core.Control.Flow
import Core.List.Util (flatten)

import Prelude (mapM_, IO, String, return)

import qualified Data.Text as Text

--- Local
-- ~ Upstream Cores
import qualified SLIR.HelmParsed.Core as ParserCore
import qualified SLIR.HelmSyntax.Core as SyntaxCore
import qualified HLIR.HelmCore.Core   as HelmCore
import qualified LLIR.DarkRoast.Core  as DarkRoastCore

-- ~ RedRust Payload
import qualified CGIR.Rust.Data.Payload.Input as IR

-- ~ Passes
import qualified CGIR.Rust.Pass.Formalize as Formalize
-- *






-- *
-- | Internal
-- *
upstream source =
    DarkRoastCore.input (HelmCore.input (SyntaxCore.input $ ParserCore.input source))



pipeline :: IR.Payload -> IR.Payload
pipeline payload =
    payload |> Formalize.input




