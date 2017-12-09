{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Render.Prelude.Class (
      Render(..)
    , RenderDebug(..)
) where


-- *
import Core

import qualified Text.PrettyPrint.Leijen.Text as P
-- *


class Render a where
    render :: a -> P.Doc

class RenderDebug a where
    debug :: a -> P.Doc


