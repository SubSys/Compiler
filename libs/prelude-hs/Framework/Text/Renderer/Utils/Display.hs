{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Text.Renderer.Utils.Display (
    packDoc
) where



import Core

import qualified Text.PrettyPrint.Leijen.Text as P

--- Local
import qualified Framework.Text.Renderer.Prelude.Class as R






packDoc :: P.Doc -> Text
packDoc doc =
    P.displayTStrict toSimpleDoc
    where
        toSimpleDoc = P.renderPretty 0.4 400 doc

