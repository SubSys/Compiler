{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Render.Display (
      packDoc
    , packDoc'
    , packDebugDoc
) where


-- *
import Core

import qualified Text.PrettyPrint.Leijen.Text as P

--- Local
import qualified Framework.Render.Prelude.Class as R
-- *


packDoc :: R.Render a => a -> Text
packDoc doc =
    P.displayTStrict toSimpleDoc
    where
        toSimpleDoc = P.renderPretty 0.4 400 (R.render doc)


packDebugDoc :: R.RenderDebug a => a -> Text
packDebugDoc doc =
    P.displayTStrict toSimpleDoc
    where
        toSimpleDoc = P.renderPretty 0.4 400 (R.debug doc)



packDoc' :: P.Doc -> Text
packDoc' doc =
    P.displayTStrict toSimpleDoc
    where
        toSimpleDoc = P.renderPretty 0.4 400 doc



