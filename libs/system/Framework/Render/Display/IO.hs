{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Render.Display.IO (
    printDoc
) where


-- *
import Core
import Prelude (IO)

import qualified Text.PrettyPrint.Leijen.Text as P

--- Local
import qualified Framework.Render.Prelude.Class as R
-- *


printDoc :: R.Render a => a -> IO ()
printDoc x =
        P.putDoc $ R.render x
