{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Render.Prelude.Instances.Primitives where


-- *
import Core

import qualified Text.PrettyPrint.Leijen.Text as P

--- Local
import qualified Framework.Render.Prelude.Class as R
-- *


-- *
-- | Syntax
-- *

instance R.Render Bool where
    render = P.bool

instance R.Render Double where
    render = P.double

instance R.Render Int where
    render = P.int

instance R.Render Text where
  render = P.stringStrict


-- *
-- | Debug
-- *


instance R.RenderDebug Bool where
    debug = P.bool

instance R.RenderDebug Double where
    debug = P.double

instance R.RenderDebug Int where
    debug = P.int

instance R.RenderDebug Text where
    debug = P.stringStrict



