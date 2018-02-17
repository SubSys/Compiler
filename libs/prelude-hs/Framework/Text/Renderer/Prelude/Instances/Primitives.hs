{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Text.Renderer.Prelude.Instances.Primitives where




import Core

import qualified Text.PrettyPrint.Leijen.Text as P

--- Local
import Framework.Text.Renderer.Prelude.Class as R




-- | Syntax


instance R.Render Bool where
    render = P.bool

instance R.Render Double where
    render = P.double

instance R.Render Int where
    render = P.int

instance R.Render Text where
  render = P.stringStrict





