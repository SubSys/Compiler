{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Text.Renderer.Prelude.Class (
    Render(..)
) where



import qualified Text.PrettyPrint.Leijen.Text as P


class Render a where
    render :: a -> P.Doc
