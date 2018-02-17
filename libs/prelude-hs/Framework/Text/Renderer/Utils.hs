{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Text.Renderer.Utils (
    -- | Basic combinators
      P.empty
    , P.isEmpty
    , P.char
    , P.text
    , P.textStrict
    , P.nest
    , P.line
    , P.linebreak
    , P.group
    , P.softline
    , P.softbreak
    , P.spacebreak

    -- | Alignment
    , P.align
    , P.hang
    , P.indent
    , P.encloseSep
    , P.list
    , P.tupled
    , P.semiBraces

     -- | List combinators
    , P.hsep
    , P.vsep
    , P.fillSep
    , P.sep
    , P.hcat
    , P.vcat
    , P.fillCat
    , P.cat
    , P.punctuate

    -- | Bracketing combinators
    , P.enclose
    , P.squotes
    , P.dquotes
    , P.parens
    , P.angles
    , P.braces
    , P.brackets

    -- | Character documents
    , P.lparen
    , P.rparen
    , P.langle
    , P.rangle
    , P.lbrace
    , P.rbrace
    , P.lbracket
    , P.rbracket
    , P.squote
    , P.dquote
    , P.semi
    , P.colon
    , P.comma
    , P.space
    , P.dot
    , P.backslash
    , P.equals
) where



import Core
import qualified  Text.PrettyPrint.Leijen.Text as P



