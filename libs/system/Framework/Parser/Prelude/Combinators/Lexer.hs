{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Parser.Prelude.Combinators.Lexer (
    scn
  , sc
  , lexeme
  , symbol
) where

-- *
import Core
import Core.Control.Flow

import Data.Char (Char)
import Data.String (String)


import Control.Monad
import Control.Applicative

import Data.List ((++), elem)
import System.IO (IO)

import Text.Megaparsec
    ( (<|>)
    , (<?>)
    )

import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec            as M

import qualified Data.Text   as T
import qualified Data.String as String
import qualified Data.Char   as Char
import qualified Data.List   as List


-- | Local Prelude
import Framework.Parser.Prelude.Data.Parser


-- | Local Imports
-- *




-- |
-- Space Consumers


lineComment  = L.skipLineComment "--"

-- TODO: use `L.skipBlockCommentNested`
blockComment = L.skipBlockComment "{-" "-}"


-- |
-- Lexer


scn :: Parser ()
scn = L.space C.space1 lineComment blockComment


sc :: Parser ()
sc =
    L.space (void $ M.takeWhile1P Nothing f) lineComment blockComment
    where
        f x = x == ' ' || x == '\t'



lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc


