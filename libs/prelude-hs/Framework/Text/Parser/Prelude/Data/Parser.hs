module Framework.Text.Parser.Prelude.Data.Parser (Parser) where

import Prelude ()
import Core
import Data.Void (Void)

import qualified Text.Megaparsec as M

import Control.Monad.Identity        as I
import Control.Monad.Trans.Identity  as TI

import Data.String (String)

import Text.Megaparsec
    ( Parsec
    , ParsecT
    , (<|>)
    , (<?>)
    )

-- type Parser = Parsec Void String
type Parser = ParsecT Void String Identity


