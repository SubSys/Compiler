{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Text.Parser.Prelude.Combinators.Symbol (
    parens
  , braces
  , brackets
  , reservedWord
  , reservedOp
  , keywords
  , opKeywords
  , comma
  , colon
  , dot
  , equals
  , openParens
  , closeParens
  , openBrace
  , closeBrace
  , rightArrow
  , forwardSlash
  , backslash
  , caseKW
  , nullKW
  , ofKW
  , letKW
  , inKW
  , ifKW
  , thenKW
  , elseIfKW
  , elseKW
  , lowerIdentifier
  , upperIdentifier
  , opIdentifier
  , lowChar
  , upperChar
  , opChar
) where

-- *
import Core
import Core.Control.Flow

import Data.Char (Char)
import Data.String (String)

import Text.Megaparsec
    ( (<|>)
    , (<?>)
    )

import Control.Monad
import Control.Applicative

import Data.List ((++), elem)
import System.IO (IO)


import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec            as M

import qualified Data.Text   as T
import qualified Data.String as String
import qualified Data.Char   as Char
import qualified Data.List   as List


-- | Local Prelude
import Framework.Text.Parser.Prelude.Data.Parser
import Framework.Text.Parser.Prelude.Combinators.Lexer


-- | Local Imports
-- *




-- |
-- # Misc. Combinators
--


-- |
-- ## Enclosures
parens :: Parser a -> Parser a
parens = M.between (reservedOp "(") (reservedOp ")")

braces = M.between (symbol "{") (symbol "}")
brackets = M.between (symbol "[") (symbol "]")



-- |
-- # Reserved Words & Ops
reservedWord :: String -> Parser ()
reservedWord w =
    lexeme (C.string w *> M.notFollowedBy C.alphaNumChar)

-- |
-- NOTE TO SELF:
-- I’ve considered that `notFollowedBy` should be any char,
-- yet this would break parsers that allow items to precede a comma for instance,
-- without being separated by a space. 
reservedOp :: String -> Parser ()
reservedOp w =
    lexeme (C.string w *> M.notFollowedBy opChar)


-- |
-- List of Reserved Words
--
keywords :: [String]
keywords =
    [ "if"
    , "then"
    , "else"
    , "case"
    , "of"
    , "let"
    , "in"
    , "type"
    , "module"
    , "where"
    , "import"
    , "exposing"
    , "as"
    , "port"
    ]

opKeywords :: [String]
opKeywords =
    [ "."
    , "|"
    , "->"
    , "="
    , ":"
    ]



-- |
-- # Special Symbols
comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"


dot :: Parser String
dot = symbol "."

equals :: Parser String
equals = symbol "="

openParens :: Parser String
openParens = symbol "("

closeParens :: Parser String
closeParens = symbol ")"


openBrace :: Parser String
openBrace = symbol "{"

closeBrace :: Parser String
closeBrace = symbol "}"

openBracket :: Parser String
openBracket = symbol "["

closeBracket :: Parser String
closeBracket = symbol "]"

rightArrow :: Parser String
rightArrow = symbol "->"

forwardSlash :: Parser String
forwardSlash = symbol "/"

backslash :: Parser String
backslash = symbol "\\"


-- |
-- ## keywords
caseKW :: Parser String
caseKW = symbol "case"

nullKW :: Parser String
nullKW = symbol "_"

ofKW :: Parser String
ofKW = symbol "of"

letKW :: Parser String
letKW = symbol "let"

inKW :: Parser String
inKW = symbol "in"

ifKW :: Parser String
ifKW = symbol "if"


thenKW :: Parser String
thenKW = symbol "then"

elseIfKW :: Parser String
elseIfKW = symbol "else if"

elseKW :: Parser String
elseKW = symbol "else"








-- |
-- Token Combinators
--
lowerIdentifier :: Parser String
lowerIdentifier =
    (lexeme . M.try) (p >>= check)
    where
        p = (:) <$> C.lowerChar <*> M.many C.alphaNumChar
        
        check x =
            if x `elem` keywords then
                fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else
                return x


upperIdentifier :: Parser String
upperIdentifier =
    (lexeme . M.try) (p >>= check)
    where
        p = (:) <$> C.upperChar <*> M.many C.alphaNumChar
        
        check x =
            
            if x `elem` keywords then
                fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else
                return x




opIdentifier :: Parser String
opIdentifier =
    (lexeme . M.try) (p >>= check)
    where
        p = (:) <$> opChar <*> M.many opChar
        
        check x =
            if x `elem` opKeywords then
                fail $ "keyword " ++ show x ++ " cannot be an op-identifier"
            else
                return x


lowChar :: Parser Char
lowChar = C.satisfy $ (||) <$> Char.isLower <*> (== '_')

upperChar :: Parser Char
upperChar =
    C.satisfy Char.isUpper

opChar :: Parser Char
opChar =
    let
        range :: String
        range = "+-/*=.<>:&|^?%~!"
    in
        C.satisfy (`elem` range)


