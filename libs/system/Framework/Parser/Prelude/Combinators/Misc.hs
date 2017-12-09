{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeFamilies         #-}
module Framework.Parser.Prelude.Combinators.Misc (divBy, (-->)) where


-- *
import Core
import Core.List.Util (flatten)

import System.IO (IO(..))
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L


import Text.Megaparsec as M
import Data.Char  
import Data.String
import Data.List ((++), null)

import Control.Monad
import Control.Applicative hiding ((<|>))

import qualified Data.List as List

-- | Framework Imports
import Framework.Parser.Prelude.Data.Parser
import Framework.Parser.Prelude.Combinators.Lexer
import Framework.Parser.Prelude.Combinators.Symbol
-- *



-- | A better version of `sepBy`.
-- It turns out `sepBy` is rather unintuitive (at least for me).
-- I.e. I imagined:
--  * ```parens (parseType `sepBy` reservedOp “->”)```
--  * to fail on `(a)`.
--
divBy :: (Alternative m, Monad m) => m a -> m sep -> m [a]
divBy p sep = do
    x <- p
    xs <- some (sep *> p)
    return $ x : xs



-- |
-- Just to simplify the below pattern:
-- * `parseLow <**> return Type.Var` is now, `parseLow --> Type.Var`.
--
(-->) :: Monad m => m t -> (t -> b) -> m b
(-->) a b = do
    a' <- a
    
    return (b a')


infixl 4 -->



