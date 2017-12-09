module Framework.Parser.Prelude
    ( module Framework.Parser.Prelude.Data.Parser
    , module Framework.Parser.Prelude.Combinators.Lexer
    , module Framework.Parser.Prelude.Combinators.Symbol
    , module Framework.Parser.Prelude.Combinators.Layout
    , module Framework.Parser.Prelude.Combinators.Misc
    
    , module Control.Monad
    , module Control.Applicative
    
    , (<|>)
    , (<?>)
    , Char
    , String
    )
where

-- *
import Text.Megaparsec
    ( (<|>)
    , (<?>)
    )

import Data.Char   (Char)
import Data.String (String)

import Control.Monad
import Control.Applicative hiding ((<|>))


-- | Local Imports
import Framework.Parser.Prelude.Data.Parser

import Framework.Parser.Prelude.Combinators.Lexer 
import Framework.Parser.Prelude.Combinators.Symbol
import Framework.Parser.Prelude.Combinators.Layout
import Framework.Parser.Prelude.Combinators.Misc
-- *




