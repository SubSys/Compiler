module Framework.Text.Parser.Prelude
    ( module Framework.Text.Parser.Prelude.Data.Parser
    , module Framework.Text.Parser.Prelude.Combinators.Lexer
    , module Framework.Text.Parser.Prelude.Combinators.Symbol
    , module Framework.Text.Parser.Prelude.Combinators.Layout
    , module Framework.Text.Parser.Prelude.Combinators.Misc
    
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
import Framework.Text.Parser.Prelude.Data.Parser

import Framework.Text.Parser.Prelude.Combinators.Lexer 
import Framework.Text.Parser.Prelude.Combinators.Symbol
import Framework.Text.Parser.Prelude.Combinators.Layout
import Framework.Text.Parser.Prelude.Combinators.Misc
-- *




