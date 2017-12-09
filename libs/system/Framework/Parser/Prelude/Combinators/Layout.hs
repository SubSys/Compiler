{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeFamilies         #-}
module Framework.Parser.Prelude.Combinators.Layout (
    (\:\)
  , (\~\)
) where



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



-- |
-- Strict:
-- * All items are expected to be indented equally.
-- Flexible:
-- * All items are expected to be indented equally, or greater than the reference point.
-- 
-- The reference point is defined by the beginning position of the first parser.
--
data AlignmentMode
    = StrictIndentation
    | FlexibleIndentation


infixl 3 \:\
infixl 3 \~\

intro \:\ outro =
    \scn_ ->
        indentationAPI StrictIndentation scn_ intro outro


intro \~\ outro =
    \scn_ ->
        indentationAPI FlexibleIndentation scn_ intro outro



indentationAPI :: (MonadParsec e s m, Token s ~ Char)
               => AlignmentMode
               -> m ()
               -> m a
               -> m b
               -> m (a, [b])
indentationAPI mode sc_ parserA parserB =
    go    
    where
        go = do
            sc_
            
            ref <- L.indentLevel
            
            intro <- parserA
            
            outro <- indented ref
            
            return (intro, flatten outro)
            
        indented ref = indentedHandler ref mode sc_ parserB




-- |
-- Helpers
indentedHandler :: (MonadParsec e s m, Token s ~ Char)
    => Pos
    -> AlignmentMode
    -> m ()
    -> m b
    -> m [[b]]
indentedHandler ref mode sc_ p =
    setup
    where
        check Nothing =
            L.indentGuard sc_ GT ref
        check (Just pos) =
            L.indentGuard sc_ EQ pos
        
        consume =
            manyTill p (lookAhead C.eol)
        
        go setting = do
            flag <- (optional . try) (check setting)
            
            case flag of
                Just _ ->
                    (:) <$> consume <*> go setting
                _ ->
                    return []
        
        setup = do
            flag <- (optional . try) (check Nothing)
            
            case (mode, flag) of
                (FlexibleIndentation, Just _) ->
                    go Nothing
                (StrictIndentation, override@(Just _)) ->
                    go override
                (_, Nothing) ->
                    return []
