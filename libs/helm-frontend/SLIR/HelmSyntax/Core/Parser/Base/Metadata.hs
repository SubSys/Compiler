{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
module SLIR.HelmSyntax.Core.Parser.Base.Metadata (
      initMeta
    , getLocation
    , trackPosition
) where


-- *
import Core
import Core.Control.Flow
import Core.List.Util as ListUtil

import Prelude (String)

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as C

--- Frameworks
import Framework.Parser


--- Local
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Metadata
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta
-- *




initMeta :: Meta.Location -> Meta.Location ->  Maybe Meta.Meta
initMeta startingLocation endingLocation =
    Just
        Meta.Meta
            { Meta.modulePath = Text.empty
            , Meta.span =
                Meta.Span startingLocation endingLocation
            }


getLocation :: Parser Meta.Location
getLocation = do
    SourcePos {sourceName, sourceLine, sourceColumn} <- getPosition
    
    return $ Meta.Location (unPos sourceLine) (unPos sourceColumn)



trackPosition :: Parser (Maybe Meta.Meta -> b) -> Parser b
trackPosition p =
    do
        starting <- getLocation
        
        x <- p
        
        ending <- getLocation
        
        let res = x (initMeta starting ending)
        
        return res


