{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Parser.Base.Types (
    parseType
) where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)

import qualified Data.Text as Text


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Ident as ID
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Metadata as Meta


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




parseType = choice
    [ try parseString <?> "type"
    , try parseChar   <?> "type"
    , try parseInt    <?> "type"
    , try parseFloat  <?> "type"
    , try parseBool   <?> "type"
    , try parseRecord <?> "type"
    , try parseTuple  <?> "type"
    , try parseList   <?> "type"
    , try parseArr    <?> "type"
    , try parseParens <?> "type"
    , try parseVar    <?> "type"
    , parseUnion      <?> "type"
    ]


parseVar :: Parser T.Type
parseVar =
    Meta.trackPosition (ID.parseLow --> T.Var)


parseRecord :: Parser T.Type
parseRecord =
    Meta.trackPosition go
    where
        go =
            braces (field `sepBy1` reservedOp ",") --> T.Record
        
        fields :: Parser [(ID.Low, T.Type)]
        fields = braces (field `sepBy1` reservedOp ",")
        
        field = do
            
            startingLoc <- Meta.getLocation
            endingLoc <- Meta.getLocation
            
            id' <- ID.parseLow
            reservedOp ":"
            ty <- parseType
            return (id', ty)



parseTuple :: Parser T.Type
parseTuple =
    Meta.trackPosition (parens (parseType `divBy` reservedOp ",") <**> return T.Tuple)
    
    



parseList :: Parser T.Type
parseList =
    Meta.trackPosition go
    where
        go = reservedWord "List" *> parseType <**> return T.List



parseUnion :: Parser T.Type
parseUnion =
    Meta.trackPosition go
    where
        go = do    
            name <- ID.parseBig
            args <- many (try parseType)
            
            return $ T.Union name args




parseArr :: Parser T.Type
parseArr =
    Meta.trackPosition go
    where
        go = parens (parseType `divBy` reservedOp "->") <**> return pack

        pack :: [T.Type] -> Maybe Meta.Meta -> T.Type
        pack [x, y] m = T.Arr x y m
        pack (x:xs) m = T.Arr x (pack xs m) m


parseParens :: Parser T.Type
parseParens =
    Meta.trackPosition go
    where
        go = parens parseType <**> return T.Parens





-- *
-- | Literal Types
-- *

parseString :: Parser T.Type
parseString =
    Meta.trackPosition go
    where
        go = reservedWord "String" *> return T.String

parseChar :: Parser T.Type
parseChar =
    Meta.trackPosition go
    where
        go = reservedWord "Char" *> return T.Char

parseInt :: Parser T.Type
parseInt =
    Meta.trackPosition go
    where
        go = reservedWord "Int" *> return T.Int

parseFloat :: Parser T.Type
parseFloat =
    Meta.trackPosition go
    where
        go = reservedWord "Float" *> return T.Float

parseBool :: Parser T.Type
parseBool =
    Meta.trackPosition go
    where
        go = reservedWord "Bool" *> return T.Bool


















