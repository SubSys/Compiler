{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Parser.TermLevel.Patterns (
      parseCaseAlt
    , parsePattern
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Foldable as Fold

--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Parser.Base.Ident as ID
import qualified SLIR.HelmSyntax.Core.Parser.Base.Values as V
import qualified SLIR.HelmSyntax.Core.Parser.Base.Metadata as Meta


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


parseCaseAlt :: Parser E.Expr -> Parser P.CaseAlt
parseCaseAlt parseExpr =
    Meta.trackPosition  go
    where
        go = do
            scn
            pattrn <- parsePattern
            reservedOp "->"
            
            expr <- body
            
            return $ P.CaseAlt pattrn expr
        
        body = L.lineFold scn $ \scn' -> optional scn' *> parseExpr



parsePattern :: Parser P.Pattern
parsePattern = 
    choice
        [ try parseLit
        , try parseRecord
        , try parseList
        , try parseCons
        , try parseTuple
        , try parseVar
        , try parseWildcard
        , parseCon
        ]


parseLit :: Parser P.Pattern
parseLit =
    Meta.trackPosition go
    where
        go = V.parseLiteral <**> return P.Lit


parseRecord :: Parser P.Pattern
parseRecord =
    Meta.trackPosition go
    where
        go = go' <**> return P.Record
        go' = braces (ID.parseLow `sepBy` reservedOp ",")


parseList :: Parser P.Pattern
parseList =
    Meta.trackPosition go
    where
        go = sugared <**> return P.List
        
        sugared = brackets (parsePattern `sepBy` reservedOp ",")


parseCons :: Parser P.Pattern
parseCons =
    Meta.trackPosition go
    where
        go = parens cons

        cons = do
            xs <- some (try (parsePattern <* reservedOp "::"))
            
            rest <- (reservedOp "[]" *> return Nothing) <|> (parsePattern <**> return Just)

            return (P.Cons xs rest)







parseTuple :: Parser P.Pattern
parseTuple =
    Meta.trackPosition go
    where
        go = parens (parsePattern `divBy` reservedOp ",") <**> return P.Tuple


parseCon :: Parser P.Pattern
parseCon =
    Meta.trackPosition go
    where
        go = do
            name <- ID.parseBig
            args <- optional (some parsePattern) <**> return (fromMaybe [])
            return $ P.Con name args


parseVar :: Parser P.Pattern
parseVar =
    Meta.trackPosition go
    where
        go = ID.parseLow <**> return P.Var


parseWildcard :: Parser P.Pattern
parseWildcard =
    Meta.trackPosition go
    where
        go = reservedOp "_" *> return P.Wildcard








