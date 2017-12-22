{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Parser.TopLevel.Fixities (
    parseInfix
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L


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



parseInfix :: Parser Decl.Infix
parseInfix =
    choice
        [ try parseInfixL
        , try parseInfixR
        , parseInfixN
        ]


parseInfixL :: Parser Decl.Infix
parseInfixL =
    Meta.trackPosition go
    where
        go = do
            val <- reservedWord "infixl"
            
            prec <- parseOpPrecedence
            sym <- ID.parseSym
            
            return $ Decl.InfixL sym prec



parseInfixR :: Parser Decl.Infix
parseInfixR =
    Meta.trackPosition go
    where
        go = do
            val <- reservedWord "infixr"
            
            prec <- parseOpPrecedence
            
            sym <- ID.parseSym
            
            return $ Decl.InfixR sym prec


parseInfixN :: Parser Decl.Infix
parseInfixN =
    Meta.trackPosition go
    where
        go = do
            val <- reservedWord "infix"
            
            prec <- parseOpPrecedence
            
            sym <- ID.parseSym
            
            return $ Decl.InfixN sym prec




-- *
-- | Misc. Helpers
-- *

parseOpPrecedence =
    Meta.trackPosition go
    where
        go = lexeme L.decimal <**> return Decl.OpPrecedence

