{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Parser.Base.Values (
    parseLiteral
) where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (singleton)

import Prelude (show, read)

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char       as C
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





parseLiteral :: Parser V.LiteralValue
parseLiteral =
    choice
        [ parseChar
        , parseString
        , try parseFloat
        , parseInt
        , parseBool
        ]



parseChar :: Parser V.LiteralValue
parseChar =
    Meta.trackPosition go
    where
        go = (tk *> L.charLiteral <* tk) <**> return (V.Char . Text.pack . singleton)
        tk = C.char '\''

parseString :: Parser V.LiteralValue
parseString =
    Meta.trackPosition go
    where
        go = (tk >> manyTill L.charLiteral tk) <**> return (V.String . Text.pack)
        tk = C.char '"'


parseInt :: Parser V.LiteralValue
parseInt =
    Meta.trackPosition go
    where
        go = lexeme L.decimal <**> return V.Int

parseFloat :: Parser V.LiteralValue
parseFloat =
    Meta.trackPosition go
    where
        go = go' <**> return V.Float
        
        go' :: Parser Double
        go' = do
            x <- some C.digitChar
            C.char '.'
            y <- some C.digitChar
            
            -- TODO:
            -- ?...
            return $ read $ x ++ "." ++ y




parseBool :: Parser V.LiteralValue
parseBool =
    Meta.trackPosition go
    where
        go = try trueCon <|> falseCon
        
        trueCon = reservedWord "True"  *> return (V.Bool True)
        falseCon = reservedWord "False"  *> return (V.Bool False)


