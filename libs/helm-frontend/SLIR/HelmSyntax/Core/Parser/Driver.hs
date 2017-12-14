{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Parser.Driver (
      parser
    , runHeaderParser
) where


-- *
import Core
import Core.Control.Flow

import Prelude (IO, String)

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Parser.Driviver.Program as Program
import qualified SLIR.HelmSyntax.Core.Parser.Driviver.Header  as Header


-- HelmSyntax Payload
import qualified SLIR.HelmSyntax.Data.Payload as Payload

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

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Base      as Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Exporting as Export
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Importing as Import


-- ~ Parsers
import qualified SLIR.HelmSyntax.Core.Parser.Base.Types            as T
import qualified SLIR.HelmSyntax.Core.Parser.Base.Etc              as Etc
import qualified SLIR.HelmSyntax.Core.Parser.Base.Ident            as ID
import qualified SLIR.HelmSyntax.Core.Parser.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.Core.Parser.TopLevel.Unions       as Decl
import qualified SLIR.HelmSyntax.Core.Parser.TopLevel.Functions    as Decl
import qualified SLIR.HelmSyntax.Core.Parser.TopLevel.Fixities     as Decl
-- *




type ErrorMessage = Text

-- | Core parser API
--
parser :: IO String -> IO (Either ErrorMessage Payload.Module)
parser source =
    do
        result <- runParser parseModule "" <$> source
    
        case result of
            Left err ->
                return $ Left $ Text.pack (parseErrorPretty err)
            Right payload ->
                return $ Right payload


runHeaderParser :: IO String -> IO (Either ErrorMessage Payload.ModuleHeader)
runHeaderParser source =
    do
        result <- runParser parseHeader "" <$> source
    
        case result of
            Left err ->
                return $ Left $ Text.pack (parseErrorPretty err)
            Right payload ->
                return $ Right payload



-- | Root Parser
--
parseModule :: Parser Payload.Module
parseModule = do
    header <- Header.parseHeader
    program <- Program.parseProgram
    
    return Payload.Module
        { Payload.header = header
        , Payload.program = program
        }


parseHeader :: Parser Payload.ModuleHeader
parseHeader =
    Header.parseHeader

