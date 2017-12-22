{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Parser.Driver (
      runModuleParser
    , runHeaderParser
) where


-- *
import Core
import Core.List.Util (flatten)
import Core.Control.Flow

import Prelude (IO, return, (<$>))

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L


--- Frameworks
import qualified Framework.Parser as Parser (parseErrorPretty, runParser)


--- Local Deps
-- HelmSyntax Payload
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload as Payload
import qualified SLIR.HelmSyntax.Data.Initialization as Init

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
import qualified SLIR.HelmSyntax.AST.Data.Header.Base       as Base
import qualified SLIR.HelmSyntax.AST.Data.Header.ImportDecl as Decl
-- ~~ Metadata
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

--- Local
-- ~ Sub Parsers
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Ident         as ID
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Metadata      as Meta

-- ~~ Header - Sub Parsers
import qualified SLIR.HelmSyntax.Core.Module.Parser.Root as Root
-- *




-- | Parse the entire module.
--
runModuleParser :: Init.SourcePath -> IO Init.SourceCode -> IO (Either Text Payload.Module)
runModuleParser path source = do
    result <- Parser.runParser (Root.parseModule path) (Text.unpack path) <$> source
    
    case result of
        Left err ->
            return $ Left $ Text.pack (Parser.parseErrorPretty err)
        
        Right payload ->
            return $ Right payload



-- | Parse just the module header.
-- (Useful for obtaining dependency information…)
--
runHeaderParser :: Init.SourcePath -> IO Init.SourceCode -> IO (Either Text Payload.ModuleHeader)
runHeaderParser path source = do
    result <- Parser.runParser (Root.parseHeader path) (Text.unpack path) <$> source
    case result of
        Left err ->
            return $ Left $ Text.pack (Parser.parseErrorPretty err)
        Right payload ->
            return $ Right payload


