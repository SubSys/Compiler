{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Core.Parser.Header.Module.Importing (
      parseModuleImporting
    , parseImportDecl
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Parser.Base.Ident         as ID
import qualified SLIR.HelmSyntax.Core.Parser.Base.Values        as V
import qualified SLIR.HelmSyntax.Core.Parser.TermLevel.Patterns as P

import qualified SLIR.HelmSyntax.Core.Parser.Header.Module.Base as Header
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

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Base      as Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Exporting as Export
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Importing as Import

-- ~~ Metadata
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta
-- *





parseModuleImporting :: Parser Import.ModuleImporting
parseModuleImporting =
    go <**> return Import.ModuleImporting
    where
        go = many (try item)
        
        item = scn *> parseImportDecl <* scn



parseImportDecl :: Parser Import.ImportDecl
parseImportDecl =
    choice
        [ try parseExplicitImport
        , try parseEverythingImport
        , parseQualifiedImport
        ]


parseQualifiedImport :: Parser Import.ImportDecl
parseQualifiedImport = do
    reservedWord "import"
    name <- ID.parseNamespace
    optAlias <- optional asName
    
    return $ Import.Qualified name optAlias

    where
        asName = reservedWord "as" *> ID.parseBig



parseExplicitImport :: Parser Import.ImportDecl
parseExplicitImport = do
    reservedWord "import"
    name <- ID.parseNamespace
    
    reservedWord "exposing"
    
    entries <- Header.parseExplicitEntries
    
    return $ Import.Explicit name entries


parseEverythingImport :: Parser Import.ImportDecl
parseEverythingImport = do
    reservedWord "import"
    name <- ID.parseNamespace
    reservedWord "exposing"
    reservedOp "(..)"
    
    return $ Import.Everything name
    
    

