{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Core.Parser.Root.Header (
    parseModuleHeader
) where


-- *
import Core
import Core.List.Util (flatten)
import Core.Control.Flow

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L


--- Frameworks
import Framework.Parser


--- Local Deps
-- HelmSyntax Payload
import qualified SLIR.HelmSyntax.Data.Payload as Payload
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
import qualified SLIR.HelmSyntax.Core.Parser.Base.Ident         as ID
import qualified SLIR.HelmSyntax.Core.Parser.Base.Metadata      as Meta

-- ~~ Header - Sub Parsers
import qualified SLIR.HelmSyntax.Core.Parser.Header.Base        as Base
import qualified SLIR.HelmSyntax.Core.Parser.Header.ModuleDecl  as ModuleDecl
import qualified SLIR.HelmSyntax.Core.Parser.Header.ImportDecl  as ImportDecl
-- *



parseModuleHeader :: Init.SourcePath -> Parser Payload.ModuleHeader
parseModuleHeader path = do
    (moduleName, exports) <- ModuleDecl.parseModuleDecl <?> "module declaration"

    imports <- many (ImportDecl.parseImportDecl <?> "import declaration")
    
    return
        Payload.ModuleHeader
            { Payload.moduleName = moduleName
            , Payload.exports    = exports
            , Payload.imports    = imports
            , Payload.modulePath = path
            }





