{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Core.Parser.Header.ImportDecl (
    parseImportDecl
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

import qualified SLIR.HelmSyntax.Core.Parser.Header.Base        as Base
-- *




-- | Parse import declaration.
-- E.g.
-- * `import Sample.Lib`
-- * `import Sample.Lib as Lib`
-- * `import Sample.Lib exposing (x, y, z)`
-- * `import Sample.Lib as Lib exposing (x, y , z)`
--
parseImportDecl :: Parser Decl.ImportDecl
parseImportDecl = do
    reservedWord "import"
    name             <- ID.parseNamespace
    asName           <- optional asName
    explicitExposing <- optional (try parseExplicitExposing)
    
    scn
    return
        $ Decl.ImportDecl name asName explicitExposing




parseExplicitExposing :: Parser Base.Entries
parseExplicitExposing =
    L.lineFold scn $ \scn' -> do
        -- optional scn'
        reservedWord "exposing"
        -- optional scn'
        Base.parseEntries



-- *
-- | Internal Helpers
-- *


asName :: Parser ID.Big
asName =
    reservedWord "as" *> ID.parseBig


