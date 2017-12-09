{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Core.Parser.Header.Module.Exporting (
    parseModuleExporting
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






-- *
-- | Internal
-- *


parseModuleExporting :: Parser Export.ModuleExporting
parseModuleExporting =
        (try intro *> outro)
    <|> systemDefault
    where
        systemDefault = return Export.Everything
        
        intro = reservedWord "exposing"
        outro = choice
            [ try everything
            , explicits
            ]

        
        explicits :: Parser Export.ModuleExporting
        explicits = Header.parseExplicitEntries <**> return Export.Explicit
        
        everything :: Parser Export.ModuleExporting
        everything = reservedOp "(..)" *> return Export.Everything


