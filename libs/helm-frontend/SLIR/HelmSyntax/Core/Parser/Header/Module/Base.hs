{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Parser.Header.Module.Base (
      parseEntry
    , parseExplicitEntries
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





-- | Misc. Utils
--
parseExplicitEntries :: Parser [Header.Entry]
parseExplicitEntries =
    parens (parseEntry `sepBy` reservedOp ",")



-- | Base
--
parseEntry :: Parser Header.Entry
parseEntry =
    choice
        [ try parseValueEntry
        , parseUnionEntry
        ]



parseValueEntry :: Parser Header.Entry
parseValueEntry = lowerIdentifier <**> return (Header.ValueEntry . Text.pack)


parseUnionEntry :: Parser Header.Entry
parseUnionEntry = do
    name <- upperIdentifier <**> return Text.pack
    cons <- parseUnionExposing
    
    return $ Header.UnionEntry name cons





-- *
-- | Internal Helpers
-- *
parseUnionExposing :: Parser Header.UnionExposing
parseUnionExposing =
    choice
        [ try unionExposingEverything
        , unionExposingExplicit
        ]

unionExposingEverything :: Parser Header.UnionExposing
unionExposingEverything =
    reservedOp "(..)" *> return Header.UnionEverything


unionExposingExplicit :: Parser Header.UnionExposing
unionExposingExplicit =
    go <**> return Header.UnionExplicit
    where
        xs = parens (upperIdentifier `sepBy` reservedOp ",") <**> return (map Text.pack)
        
        go = optional xs <**> return (fromMaybe [])




