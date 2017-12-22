{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Parser.Header.Base where


-- *
import Core
import Core.Control.Flow

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Ident         as ID
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Values        as V
import qualified SLIR.HelmSyntax.Core.Module.Parser.TermLevel.Patterns as P
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

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Base       as Base
import qualified SLIR.HelmSyntax.AST.Data.Header.ImportDecl as Decl

-- ~~ Metadata
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta
-- *




{-# ANN module "HLint: ignore" #-}






parseEntries :: Parser Base.Entries
parseEntries =
    choice
        [ try (explicitItems <**> return Base.Explicit)
        , try (explicitItem  <**> return (Base.Explicit . singleton))
        , try (everything      *> return Base.Everything)
        , explicitNothing      *> return (Base.Explicit [])
        ]
    
    where
        singleton x = [x]
        
        explicitItems =
            parens ((scn *> parseEntry <* scn) `divBy` (scn *> reservedOp ","))
        
        explicitItem =
            parens (scn *> parseEntry <* scn)
        
        explicitNothing = do
            reservedOp "("
            -- scn
            reservedOp ")"
        
        everything =
            reservedOp "(..)"
        




parseEntry :: Parser Base.Entry
parseEntry =
    go
    where
        go =
            choice
                [ lowEntryParser
                , symEntryParser
                , unionEntryParser
                ]



-- *
-- | Entry Variations
-- *

lowEntryParser :: Parser Base.Entry
lowEntryParser = 
    go <**> return (Base.ValueEntry False)
    where
        go = low2Text <$> ID.parseLow


symEntryParser :: Parser Base.Entry
symEntryParser =
    go <**> return (Base.ValueEntry True)

    where
        go = reservedWord "(" *> (sym2Text <$> ID.parseSym) <* reservedWord ")"


unionEntryParser :: Parser Base.Entry
unionEntryParser =
    go
    
    where
        go = do
            unionName <- big2Text <$> ID.parseBig
            unionExposing <- optional parseUnionExposing
            
            return $ Base.UnionEntry unionName unionExposing



parseUnionExposing :: Parser Base.UnionExposing
parseUnionExposing =
    try con2 <|> con1

    where
        con1 = reservedOp "(..)" *> return Base.UnionEverything
        con2 = con2'           <**> return Base.UnionExplicit
        
        con2' = parens ((big2Text <$> ID.parseBig) `divBy` reservedOp ",")


-- *
-- | Internal Helpers
-- *




low2Text :: ID.Low -> Text
low2Text (ID.Low txt _ _) = txt

sym2Text :: ID.Sym -> Text
sym2Text (ID.Sym txt _ _) = txt

big2Text :: ID.Big -> Text
big2Text (ID.Big txt _ _) = txt

-- extractText :: Either ID.Sym ID.Low -> Text
-- extractText (Left  (ID.Sym txt _ _)) = txt
-- extractText (Right (ID.Low txt _ _)) = txt

