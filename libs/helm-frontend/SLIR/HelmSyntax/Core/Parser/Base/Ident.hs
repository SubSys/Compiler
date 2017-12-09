{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
module SLIR.HelmSyntax.Core.Parser.Base.Ident (
      parseLow
    , parseBig
    , parseSym
    , parseNamespace
) where


-- *
import Core
import Core.Control.Flow
import Core.List.Util as ListUtil

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as C

--- Frameworks
import Framework.Parser


--- Local
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

-- ~~ Metadata
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta
-- *


parseLow :: Parser ID.Low
parseLow =
    Meta.trackPosition go
    where
        go = do
            namespace <- optional (try parseNamespacePrefix)
            name <- lowerIdentifier <**> return Text.pack
            
            return $ ID.Low name namespace 




parseBig :: Parser ID.Big
parseBig =
    Meta.trackPosition go
    where
        go = do
            namespace <- optional  (try parseNamespacePrefix)
            name <- upperIdentifier <**> return Text.pack
            
            return $ ID.Big name namespace



parseSym :: Parser ID.Sym
parseSym =
    Meta.trackPosition go
    where
        go = do
            
            op <- opIdentifier <**> return Text.pack
            
            return $ ID.Sym op Nothing 


parseNamespace :: Parser ID.Namespace
parseNamespace =
    go <**> return ID.Namespace
    where
        names :: Parser [String]
        names = upperIdentifier `sepBy1` reservedOp "."
        
        go = map Text.pack <$> names




-- *
-- | Internal
-- *

parseNamespacePrefix :: Parser ID.Namespace
parseNamespacePrefix =
    upperIdentifier `endBy1` reservedOp "." <**> return (ID.Namespace . map Text.pack)







