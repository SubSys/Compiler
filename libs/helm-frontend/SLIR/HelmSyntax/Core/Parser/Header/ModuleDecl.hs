{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- | Module (Header) Declaration
-- E.g. `module Main exposing (..)`
module SLIR.HelmSyntax.Core.Parser.Header.ModuleDecl (
    parseModuleDecl
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



{-# ANN module "HLint: ignore" #-}


-- | Parse module declaration.
-- E.g.
-- * `module Main exposing (..)`
parseModuleDecl :: Parser (ID.Namespace, Base.Entries)
parseModuleDecl =
    go <* scn
    where
        go = do
            reservedWord "module"

            name <- ID.parseNamespace

            rest <- (withExposing <?> "exposing (a, b, c)")

            case rest of
                Nothing ->
                    return (name, Base.Everything)
                
                Just entries ->
                    return (name, entries)


        withExposing = L.lineFold scn $ \scn' -> do
            optional (try scn')
            proceed <- optional (reservedWord "exposing")
            case proceed of
                Nothing -> return Nothing
                _ ->  do
                    optional (try scn')
                    entries <- Base.parseEntries <?> "exposing items (E.g. `exposing (a, b, c)`)"
                    
                    return $ Just entries


