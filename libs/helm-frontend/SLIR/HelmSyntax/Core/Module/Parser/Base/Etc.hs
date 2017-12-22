{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Parser.Base.Etc (
    parseSignature
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Text as Text


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Ident as ID
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Types as T
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

-- ~~ Metadata
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta
-- *



parseSignature :: Parser Etc.Signature
parseSignature =
    Meta.trackPosition go
    where
        go =
            (intro *> outro <?> "type signature") --> (Etc.Unresolved . pack)
        
        intro = name *> (reservedOp ":" <?> "type signature")
        outro = T.parseType `sepBy1` reservedOp "->"
        

        pack [x]    = x
        pack [x, y] = T.Arr x y Nothing
        pack (x:xs) = T.Arr x (pack xs) Nothing
    
        name = try alphaName <|> symName
        
        alphaName = ID.parseLow <**> return Left
        symName = (reservedWord "(" *> ID.parseSym <* reservedWord ")") <**> return Right




