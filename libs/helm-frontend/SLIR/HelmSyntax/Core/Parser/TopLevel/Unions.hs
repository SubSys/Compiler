{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Parser.TopLevel.Unions (
    parseUnion
) where


-- *
import Core
import Core.Control.Flow

import Prelude (error)

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Parser.Base.Ident as ID
import qualified SLIR.HelmSyntax.Core.Parser.Base.Types as T
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




parseUnion :: Parser Decl.Union
parseUnion =
    Meta.trackPosition go
    where
        go = (intro \:\ parseConstructor $ scn) <**> return pack
        
        eq = optional (reservedOp "=")
        
        intro = do
            reservedWord "type"
            name <- ID.parseBig
            args <- optional (some ID.parseLow) <**> return (fromMaybe [])
            
            eq
            
            return (name, args)
        
        pack ((name, args), cons)
            | null cons = error "Missing union constructors (TODO: Improve me)"
            | otherwise =
                Decl.Union name args cons


parseConstructor :: Parser Decl.Constructor
parseConstructor = 
    Meta.trackPosition go
    where
        go = L.lineFold scn $ \scn' -> do
            optional eq
            optional pipe
            
            name <- ID.parseBig
            
            args <- optional (some (try (scn' *> T.parseType))) <**> return (fromMaybe [])
            
            return $ \meta -> Decl.Constructor name args meta
        
        eq = optional (reservedOp "=")
        pipe = optional (reservedOp "|")




