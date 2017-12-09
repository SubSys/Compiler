{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Core.Parser.Driviver.Program (
    parseProgram
) where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (singleton)

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Parser.Base.Ident         as ID

import qualified SLIR.HelmSyntax.Core.Parser.TopLevel.Unions       as Decl
import qualified SLIR.HelmSyntax.Core.Parser.TopLevel.Functions    as Decl
import qualified SLIR.HelmSyntax.Core.Parser.TopLevel.Fixities     as Decl


-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Payload as Payload

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
-- *



{-# ANN module "HLint: ignore" #-}



parseProgram :: Parser Payload.Program
parseProgram = do
    decls <- someTill parseDecl eof
    
    return Payload.Program
        { Payload.functions = getFunctions decls
        , Payload.unions = getUnions decls
        , Payload.fixities = getInfixs decls
        }


parseDecl :: Parser TopLevelDecl
parseDecl =
    choice
        [ decl function <?> "function declaration"
        , decl union <?> "union declaration"
        , hidden (decl infixer)
        ]

    where
        -- decl x = scn *> x <* scn
        decl x = scn *> x <* scn
        
        function = Decl.parseFunction <**> return Function
        union = Decl.parseUnion <**> return Union
        infixer = Decl.parseInfix <**> return Infix




-- isFunction :: Parser Bool
-- isFunction = lowerIdentifier




-- *
-- | Internal (Tmp.)
-- *
data TopLevelDecl
    = Function Decl.Function
    | Infix Decl.Infix
    | Union Decl.Union
    deriving (Show)



getFunctions :: [TopLevelDecl] -> [Decl.Function]
getFunctions =
    mapMaybe get
    where
        get (Function x) = Just x
        get _ = Nothing

getInfixs :: [TopLevelDecl] -> [Decl.Infix]
getInfixs =
    mapMaybe get
    where
        get (Infix x) = Just x
        get _ = Nothing

getUnions :: [TopLevelDecl] -> [Decl.Union]
getUnions =
    mapMaybe get
    where
        get (Union x) = Just x
        get _ = Nothing



