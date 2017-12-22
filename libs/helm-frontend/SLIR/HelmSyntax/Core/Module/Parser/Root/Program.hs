{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Core.Module.Parser.Root.Program (
    parseProgram
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
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload as Payload
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
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Ident         as ID
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Metadata      as Meta

-- ~~ Header - Sub Parsers
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Ident            as ID
import qualified SLIR.HelmSyntax.Core.Module.Parser.TopLevel.Unions       as Decl
import qualified SLIR.HelmSyntax.Core.Module.Parser.TopLevel.Functions    as Decl
import qualified SLIR.HelmSyntax.Core.Module.Parser.TopLevel.Fixities     as Decl
-- *



{-# ANN module "HLint: ignore" #-}



parseProgram :: Parser Payload.Program
parseProgram = do
    decls <- manyTill parseDecl eof
    
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







