{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core.Module.Parser.TopLevel.Functions (
    parseFunction
) where


-- *
import Core
import Core.Control.Flow

import Data.Monoid ((<>))

import qualified Data.Set  as Set
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Types            as T
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Etc              as Etc
import qualified SLIR.HelmSyntax.Core.Module.Parser.Base.Ident            as ID
import qualified SLIR.HelmSyntax.Core.Module.Parser.TermLevel.Expressions as E
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



{-# ANN module "HLint: ignore" #-}




parseFunction :: Parser Decl.Function
parseFunction =
    choice
        [ try parseOpDecl
        , parseFnDecl 
        ] <?> "function declaration"



parseOpDecl :: Parser Decl.Function
parseOpDecl =
    Meta.trackPosition go
    where
        go = do
            signature <- optional (try Etc.parseSignature <?> "type signature")
            scn
            
            
            name <- reservedWord "(" *> ID.parseSym <* reservedWord ")"
            
            args <- parseArgs
            
            reservedOp "="
            
            expr <- parseBody
            
            return $ Decl.OpDecl name args expr signature


parseFnDecl :: Parser Decl.Function
parseFnDecl =
    Meta.trackPosition go
    where
        go = do
            signature <- optional (try Etc.parseSignature <?> "type signature")
            scn
            name <- ID.parseLow <?> "function declaration"
            
            args <- parseArgs <?> "function declaration"
            
            reservedOp "=" <?> "function declaration"
            
            -- TODO: Add error handler
            expr <- parseBody <?> "expression"
            
            -- case body of
            --     Right expr -> return $ Decl.FnDecl name args expr (Just signature)
            --     Left err -> unexpected err
                    -- return $
                    --     Decl.FnDecl
                    --         (ID.Low (Text.pack "...") Nothing)
                    --         []
                    --         (E.Var (ID.Low (Text.pack "..") Nothing))
                    --         Nothing
            
            return $ Decl.FnDecl name args expr signature
            




-- *
-- | Internal Helpers
-- *

parseArgs :: Parser [ID.Low]
parseArgs =
    optional (some ID.parseLow) <**> return (fromMaybe [])


parseBody :: Parser E.Expr
parseBody =
    L.lineFold scn (\scn' -> optional scn' *> E.parseExpr parseFnDecl) <* sc

