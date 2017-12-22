{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Core.Module.Parser.TermLevel.Expressions where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (singleton)

import qualified Data.Set  as Set
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Module.Parser.TermLevel.Expressions.Helpers as Helper

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

-- ~~ Metadata
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta
-- *




parseExpr :: Parser Decl.Function -> Parser E.Expr
parseExpr f =
    choice
        [ try (parseBinOp f)  -- <?> "expression"
        , try (parseApp f)    -- <?> "expression"
        , try (parseIf f)     -- <?> "expression"
        , try (parseLit f)    -- <?> "expression"
        , try (parseRecord f) -- <?> "expression"
        , try (parseTuple f)  -- <?> "expression"
        , try (parseList f)   -- <?> "expression"
        , try (parseCon f)    -- <?> "expression"
        , try (parseLet f)    -- <?> "expression"
        , try (parseCase f)   -- <?> "expression"
        , try (parseParens f) -- <?> "expression"
        , parseVar f          -- <?> "expression"
        ]


parseVar :: Parser Decl.Function -> Parser E.Expr
parseVar f =
    Meta.trackPosition go
    where
        go = ID.parseLow <**> return E.Var

parseLit :: Parser Decl.Function -> Parser E.Expr
parseLit f =
    Meta.trackPosition go
    where
        go = V.parseLiteral <**> return E.Lit


parseRecord :: Parser Decl.Function -> Parser E.Expr
parseRecord f =
    Meta.trackPosition go
    where
        go = go' <**> return E.Record
        go' = braces (field `sepBy1` reservedOp ",")
        
        field = do
            id' <- ID.parseLow
            reservedOp "="
            expr <- parseExpr f
            return (id', expr)


parseTuple :: Parser Decl.Function -> Parser E.Expr
parseTuple f =
    Meta.trackPosition go
    where
        go = parens (parseExpr f `divBy` reservedOp ",") <**> return E.Tuple

parseList :: Parser Decl.Function -> Parser E.Expr
parseList f =
    Meta.trackPosition go
    where
        go = try xs <|> emptyList
        
        xs = brackets (parseExpr f `sepBy1` reservedOp ",") <**> return E.List
        emptyList = reservedOp "[]" *> return (E.List [])


parseCon :: Parser Decl.Function -> Parser E.Expr
parseCon f =
    Meta.trackPosition go
    where
        go = ID.parseBig <**> return E.Con

parseIf :: Parser Decl.Function -> Parser E.Expr
parseIf f =
    Meta.trackPosition go
    where
        go = do
            ifBranch <- Helper.ifBranch (parseExpr f)
            
            scn
            
            elseIfBranches <- Helper.elseIfBranches (parseExpr f)
            
            scn
            
            elseBranch <- Helper.elseBranch (parseExpr f)
            
            return $ E.If (ifBranch : elseIfBranches) elseBranch




parseLet :: Parser Decl.Function -> Parser E.Expr
parseLet f =
    Meta.trackPosition go
    where
        go = do
            decls <- intro
            scn
            
            expr <- outro
            
            return $ E.Let decls expr
        
        intro = snd <$> (reservedWord "let" \:\ f $ scn)
        outro = reservedWord "in" *> L.lineFold scn (\scn' -> optional scn' *> parseExpr f)



parseCase :: Parser Decl.Function -> Parser E.Expr
parseCase f = 
    Meta.trackPosition go
    where
        go = L.indentBlock scn p >>= pack
        
        p = do
            header <- intro
            
            return (L.IndentMany Nothing (return . (header, )) outro)


        intro = reservedWord "case" *> parseExpr f <* reservedWord "of"
        outro = P.parseCaseAlt (parseExpr f)
        
        pack (con, alts)
            | not (null alts) =
                return (E.Case con alts)

            | otherwise =
                fancyFailure $ Set.fromList
                    [ ErrorFail "Missing case alternatives."
                    , ErrorFail "TODO: What to (actually) say when missing case alternatives?"
                    ]





-- parseRecordUpdate :: Parser Decl.Function -> Parser E.Expr
-- parseRecordUpdate f =

-- parseRecordAccess :: Parser Decl.Function -> Parser E.Expr
-- parseRecordAccess f =


parseParens :: Parser Decl.Function -> Parser E.Expr
parseParens f =
    Meta.trackPosition go
    where
        go = parens (parseExpr f) <**> return E.Parens


-- parseAbs :: Parser Decl.Function -> Parser E.Expr
-- parseAbs f =


parseBinOp :: Parser Decl.Function -> Parser E.Expr
parseBinOp f =
    Meta.trackPosition go
    where
        go = L.lineFold scn $ \scn' -> do
            e1 <- item
            
            optional scn'
            
            sc
            
            op <- ID.parseSym
            
            
            sc
            
            optional scn'
            
            e2 <- item
            
            sc
            
            rests <- optional (some (rest scn'))
            
            case rests of
                Nothing ->
                    return $ \meta -> E.BinOp op e1 e2 meta
                Just xs ->
                    return $ \meta -> Helper.chainedBinOps meta e1 ((op, e2) : xs)
        
        rest scn_ = do
            
            optional (try scn_)
            
            op <- ID.parseSym
            
            optional (try scn_)
            
            e2 <- item
            
            return (op, e2)
        
        
        item = choice
            [ try (parseLit f)
            , try (parseVar f)
            , try (parseCon f)
            , try (parseList f)
            , try (parseRecord f)
            , try (parseTuple f)
            , parseParens f
            ]




parseApp :: Parser Decl.Function -> Parser E.Expr
parseApp f =
    Meta.trackPosition go
    where
        go = L.lineFold scn $ \scn' -> do
            e1 <- expr
            
            args <- some (try (optional scn' *> arg))
            
            scn

            return $ \meta -> Helper.appsToTree meta (e1 : args)
        
        expr = choice
            [ parseVar f
            , parseCon f
            , parseParens f
            ]
        
        arg = choice
            [ try (parseLit f)
            , try (parseVar f)
            , try (parseCon f)
            , try (parseList f)
            , try (parseVar f)
            , try (parseRecord f)
            , try (parseParens f)
            , parseTuple f
            ]

