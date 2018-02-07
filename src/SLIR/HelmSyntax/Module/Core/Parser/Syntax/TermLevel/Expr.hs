{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Module.Core.Parser.Syntax.TermLevel.Expr (
    parseExpr
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    (return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude as Pre

import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable as F


-- + Megaparsec & Related
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

-- + Frameworks
import Framework.Text.Parser

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Metadata          as Meta
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Ident             as ID
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Values            as V
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Types             as T
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.TermLevel.Expr.Helpers as Helper
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.TermLevel.Patterns     as P
-- *





parseExpr :: Parser Decl.Function -> Parser E.Expr
parseExpr f =
    choice
        [ try (parseBinOp f)  -- <?> "expression"
        , try (parseApp f)    -- <?> "expression"
        , try (parseIf f)     -- <?> "expression"
        , try (parseLit f)    -- <?> "expression"
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
        go = ID.parseBig <**> return E.Constr

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
                    return $ \meta -> E.InfixApp op e1 e2 meta
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
            , try (parseParens f)
            , parseTuple f
            ]























