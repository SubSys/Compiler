{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Core.Parser.TermLevel.Expressions.Helpers (
      ifBranch
    , elseIfBranches
    , elseBranch
    , appsToTree
    , chainedBinOps
) where


-- *
import Core
import Core.Control.Flow

import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as L


--- Frameworks
import Framework.Parser


--- Local
import qualified SLIR.HelmSyntax.Core.Parser.Base.Ident         as ID
import qualified SLIR.HelmSyntax.Core.Parser.Base.Values        as V
import qualified SLIR.HelmSyntax.Core.Parser.TermLevel.Patterns as P
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




{-# ANN module "HLint: ignore" #-}






-- | If Branches
--
ifBranch :: Parser E.Expr -> Parser (E.Expr, E.Expr)
ifBranch exprParser = do 
    con <- intro
    expr <- try outro
    return (con, expr)
    where
        intro = reservedWord "if" *> exprParser <* reservedWord "then"
        outro = L.lineFold scn $ \scn' -> optional scn' *> exprParser


elseIfBranches :: Parser E.Expr -> Parser [(E.Expr, E.Expr)]
elseIfBranches e =
    optional (some $ branch) <**> return (fromMaybe [])
    where
        branch = scn *> elseIfBranch e <* scn


elseIfBranch :: Parser E.Expr -> Parser (E.Expr, E.Expr)
elseIfBranch exprParser = do 
    con <- intro
    expr <- outro
    return (con, expr)
    where
        intro = reservedWord "else if" *> exprParser <* reservedWord "then"
        outro = L.lineFold scn $ \scn' -> optional scn' *> exprParser



elseBranch :: Parser E.Expr -> Parser E.Expr
elseBranch exprParser =
    reservedWord "else" *> body
    where
        body = L.lineFold scn $ \scn' -> optional scn' *> exprParser










appsToTree :: Maybe Meta.Meta -> [E.Expr] -> E.Expr
appsToTree m [x] = x
appsToTree m [x, y] = (x `E.App` y) m

-- | Curry Expr Nodes
appsToTree m (x:y:zs) =
    let
        a = (x `E.App` y) m
    in
        appsToTree m (a : zs)



-- | Default - Resolve after parsing.
chainedBinOps :: Maybe Meta.Meta -> E.Expr -> [(ID.Sym, E.Expr)] -> E.Expr
chainedBinOps m e1 [(op, e2)] =
    E.BinOp op e1 e2 Nothing

chainedBinOps m e1 ((op, e2):es) =
    chainedBinOps m (parens e1 op e2) es
    
    where
        
        parens e1 op e2 =
            E.BinOp op e1 e2 Nothing








