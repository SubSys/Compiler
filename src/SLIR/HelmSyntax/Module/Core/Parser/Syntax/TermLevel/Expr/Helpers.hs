{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
module SLIR.HelmSyntax.Module.Core.Parser.Syntax.TermLevel.Expr.Helpers (
    ifBranch
  , elseIfBranches
  , elseBranch
  , appsToTree
  , chainedBinOps
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
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Metadata as Meta
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Ident    as ID
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Values   as V
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Types    as T
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










appsToTree :: Meta.Meta -> [E.Expr] -> E.Expr
appsToTree m [x] = x
appsToTree m [x, y] = (x `E.App` y) m

-- | Curry Expr Nodes
appsToTree m (x:y:zs) =
    let
        a = (x `E.App` y) m
    in
        appsToTree m (a : zs)



-- | Default - Resolve after parsing.
chainedBinOps :: Meta.Meta -> E.Expr -> [(ID.Ident, E.Expr)] -> E.Expr
chainedBinOps m e1 [(op, e2)] =
    E.InfixApp op e1 e2 Meta.Empty

chainedBinOps m e1 ((op, e2):es) =
    chainedBinOps m (parens e1 op e2) es
    
    where
        
        parens e1 op e2 =
            E.InfixApp op e1 e2 Meta.Empty




