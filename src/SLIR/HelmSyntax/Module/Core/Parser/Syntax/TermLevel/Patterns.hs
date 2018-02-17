{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Module.Core.Parser.Syntax.TermLevel.Patterns (
    parseCaseAlt
  , parsePattern
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



parseCaseAlt :: Parser E.Expr -> Parser P.CaseAlt
parseCaseAlt parseExpr =
    Meta.trackPosition  go
    where
        go = do
            scn
            pattrn <- parsePattern
            reservedOp "->"
            
            expr <- body
            
            return $ P.CaseAlt pattrn expr
        
        body = L.lineFold scn $ \scn' -> optional scn' *> parseExpr



parsePattern :: Parser P.Pattern
parsePattern = 
    choice
        [ try parseLit
        , try parseList
        , try parseCons
        , try parseTuple
        , try parseVar
        , try parseWildcard
        , parseCon
        ]


parseLit :: Parser P.Pattern
parseLit =
    Meta.trackPosition go
    where
        go = V.parseLiteral <**> return P.Lit


parseList :: Parser P.Pattern
parseList =
    Meta.trackPosition go
    where
        go = sugared <**> return P.List
        
        sugared = brackets (parsePattern `sepBy` reservedOp ",")


parseCons :: Parser P.Pattern
parseCons =
    Meta.trackPosition go
    where
        go = parens cons

        cons = do
            xs <- some (try (parsePattern <* reservedOp "::"))
            
            rest <- (reservedOp "[]" *> return Nothing) <|> (parsePattern <**> return Just)

            return (P.ListCons xs rest)







parseTuple :: Parser P.Pattern
parseTuple =
    Meta.trackPosition go
    where
        go = parens (parsePattern `divBy` reservedOp ",") <**> return P.Tuple


parseCon :: Parser P.Pattern
parseCon =
    Meta.trackPosition go
    where
        go = do
            name <- ID.parseBig
            args <- optional (some parsePattern) <**> return (fromMaybe [])
            return $ P.Constr name args


parseVar :: Parser P.Pattern
parseVar =
    Meta.trackPosition go
    where
        go = ID.parseLow <**> return (P.Var . Etc.Binder_)



parseWildcard :: Parser P.Pattern
parseWildcard =
    Meta.trackPosition go
    where
        go = reservedOp "_" *> return P.Wildcard

