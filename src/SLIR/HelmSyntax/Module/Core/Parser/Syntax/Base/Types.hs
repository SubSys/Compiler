{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Types (
    parseType
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
-- *







parseType = choice
    [ try parseString <?> "type"
    , try parseChar   <?> "type"
    , try parseInt    <?> "type"
    , try parseFloat  <?> "type"
    , try parseBool   <?> "type"
    , try parseTuple  <?> "type"
    , try parseList   <?> "type"
    , try parseArr    <?> "type"
    , try parseParens <?> "type"
    , try parseVar    <?> "type"
    , parseUnion      <?> "type"
    ]


parseVar :: Parser T.Type
parseVar =
    Meta.trackPosition (ID.parseLow --> T.Var)



parseTuple :: Parser T.Type
parseTuple =
    Meta.trackPosition (parens (parseType `divBy` reservedOp ",") <**> return T.Tuple)




parseList :: Parser T.Type
parseList =
    Meta.trackPosition go
    where
        go = reservedWord "List" *> parseType <**> return T.List



parseUnion :: Parser T.Type
parseUnion =
    Meta.trackPosition go
    where
        go = do    
            name <- ID.parseBig
            args <- many (try parseType)
            
            return $ T.Union name args




parseArr :: Parser T.Type
parseArr =
    Meta.trackPosition go
    where
        go = parens (parseType `divBy` reservedOp "->") <**> return pack

        pack :: [T.Type] -> Meta.Meta -> T.Type
        pack [x, y] m = T.Arr x y m
        pack (x:xs) m = T.Arr x (pack xs m) m


parseParens :: Parser T.Type
parseParens =
    Meta.trackPosition go
    where
        go = parens parseType <**> return T.Parens





-- *
-- | Literal Types
-- *

parseString :: Parser T.Type
parseString =
    Meta.trackPosition go
    where
        go = reservedWord "String" *> return T.String

parseChar :: Parser T.Type
parseChar =
    Meta.trackPosition go
    where
        go = reservedWord "Char" *> return T.Char

parseInt :: Parser T.Type
parseInt =
    Meta.trackPosition go
    where
        go = reservedWord "Int" *> return T.Int

parseFloat :: Parser T.Type
parseFloat =
    Meta.trackPosition go
    where
        go = reservedWord "Float" *> return T.Float

parseBool :: Parser T.Type
parseBool =
    Meta.trackPosition go
    where
        go = reservedWord "Bool" *> return T.Bool



