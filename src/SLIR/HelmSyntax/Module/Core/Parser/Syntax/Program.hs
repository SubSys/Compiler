{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Module.Core.Parser.Syntax.Program (
    parseProgram
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

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

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
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Metadata      as Meta
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Ident         as ID
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Values        as V
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Types         as T
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.Module.Core.Parser.Syntax.TopLevel.Unions    as Decl
-- *









{-# ANN module "HLint: ignore" #-}



parseProgram :: Parser I.Program
parseProgram = do
    decls <- manyTill parseDecl eof
    
    return I.Program
        { I.functions = getFunctions decls
        , I.unions = getUnions decls
        , I.fixities = getInfixs decls
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















