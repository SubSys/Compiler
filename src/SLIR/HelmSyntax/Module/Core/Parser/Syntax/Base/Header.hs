{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Module.Core.Parser.Syntax.Base.Header (
    parseEntries
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
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Header   as Header

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



{-# ANN module "HLint: ignore" #-}





parseEntries :: Parser Header.Entries
parseEntries =
    choice
        [ try (explicitItems <**> return Header.Explicit)
        , try (explicitItem  <**> return (Header.Explicit . singleton))
        , try (everything      *> return Header.Everything)
        , explicitNothing      *> return (Header.Explicit [])
        ]
    
    where
        singleton x = [x]
        
        explicitItems =
            parens ((scn *> parseEntry <* scn) `divBy` (scn *> reservedOp ","))
        
        explicitItem =
            parens (scn *> parseEntry <* scn)
        
        explicitNothing = do
            reservedOp "("
            -- scn
            reservedOp ")"
        
        everything =
            reservedOp "(..)"
        




parseEntry :: Parser Header.Entry
parseEntry =
    go
    where
        go =
            choice
                [ lowEntryParser
                , symEntryParser
                , unionEntryParser
                ]



-- *
-- | Entry Variations
-- *

lowEntryParser :: Parser Header.Entry
lowEntryParser = 
    go <**> return (Header.ValueEntry False)
    where
        go = ident2Text <$> ID.parseLow


symEntryParser :: Parser Header.Entry
symEntryParser =
    go <**> return (Header.ValueEntry True)

    where
        go = reservedWord "(" *> (ident2Text <$> ID.parseSym) <* reservedWord ")"


unionEntryParser :: Parser Header.Entry
unionEntryParser =
    go
    
    where
        go = do
            unionName <- ident2Text <$> ID.parseBig
            unionExposing <- optional parseUnionExposing
            
            return $ Header.UnionEntry unionName unionExposing



parseUnionExposing :: Parser Header.UnionExposing
parseUnionExposing =
    try con2 <|> con1

    where
        con1 = reservedOp "(..)" *> return Header.UnionEverything
        con2 = con2'           <**> return Header.UnionExplicit
        
        con2' = parens ((ident2Text <$> ID.parseBig) `divBy` reservedOp ",")






-- *
-- | Internal Helpers
-- *




ident2Text :: ID.Ident -> Text
ident2Text (ID.Ident txt _ _) = txt






