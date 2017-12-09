{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SLIR.HelmSyntax.Internal.AST.Ident where


-- *
import Core
import Data.Data (Data, Typeable)
-- *



data Test = Test
    deriving (Show, Data, Typeable, Eq, Ord)



data Ident = Ident
    { syntacticClass :: SyntacticType
    , semanticClass :: SemanticType
    }


data SyntacticType
    = Sym
    | Low
    | Big

data SemanticType
    = Binder
    | Ref





