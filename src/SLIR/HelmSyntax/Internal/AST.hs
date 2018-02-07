{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SLIR.HelmSyntax.Internal.AST where


-- ~
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

import Data.Data (Data, Typeable)


import qualified Prelude as Pre
import qualified Core.Utils as Core

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

-- + Text Rendering


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable as F

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP
-- ~


{-# ANN module ("HLint: ignore" :: String) #-}



data Ident = Ident Text (Maybe Namespace) Meta
    deriving (Show, Eq, Ord, Data, Typeable)


newtype Namespace = Namespace [Text]
    deriving (Show, Eq, Ord, Data, Typeable)



{-
    # Fixities
-}

data Infix
    = InfixL Ident OpPrecedence Meta
    | InfixR Ident OpPrecedence Meta
    | InfixN Ident OpPrecedence Meta
    deriving (Show, Eq, Ord, Data, Typeable)


data OpPrecedence = OpPrecedence Int Meta
    deriving (Show, Eq, Ord, Data, Typeable)





{-
    # Unions
-}

data Union = Union Ident [Ident] [Constructor] Meta
    deriving (Show, Eq, Ord, Data, Typeable)

data Constructor = Constructor Ident [Type] Meta
    deriving (Show, Eq, Ord, Data, Typeable)





{-
    # Function Declarations
-}

data Function = Function Binder [Binder] Expr Signature Meta
    deriving (Show, Eq, Ord, Data, Typeable)


data Signature
    = Validated Scheme Meta
    | Unresolved Type Meta
    | Unknown
    deriving (Show, Eq, Ord, Data, Typeable)



{-
    # Expression Nodes
-}


data Expr
    = VarExpr Ident Meta
    | LitExpr LiteralValue Meta
    | AbsExpr Binder Expr Meta
    | AppExpr Expr Expr Meta
    | CaseExpr Expr [CaseAlt] Meta
    | LetExpr [Function] Expr Meta
    | IfExpr [(Expr, Expr)] Expr Meta
    | ParensExpr Expr Meta
    | InfixAppExpr Ident Expr Expr Meta
    | ConstrExpr Ident Meta
    | ListExpr [Expr] Meta
    | TupleExpr [Expr] Meta
    | FunCallExpr Ident [Expr] (Maybe Type) Meta
    | ConCallExpr Ident [Expr] (Maybe Type) Meta
    deriving (Show, Eq, Ord, Data, Typeable)





{-
    # Pattern Nodes
-}


data Pattern
    = VarPattern Binder Meta
    | LitPattern LiteralValue Meta
    | ListPattern [Pattern] Meta
    
    -- | List ‘Cons’ Pattern
    -- Examples:
    -- * `x :: xs` = `ConsPattern [(VarPattern x)] (Just (VarPattern xs))`
    -- * `x :: []` = `ConsPattern [(VarPattern x)] Nothing`
    -- * `x1 :: x2 :: x3 :: []` =
    --          `ConsPattern [(VarPattern x1), (VarPattern x2), (VarPattern x3)] Nothing`
    -- NOTE:
    -- * Regarding the  `(Maybe Pattern)` field
    -- ** Just p == 'rest of the list'
    -- ** Nothing == `[]`/Nil -i.e, end of the list pattern.
    | ListConsPattern [Pattern] (Maybe Pattern) Meta
    | TuplePattern [Pattern] Meta
    | ConstrPattern Ident [Pattern] Meta
    | WildcardPattern Meta
    deriving (Show, Eq, Ord, Data, Typeable)



{-
    ## Case-Alt Node
-}

data CaseAlt = CaseAlt Pattern Expr Meta
    deriving (Show, Eq, Ord, Data, Typeable)




{-
    # Type Nodes
-}

data Type
    = TupleType  [Type] Meta
    | ListType   Type Meta

    | UnionType Ident [Type] Meta
    | VarType Ident Meta
    | ArrType Type Type Meta
    | ParensType Type Meta
    
    | StringType Meta
    | CharType Meta
    | IntType Meta
    | FloatType Meta
    | BoolType Meta
    
    | Superposed (Forall [Ident]) [Type]
    deriving (Show, Eq, Ord, Data, Typeable)


newtype Forall a = Forall a
    deriving (Show, Eq, Ord, Data, Typeable)



data Scheme = Scheme (Forall [Ident]) Type
    deriving (Show, Eq, Ord, Data, Typeable)



{-
    # Base Values
-}

data LiteralValue
    = CharLit Text Meta
    | StringLit Text Meta
    | IntLit Int Meta
    | FloatLit Double Meta
    | BoolLit Bool Meta
    deriving (Show, Eq, Ord, Data, Typeable)






{-
    # Uncategorized - Etc.
-}

data Binder = Binder Ident (Maybe Type)
    deriving (Show, Eq, Ord, Data, Typeable)














{-
    # Header
-}

{-
    ## Header Base
-}


data Entries
    = Everything
    | Explicit [Entry]
    deriving (Show, Data, Typeable)


data Entry
    = ValueEntry IsSym Text
    | UnionEntry Text (Maybe UnionExposing)
    deriving (Show, Data, Typeable)


data UnionExposing
    = UnionEverything
    | UnionExplicit [Text]
    deriving (Show, Data, Typeable)

type IsSym = Bool




{-
    ## Header Import Declarations
-}

data ImportDecl
    = ImportDecl Namespace (Maybe AsName) (Maybe ExplicitExposing)
    deriving (Show, Data, Typeable)

type AsName = Ident

type ExplicitExposing = Entries














{-
    # Metadata Nodes
    > 'Source Code Locations' & Related
-}


data Meta
    = Empty
    | Meta
        { span :: Maybe Span
        , inferredType :: Maybe Type
        
        -- If this is used in the context of a value, and is likewise overloaded,
        -- use this field to record, and resolve the overloaded decl...
        -- NOTE: (At the time of writing this) Currently this is set for all FunCall expressions…
        , overloadedTargetType :: Maybe Type
        }
    
    deriving (Data, Typeable)


data Span = Span Location Location
    deriving (Data, Typeable, Show)

data Location = Location
    { lineNum :: Int
    , columnNum :: Int
    }
    deriving (Data, Typeable, Show)


instance Eq Meta where
    (==) x y = True

instance Ord Meta where
    compare x y = EQ

instance Show Meta where
    show x = ""

-- |
-- If this is used in the context of a value, and is likewise overloaded,
-- record overloaded metadata with this field.
-- data OverloadedTarget = OverloadedTarget
--     { targetType :: Maybe T.Type
--     }


{-
    # Internal - Special AST Forms
-}

data AST_
    = FunctionField_ Binder [Binder] Expr_ Signature Meta

    | VarExpr_ Ident Meta
    | AbsExpr_ Binder Expr_ Meta
    | AppExpr_ Expr_ Expr_ Meta
    | CaseExpr_ Expr_ [CaseAlt_] Meta
    | LetExpr_ [Function_] Expr_ Meta
    
    
    | LitExpr_ LiteralValue Meta
    | IfExpr_ [(Expr_, Expr_)] Expr_ Meta
    | ParensExpr_ Expr_ Meta
    | InfixAppExpr_ Ident Expr_ Expr_ Meta
    | ConstrExpr_ Ident Meta
    | ListExpr_ [Expr_] Meta
    | TupleExpr_ [Expr_] Meta

    | VarPattern_ Binder Meta
    | ListPattern_ [Pattern_] Meta
    | ListConsPattern_ [Pattern_] (Maybe Pattern_) Meta
    | TuplePattern_ [Pattern_] Meta
    | ConstrPattern_ Ident [Pattern_] Meta
    | WildcardPattern_ Meta

    | CaseAlt_ Pattern_ Expr_ Meta


type Expr_ = AST_
type Pattern_ = AST_
type CaseAlt_ = AST_
type Function_ = AST_


data ASTF a
    = FunctionF Binder [Binder] a Signature Meta

    | VarExprF Ident Meta
    | AbsExprF Binder a Meta
    | AppExprF a a Meta
    | CaseExprF a [a] Meta
    | LetExprF [a] a Meta
    
    | LitExprF LiteralValue Meta
    | IfExprF [(a, a)] a Meta
    | ParensExprF a Meta
    | InfixAppExprF Ident a a Meta
    | ConstrExprF Ident Meta
    | ListExprF [a] Meta
    | TupleExprF [a] Meta

    | VarPatternF Binder Meta
    | ListPatternF [a] Meta
    | ListConsPatternF
    | TuplePatternF [a] Meta
    | WildcardPatternF Meta

    | CaseAltF a a Meta
    deriving (Fun.Functor, Fold.Foldable)


type ExprF a = ASTF a
type PatternF a = ASTF a
type CaseAltF a = ASTF a
type FunctionF a = ASTF a


{-
    ## Conversions
-}

-- toAST_ :: Function -> AST_
-- toAST_ (Function name args expr) =



generalizeFunction :: Function -> Function_
generalizeFunction (Function name args body sig meta) =
    FunctionField_ name args (generalizeExpr body) sig meta


generalizeExpr :: Expr -> Expr_
generalizeExpr (VarExpr ident meta) =
    VarExpr_ ident meta

generalizeExpr (AbsExpr ident expr meta) =
    AbsExpr_ ident (generalizeExpr expr) meta

generalizeExpr (AppExpr e1 e2 meta) =
    AppExpr_ (generalizeExpr e1) (generalizeExpr e2) meta


generalizeExpr (CaseExpr con alts meta) =
    CaseExpr_ (generalizeExpr con) (map generalizeCaseAlt alts) meta


generalizeExpr (LetExpr fns expr meta) =
    LetExpr_ (map generalizeFunction fns) (generalizeExpr expr) meta



generalizeExpr (LitExpr value meta) =
    LitExpr_ value meta

generalizeExpr (IfExpr intros elseExpr meta) =
    IfExpr_
        (Core.mapPairs generalizeExpr intros)
        (generalizeExpr elseExpr)
        meta

generalizeExpr (ParensExpr expr meta) =
    ParensExpr_
        (generalizeExpr expr)
        meta

generalizeExpr (InfixAppExpr ident e1 e2 meta) =
    InfixAppExpr_
        ident
        (generalizeExpr e1)
        (generalizeExpr e2)
        meta

generalizeExpr (ConstrExpr ident meta) =
    ConstrExpr_ ident meta

generalizeExpr (ListExpr xs meta) =
    ListExpr_ (map generalizeExpr xs) meta

generalizeExpr (TupleExpr xs meta) =
    TupleExpr_ (map generalizeExpr xs) meta




generalizePattern :: Pattern -> Pattern_
generalizePattern (VarPattern ident meta) =
    VarPattern_ ident meta


generalizePattern (ListPattern xs meta) =
    ListPattern_ (map generalizePattern xs) meta

generalizePattern (ListConsPattern xs Nothing meta) =
    ListConsPattern_ (map generalizePattern xs) Nothing meta

generalizePattern (ListConsPattern xs (Just end) meta) =
    ListConsPattern_ (map generalizePattern xs) (Just $ generalizePattern end) meta

generalizePattern (ConstrPattern ident args meta) =
    ConstrPattern_ ident (map generalizePattern args) meta

generalizePattern (TuplePattern xs meta) =
    TuplePattern_ (map generalizePattern xs) meta

generalizePattern (WildcardPattern meta) =
    WildcardPattern_ meta



generalizeCaseAlt :: CaseAlt -> CaseAlt_
generalizeCaseAlt (CaseAlt patrn expr meta) =
    CaseAlt_
        (generalizePattern patrn)
        (generalizeExpr expr)
        meta



type instance F.Base AST_ = ASTF

instance F.Recursive AST_ where
    project (FunctionField_ name args body sig meta) =
        FunctionF name args body sig meta

    project (VarExpr_ name meta) =
        VarExprF name meta

    project (AbsExpr_ arg expr meta) =
        AbsExprF arg expr meta

    project (AppExpr_ e1 e2 meta) =
        AppExprF e1 e2 meta

    project (CaseExpr_ con alts meta) =
        CaseExprF con alts meta

    project (LetExpr_ fns expr meta) =
        LetExprF fns expr meta


    project (LitExpr_ value meta) =
        LitExprF value meta

    project (IfExpr_ xs x meta) =
        IfExprF xs x meta

    project (ParensExpr_ a meta) =
        ParensExprF a meta

    project (InfixAppExpr_ ident x1 x2 meta) =
        InfixAppExprF ident x1 x2 meta

    project (ConstrExpr_ ident meta) =
        ConstrExprF ident meta

    project (ListExpr_ xs meta) =
        ListExprF xs meta

    project (TupleExpr_ xs meta) =
        TupleExprF xs meta



    project (VarPattern_ binder meta) =
        VarPatternF binder meta

    project (ListPattern_ xs meta) =
        ListPatternF xs meta

    project (TuplePattern_ xs meta) =
        TuplePatternF xs meta

    project (WildcardPattern_ meta) =
        WildcardPatternF meta

    project (CaseAlt_ patrn expr meta) =
        CaseAltF patrn expr meta


instance F.Corecursive AST_ where
    embed (FunctionF name args body sig meta) =
        FunctionField_ name args body sig meta

    embed (VarExprF name meta) =
        VarExpr_ name meta

    embed (AbsExprF arg expr meta) =
        AbsExpr_ arg expr meta

    embed (AppExprF e1 e2 meta) =
        AppExpr_ e1 e2 meta

    embed (CaseExprF con alts meta) =
        CaseExpr_ con alts meta

    embed (LetExprF fns expr meta) =
        LetExpr_ fns expr meta

    
    embed (LitExprF value meta) =
        LitExpr_ value meta

    embed (IfExprF xs x meta) =
        IfExpr_ xs x meta

    embed (ParensExprF a meta) =
        ParensExpr_ a meta

    embed (InfixAppExprF ident x1 x2 meta) =
        InfixAppExpr_ ident x1 x2 meta

    embed (ConstrExprF ident meta) =
        ConstrExpr_ ident meta

    embed (ListExprF xs meta) =
        ListExpr_ xs meta

    embed (TupleExprF xs meta) =
        TupleExpr_ xs meta



    embed (VarPatternF binder meta) =
        VarPattern_ binder meta

    embed (ListPatternF xs meta) =
        ListPattern_ xs meta

    embed (TuplePatternF xs meta) =
        TuplePattern_ xs meta

    embed (WildcardPatternF meta) =
        WildcardPattern_ meta

    embed (CaseAltF patrn expr meta) =
        CaseAlt_ patrn expr meta








-- | Testing...
--
-- 
-- sample =
--     AbsExpr (Ident "x")
--         $ AbsExpr (Ident "y")
--             $ VarExpr (Ident "f") `AppExpr` VarExpr (Ident "x")
-- 
-- 
-- 
-- 
-- without :: Eq a => [a] -> [a] -> [a]
-- without =
--     Fold.foldr (List.filter . (/=))
-- 
-- 
-- 
-- freeVars :: Expr -> [Ident]
-- freeVars = (F.cata algebra) . generalizeExpr
--     where
--         algebra :: ExprF [Ident] -> [Ident]
--         algebra (VarExprF x)      = [x]
--         algebra (AbsExprF b e)   =
--             e  `without` [b]
-- 
--         algebra x = Fold.fold x





-- 
-- 
-- 
-- run =
--     let
--         xs = freeVars sample
--     in do
--         PP.prettyPrint xs
-- 







