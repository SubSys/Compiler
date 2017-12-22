{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module HLIR.HelmIntro.Internal.AST where


-- *
import Core
import Data.Data (Data, Typeable)
-- *



-- *
-- | # TopLevel
-- *




-- *
-- | ## Functions
-- *
data Function = Function Binder [Binder] Expr (Maybe Scheme)
    deriving (Show, Eq, Ord, Data, Typeable)





-- *
-- | ## Unions
-- *
data Union = Union Big [Low] [Constructor]
    deriving (Data, Typeable, Show)

data Constructor = Constructor Big [Type]
    deriving (Show, Eq, Ord, Data, Typeable)









-- *
-- | # TermLevel
-- *





-- *
-- | ## Expressions
-- *
data Expr
    = VarExpr Ref
    | LitExpr  LiteralValue
    | TupleExpr [Expr]
    | ListExpr [Expr]
    | ConExpr Big
    | IfExpr [(Expr, Expr)] Expr
    | LetExpr [Function] Expr
    | CaseExpr Expr [CaseAlt]
    
    | ParensExpr Expr
    
    | AppExpr Expr Expr
    
    | BinAppExpr Ref Expr Expr
    
    | AbsExpr Binder Expr
    
    deriving (Show, Eq, Ord, Data, Typeable)














-- *
-- | ## Patterns
-- *

data CaseAlt = CaseAlt Pattern Expr
    deriving (Show, Eq, Ord, Data, Typeable)



data Pattern
    = LitPattern  LiteralValue
    
    -- | RecordPattern [Low]
    
    | ListPattern [Pattern]
    
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
    | ConsPattern [Pattern] (Maybe Pattern)
    
    
    | TuplePattern [Pattern]
    
    | ConPattern Big [Pattern]
    | VarPattern Binder
    | WildcardPattern
    deriving (Show, Eq, Ord, Data, Typeable)










-- *
-- | # Base
-- *



-- *
-- | ## Identifiers (Ident)
-- *


data Ref = Ref Text (Maybe Namespace) 
    deriving (Show, Eq, Ord, Data, Typeable)

data Binder = Binder Text (Maybe Namespace) 
    deriving (Show, Eq, Ord, Data, Typeable)

data Low = Low Text (Maybe Namespace) 
    deriving (Show, Eq, Ord, Data, Typeable)

data Big = Big Text (Maybe Namespace) 
    deriving (Show, Eq, Ord, Data, Typeable)

newtype Namespace = Namespace [Text]
    deriving (Show, Eq, Ord, Data, Typeable)









-- *
-- | ## Types
-- *
data Type
    = LiteralType LiteralType
    | RecordType [(Low, Type)]
    | TupleType  [Type]
    | ListType   Type

    | UnionType Big [Type]
    | VarType Low
    | ArrType Type Type
    | ParensType Type
    
    | Superposed Type [Type]
    deriving (Show, Eq, Ord, Data, Typeable)


data LiteralType
    = StringType
    | CharType
    | IntType
    | FloatType
    | BoolType
    deriving (Show, Eq, Ord, Data, Typeable)




-- *
-- | ### Type Schemes
-- *

-- NOTE:
-- * `Superposed` - Sometimes abbreviated as `Super` throughout codebase.

data Scheme
    = Forall [Low] Type
    deriving (Show, Eq, Ord, Data, Typeable)


-- data Forall = Forall [Low] Type
--     deriving (Show, Eq, Ord, Data, Typeable)












-- *
-- | ## Values
-- *

data LiteralValue
    = CharLit Text
    | StringLit Text
    | IntLit Int
    | FloatLit Double
    | BoolLit Bool
    deriving (Show, Eq, Ord, Data, Typeable)


