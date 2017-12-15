{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SLIR.HelmSyntax.Internal.AST where


-- *
import Core
import Data.Data (Data, Typeable)
-- *



-- *
-- | # TopLevel
-- *



-- *
-- | ## Fixities
-- *
data Infix
    = InfixL Sym OpPrecedence (Maybe Meta)
    | InfixR Sym OpPrecedence (Maybe Meta)
    | InfixN Sym OpPrecedence (Maybe Meta)
    deriving (Show, Data, Typeable)


data OpPrecedence = OpPrecedence Int (Maybe Meta)
    deriving (Show, Data, Typeable)





-- *
-- | ## Functions
-- *
data Function
    = FnDecl Low [Low] Expr (Maybe Signature) (Maybe Meta)
    | OpDecl Sym [Low] Expr (Maybe Signature) (Maybe Meta)
    deriving (Show, Data, Typeable)





-- *
-- | ## Unions
-- *
data Union = Union Big [Low] [Constructor] (Maybe Meta)
    deriving (Data, Typeable, Show)

data Constructor = Constructor Big [Type] (Maybe Meta)
    deriving (Show, Data, Typeable)









-- *
-- | # TermLevel
-- *





-- *
-- | ## Expressions
-- *
data Expr
    = VarExpr Low (Maybe Meta)
    | LitExpr  LiteralValue (Maybe Meta)
    | RecordExpr [(Low, Expr)] (Maybe Meta)
    | TupleExpr [Expr] (Maybe Meta)
    | ListExpr [Expr] (Maybe Meta)
    
    | ConExpr Big (Maybe Meta)
    
    | BinOpExpr Sym Expr Expr (Maybe Meta)
    
    | IfExpr [(Expr, Expr)] Expr (Maybe Meta)
    
    | LetExpr [Function] Expr (Maybe Meta)
    
    | CaseExpr Expr [CaseAlt] (Maybe Meta)
    
    -- | TODO: Handle Ref(s)?
    | RecordUpdateExpr Low [(Low, Expr)] (Maybe Meta)
    | RecordAccessExpr Low (Maybe Expr) (Maybe Meta)
    
    | ParensExpr Expr (Maybe Meta)
    
    | AppExpr Expr Expr (Maybe Meta)
    
    | AbsExpr Low Expr (Maybe Meta)
    
    deriving (Show, Data, Typeable)














-- *
-- | ## Patterns
-- *
type PatternType = Type
type ExprType = Type

data CaseAlt = CaseAlt Pattern Expr (Maybe Meta)
    deriving (Show, Data, Typeable)



data Pattern
    = LitPattern  LiteralValue (Maybe Meta)
    
    | RecordPattern [Low] (Maybe Meta)
    
    | ListPattern [Pattern] (Maybe Meta)
    
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
    | ConsPattern [Pattern] (Maybe Pattern) (Maybe Meta)
    
    
    | TuplePattern [Pattern] (Maybe Meta)
    
    | ConPattern Big [Pattern] (Maybe Meta)
    | VarPattern Low (Maybe Meta)
    | WildcardPattern (Maybe Meta)
    deriving (Show, Data, Typeable)










-- *
-- | # Base
-- *



-- *
-- | ## Identifiers (Ident)
-- *


data Sym = Sym Text (Maybe Namespace)  (Maybe Meta)
    deriving (Show, Data, Typeable)

data Low = Low Text (Maybe Namespace)  (Maybe Meta)
    deriving (Show, Data, Typeable)

data Big = Big Text (Maybe Namespace)  (Maybe Meta)
    deriving (Show, Data, Typeable)


newtype Namespace = Namespace [Text]
    deriving (Show, Data, Typeable)









-- *
-- | ## Types
-- *
data Type
    = LiteralType LiteralType
    | RecordType [(Low, Type)] (Maybe Meta)
    | TupleType  [Type] (Maybe Meta)
    | ListType   Type (Maybe Meta)

    | UnionType Big [Type] (Maybe Meta)
    | VarType Low (Maybe Meta)
    | ArrType Type Type (Maybe Meta)
    | ParensType Type (Maybe Meta)
    
    | Superposed Type [Type]
    deriving (Show, Data, Typeable)


data LiteralType
    = StringType (Maybe Meta)
    | CharType (Maybe Meta)
    | IntType (Maybe Meta)
    | FloatType (Maybe Meta)
    | BoolType (Maybe Meta)
    deriving (Show, Data, Typeable)




-- *
-- | ### Type Schemes
-- *

-- NOTE:
-- * `Superposed` - Sometimes abbreviated as `Super` throughout codebase.

data Scheme
    = Forall [Low] Type
    deriving (Show, Data, Typeable)


-- data Forall = Forall [Low] Type
--     deriving (Show, Data, Typeable)












-- *
-- | ## Values
-- *

data LiteralValue
    = CharLit Text (Maybe Meta)
    | StringLit Text (Maybe Meta)
    | IntLit Int (Maybe Meta)
    | FloatLit Double (Maybe Meta)
    | BoolLit Bool (Maybe Meta)
    deriving (Show, Data, Typeable)








-- *
-- | Etc.
-- *


data Signature
    = Validated Scheme (Maybe Meta)
    | Unresolved Type (Maybe Meta)
    deriving (Show, Data, Typeable)




-- *
-- | # Header
-- *


-- *
-- | ## Header Base
-- *
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

-- *
-- | ## Header Import Declarations
-- *

data ImportDecl
    = ImportDecl Namespace (Maybe AsName) (Maybe ExplicitExposing)
    deriving (Show, Data, Typeable)

type AsName = Big

type ExplicitExposing = Entries



-- *
-- | Metadata, 'Source Code Location' & Related
-- *
data Meta
    = Meta
        { span :: Span
        , modulePath :: Text
        }
    deriving (Show, Data, Typeable, Eq, Ord)

data Span
    = Span Location Location
    deriving (Show, Data, Typeable, Eq, Ord)

data Location
    = Location
        { lineNum :: Int
        , columnNum :: Int
        }
    deriving (Show, Data, Typeable, Eq, Ord)





