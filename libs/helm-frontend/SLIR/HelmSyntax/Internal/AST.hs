{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SLIR.HelmSyntax.Internal.AST where


-- *
import Core
import Data.Data (Data, Typeable)
-- *



-- *
-- | Internal Specials
-- *
data Binder
    = Binder Text
    | BinderIndex Int
    deriving (Show, Data, Typeable, Eq, Ord)


data Ref
    = Ref Text
    | RefIndex Int
    deriving (Show, Data, Typeable, Eq, Ord)


data Index
    = Index Int
    | Unknown
    deriving (Show, Data, Typeable, Eq, Ord)



-- *
-- | # TopLevel
-- *



-- *
-- | ## Fixities
-- *
data Infix
    = InfixL (Sym Text) OpPrecedence (Maybe Meta)
    | InfixR (Sym Text) OpPrecedence (Maybe Meta)
    | InfixN (Sym Text) OpPrecedence (Maybe Meta)
    deriving (Show, Data, Typeable)


data OpPrecedence = OpPrecedence Int (Maybe Meta)
    deriving (Show, Data, Typeable)





-- *
-- | ## Functions
-- *
data Function
    = FnDecl (Low Binder) [Low Binder] Expr (Maybe Signature) (Maybe Meta)
    | OpDecl (Sym Binder) [Low Binder] Expr (Maybe Signature) (Maybe Meta)
    deriving (Show, Data, Typeable)





-- *
-- | ## Unions
-- *
data Union = Union (Big Text) [Low Text] [Constructor] (Maybe Meta)
    deriving (Data, Typeable, Show)

data Constructor = Constructor (Big Text) [Type] (Maybe Meta)
    deriving (Show, Data, Typeable)









-- *
-- | # TermLevel
-- *





-- *
-- | ## Expressions
-- *
data Expr
    = VarExpr (Low Ref) (Maybe Meta)
    | LitExpr  LiteralValue (Maybe Meta)
    | RecordExpr [(Low Text, Expr)] (Maybe Meta)
    | TupleExpr [Expr] (Maybe Meta)
    | ListExpr [Expr] (Maybe Meta)
    
    | ConExpr (Big Text) (Maybe Meta)
    
    | BinOpExpr (Sym Ref) Expr Expr (Maybe Meta)
    
    | IfExpr [(Expr, Expr)] Expr (Maybe Meta)
    
    | LetExpr [Function] Expr (Maybe Meta)
    
    | CaseExpr Expr [CaseAlt] (Maybe Meta)
    
    -- | TODO: Handle Ref(s)?
    | RecordUpdateExpr (Low Text) [(Low Text, Expr)] (Maybe Meta)
    | RecordAccessExpr (Low Text) (Maybe Expr) (Maybe Meta)
    
    | ParensExpr Expr (Maybe Meta)
    
    | AppExpr Expr Expr (Maybe Meta)
    
    | AbsExpr (Low Binder) Expr (Maybe Meta)
    
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
    
    | RecordPattern [Low Text] (Maybe Meta)
    
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
    
    | ConPattern (Big Text) [Pattern] (Maybe Meta)
    | VarPattern (Low Text) (Maybe Meta)
    | WildcardPattern (Maybe Meta)
    deriving (Show, Data, Typeable)










-- *
-- | # Base
-- *



-- *
-- | ## Identifiers (Ident)
-- *


data Sym a = Sym a (Maybe Namespace)  (Maybe Meta)
    deriving (Show, Data, Typeable)

data Low a = Low a (Maybe Namespace)  (Maybe Meta)
    deriving (Show, Data, Typeable)

data Big a = Big a (Maybe Namespace)  (Maybe Meta)
    deriving (Show, Data, Typeable)


newtype Namespace = Namespace [Text]
    deriving (Show, Data, Typeable)









-- *
-- | ## Types
-- *
data Type
    = LiteralType LiteralType
    | RecordType [(Low Text, Type)] (Maybe Meta)
    | TupleType  [Type] (Maybe Meta)
    | ListType   Type (Maybe Meta)

    | UnionType (Big Text) [Type] (Maybe Meta)
    | VarType (Low Text) (Maybe Meta)
    | ArrType Type Type (Maybe Meta)
    | ParensType Type (Maybe Meta)
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
data Scheme = Forall [Low Text] Type
    deriving (Show, Data, Typeable)





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
data ModuleExporting
    = ExportExplicit [Entry]
    | ExportEverything
    deriving (Show, Data, Typeable)


newtype ModuleImporting = ModuleImporting [ImportDecl]
    deriving (Show, Data, Typeable)


data ImportDecl
    -- |
    -- e.g.
    --     * import Sample.One as One
    --     * import Sample.One
    = ImportQualified Namespace (Maybe (Big Text))
    
    -- |
    -- e.g.
    --      import Something (Alpha, red, green, Blue(..))
    | ImportExplicit Namespace [Entry]
    
    -- |
    -- e.g.
    --      import Something.Alpha (..)
    | ImportEverything Namespace
    deriving (Show, Data, Typeable)


-- | Base (Module Header) Types
--
data Entry
    = ValueEntry Text
    | UnionEntry Text UnionExposing
    deriving (Show, Data, Typeable)


data UnionExposing
    = UnionEverything
    | UnionExplicit [Text]
    deriving (Show, Data, Typeable)




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





