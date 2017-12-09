{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module CGIR.Rust.Internal.AST where


-- *
import Core
import Data.Data (Data, Typeable)
-- *




-- *
-- | # Specials
-- *




-- *
-- | # TopLevel
-- *




-- *
-- | ## Functions
-- *
data Function = Function Low [Generic] [Input] Output Block
    deriving (Show, Data, Typeable)






-- *
-- | ## Unions
-- *
data Union = Union Big [Low] [Constructor]
    deriving (Show, Data, Typeable)


data Constructor = Constructor Big [Type]
    deriving (Show, Data, Typeable)









-- *
-- | # TermLevel
-- *

newtype Block = Block [Stmt]
    deriving (Show, Data, Typeable)


-- *
-- | ## Statements & Expressions
-- *

data Stmt
    = FunCallStmt Low [Stmt]
    | ConCallStmt Big [Stmt]
    | RefStmt (Maybe RefPrefix) Low
    | LitStmt LiteralValue
    | TupleStmt [Stmt]
    | ListStmt [Stmt]
    | CaseStmt Stmt [CaseAlt]
    deriving (Show, Data, Typeable)



data RefPrefix
    = AndRef
    deriving (Show, Data, Typeable)







-- *
-- | ## Patterns
-- *


data CaseAlt = CaseAlt Pattern Block
    deriving (Show, Data, Typeable)


data Pattern
    = LitPattern  LiteralValue

    | RecordPattern [Low]
    | ListPattern [Pattern]
    | ConsPattern [Pattern] (Maybe Pattern)

    | TuplePattern [Pattern]

    | ConPattern Big [Pattern]
    | VarPattern Low
    | WildcardPattern
    deriving (Show, Data, Typeable)











-- *
-- | # Base
-- *



-- *
-- | ## Identifiers (Ident)
-- *
data Low = Low Text (Maybe Namespace)
    deriving (Show, Data, Typeable, Ord, Eq)

data Big = Big Text (Maybe Namespace)
    deriving (Show, Data, Typeable, Ord, Eq)

newtype Namespace = Namespace [Text]
    deriving (Show, Data, Typeable, Ord, Eq)






-- *
-- | ## Types
-- *
data Type
    = LiteralType LiteralType
    | RecordType [(Low, Type)]
    | TupleType  [Type]
    | ListType   Type

    | UnionType Big [Type]
    | GenericType Generic
    
    | FnType [Type] Output
    
    deriving (Show, Data, Typeable, Eq, Ord)



data LiteralType
    = StringType
    | CharType
    | IntType
    | FloatType
    | BoolType
    deriving (Show, Data, Typeable, Eq, Ord)






-- *
-- | ## Values
-- *

data LiteralValue
    = CharLit Text
    | StringLit Text
    | IntLit Int
    | FloatLit Double
    | BoolLit Bool
    deriving (Show, Data, Typeable, Eq, Ord)








-- *
-- | # Etc.
-- *


data Input = Input Low Type
    deriving (Show, Data, Typeable, Eq, Ord)

newtype Output = Output Type
    deriving (Show, Data, Typeable, Eq, Ord)

newtype Generic = Generic Low
    deriving (Show, Data, Typeable, Eq, Ord)








