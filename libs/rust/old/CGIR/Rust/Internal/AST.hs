{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module CGIR.Rust.Internal.AST where


-- *
import Core
import Data.Data (Data, Typeable)
-- *



-- *
-- | Call Site Declarations
-- *
data FunctionDecl = FunctionDecl Low (Maybe Self) [Generic] [Input] Output Block
    deriving (Show, Data, Typeable)


data ImplDecl = ImplDecl (Path Low) [FunctionDecl]
    deriving (Show, Data, Typeable)



-- | Base
--

data Input = Input Low Type
    deriving (Show, Data, Typeable)


newtype Output = Output Type
    deriving (Show, Data, Typeable)


data Self = Self
    deriving (Show, Data, Typeable)


newtype Generic = Generic Big
    deriving (Show, Data, Typeable)







-- *
-- | Top-Level Enum & Struct Declarations
-- *
data EnumDecl = EnumDecl Big [Big] [VariantDecl]
    deriving (Show, Data, Typeable)

data StructDecl = StructDecl Big [(Low, Type)]
    deriving (Show, Data, Typeable)

data VariantDecl
    = StructVariant Big [(Low, Type)]
    | TupleVariant  Big [Type]
    | UnitVariant   Big
    deriving (Show, Data, Typeable)







-- *
-- | Block Declarations
-- *
newtype Block = Block [Stmt]
    deriving (Show, Data, Typeable)




-- *
-- | Statement Declarations
-- *
data Stmt
    = VarRefStmt Low
    
    | ScalarStmt ScalarValue
    
    | VariantStmt (Path Stmt)
    | StructStmt (Path Stmt)  [(Low, Stmt)]
    | TupleStmt [Stmt]
    
    | FnCallStmt (Path Stmt)
    | MethodCallStmt MethodChain
    | ImplCallStmt (Path Stmt) [MethodChain]


    | FnStmt FunctionDecl
    | BlockStmt Block
    | LetStmt Low Stmt

    | MacroCall Low [Stmt]
    
    | AssignStmt Stmt Stmt
    
    
    -- e.g: self.z
    | SelfRefStmt (Maybe Low)
    | PathRefStmt (Path Stmt)
    
    
    | MatchStmt Stmt [Arm]
    
    deriving (Show, Data, Typeable)




-- | Base
--

-- E.g: `.arg(1, 2, 3)`
data MethodChain = MethodChain Low [Stmt]
    deriving (Show, Data, Typeable)




-- *
-- | Pattrn Branches
-- *

data Arm = Arm Pattern Stmt
    deriving (Show, Data, Typeable)


data Pattern
    = VarPattern Low
    
    | WildcardPattern
    
    | ScalarPattern ScalarValue
    
    | VariantPattern (Path Pattern)
    | TuplePattern [Pattern]
    deriving (Show, Data, Typeable)







-- *
-- | # Base
-- *





-- *
-- | Simple Identifiers/References
-- *
newtype Low = Low Name
    deriving (Eq, Ord, Show, Data, Typeable)


newtype Big = Big Name
    deriving (Eq, Ord, Show, Data, Typeable)



-- *
-- | (External) Identification System
-- *
newtype Path a = Path [Segment a]
    deriving (Show, Data, Typeable)


data Segment a
    = BigSeg Big [TyArg] (Maybe Self) [a]
    | LowSeg Low [TyArg] (Maybe Self) [a]
    deriving (Show, Data, Typeable)


type TyArg = Type

-- newtype TyArg = TyArg Type
--     deriving (Show, Data, Typeable)

-- data InArg a
--     = InArg a
--     | InSelf Self
--     deriving (Show, Data, Typeable)




-- *
-- | Internal ID Misc.
-- *
type Name = Text





-- *
-- | Type System
-- *
data Type
    = EnumPath   (Path Type)
    | StructPath (Path Type)
    
    | GenericType Big
    
    -- |
    -- Builtin Types
    | ScalarType ScalarType
    | SequenceType SequenceType
    
    -- Future?:
    -- * Maps
    -- * Sets
    -- * Misc
    
    -- Special
    | TupleType [Type]
    
    | FnTypeDecl FnType
    
    deriving (Show, Data, Typeable)



data FnType
    = FnType [InType] OutType
    deriving (Show, Data, Typeable)


type InType = Type
type OutType = Type


newtype SequenceType
    = Vec Type
    deriving (Show, Data, Typeable)



data ScalarType
    = Char
    | String
    | Bool
    | I8
    | I16
    | I32
    | I64
    | I128
    | U8
    | U16
    | U32
    | U64
    | U128
    | F32
    | F64
    deriving (Show, Data, Typeable)







-- *
-- | Values
-- *
data ScalarValue
    = CharValue Text
    | StringValue Text
    | BoolValue Bool
    | I8Value Int
    | I16Value Int
    | I32Value Int
    | I64Value Int
    | I128Value Int
    | U8Value Int
    | U16Value Int
    | U32Value Int
    | U64Value Int
    | U128Value Int
    | F32Value Double
    | F64Value Double
    deriving (Show, Data, Typeable)












-- *
-- | Misc.
-- *




