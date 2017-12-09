{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module CGIR.Metal.Internal.AST where


-- *
import Core
import Data.Data (Data, Typeable)
-- *


data Test = Test
    deriving (Show, Data, Typeable)



















-- *
-- | Top-Level
-- *







-- | Function Declarations & Related
--
data FunctionDecl
    = FunctionDecl (Maybe TemplateDecl) (Maybe FunctionSpecifier) Output Low [Input] Block
    deriving (Show, Data, Typeable)


data StructDecl = StructDecl (Maybe TemplateDecl) (Maybe Big) [ObjectMember]
    deriving (Show, Data, Typeable)
    
data UnionDecl  = UnionDecl (Maybe Big) [ObjectMember]
    deriving (Show, Data, Typeable)

data EnumClassDecl   = EnumClassDecl (Maybe Big) [Big]
    deriving (Show, Data, Typeable)




-- | Base
--

data ObjectMember
    = DataFieldMember Type Low
    | MethodMember    FunctionDecl Low
    | EnumClassMember EnumClassDecl Low
    | StructMember    StructDecl Low
    | UnionMember     UnionDecl Low
    deriving (Show, Data, Typeable)






-- | Misc. Header Declarations
--
data FunctionSpecifier
    = Kernel
    | Vertex
    | Fragment
    deriving (Show, Data, Typeable)



data Input = Input Type Low (Maybe ArgAttribute)
    deriving (Show, Data, Typeable)

newtype Output = Output Type
    deriving (Show, Data, Typeable)

data ArgAttribute
    = DeviceBufferAttr      AttrIndexVal
    | ConstantBufferAttr    AttrIndexVal
    | TextureAttr           AttrIndexVal
    | TextureArrayAttr      AttrIndexVal
    | SamplerAttr           AttrIndexVal
    | ThreadgroupBufferAttr AttrIndexVal
    deriving (Show, Data, Typeable)

type AttrIndexVal = Int


newtype TemplateDecl = TemplateDecl [TemplateArg]
    deriving (Show, Data, Typeable)


newtype TemplateArg
    = Typename Big
    deriving (Show, Data, Typeable)








-- *
-- | Block & Namespace Declarations
-- *

newtype Block = Block [Stmt]
    deriving (Show, Data, Typeable)


data Namespace = Namespace Big Block
    deriving (Show, Data, Typeable)








-- *
-- | Block-Level
-- *



-- | Statements
-- 

data Stmt

    -- | Control Flow Tokens
    --
    = DefaultStmt
    | BreakStmt
    | ContinueStmt
    | ReturnStmt (Maybe Stmt)

    -- | Control Flow constructs
    --
    | IfStmt [(Stmt, Block)] Block
    | SwitchStmt Stmt [(ScopePath, Block)]

    -- | Iteration Constructs
    --
    | WhileStmt Stmt Stmt
    | DoWhileStmt Stmt Stmt
    | ForStmt [Stmt] Stmt

    -- | Expression Constructs
    --
    | InitStmt Type Low (Maybe [InitField])
    | VarStmt Low

    | NameAccessStmt ValuePath
    | RefAccessStmt ValuePath
    | PointerAccessStmt ValuePath


    -- Values
    | ScalarStmt ScalarValue
    | MatrixStmt MatrixValue
    | VectorStmt VectorValue


    -- | Imperative Constructs
    --
    | AssignStmt Stmt Stmt
    | CallStmt Low [Stmt]

    -- | Local (Stmt-Level) Declarations
    | LocalBlockStmt Block
    deriving (Show, Data, Typeable)


data InitField = InitField ValuePath Stmt
    deriving (Show, Data, Typeable)







-- | Values
--
data ScalarValue
    = BoolValue Bool
    | F32Value Double
    | F16Value Double
    | I32Value Int
    | I16Value Int
    | I8Value Int
    | U32Value Int
    | U16Value Int
    | U8Value Int
    deriving (Show, Data, Typeable)


data MatrixValue
    = F32MatValue (DimValue Double)
    | F16MatValue (DimValue Double)
    deriving (Show, Data, Typeable)


data VectorValue
    = BoolVecValue  (IndexValue Bool)
    | F32VecValue   (IndexValue Double)
    | F16VecValue   (IndexValue Double)
    | I32VecValue   (IndexValue Int)
    | I16VecValue   (IndexValue Int)
    | I8VecValue    (IndexValue Int)
    | U32VecValue   (IndexValue Int)
    | U16VecValue   (IndexValue Int)
    | U8VecValue    (IndexValue Int)
    deriving (Show, Data, Typeable)


data IndexValue a
    = Index2Value a a
    | Index3Value a a a
    | Index4Value a a a a
    deriving (Show, Data, Typeable)


data DimValue a
    = Dim2x2Value
        (a, a)
        (a, a)

    | Dim2x3Value
        (a, a)
        (a, a)
        (a, a)
    
    | Dim2x4Value
        (a, a)
        (a, a)
        (a, a)
        (a, a)
    
    | Dim3x2Value
        (a, a, a)
        (a, a, a)
    
    | Dim3x3Value
        (a, a, a)
        (a, a, a)
        (a, a, a)

    | Dim3x4Value
        (a, a, a)
        (a, a, a)
        (a, a, a)
        (a, a, a)
    
    | Dim4x2Value
        (a, a, a, a)
        (a, a, a, a)
    
    | Dim4x3Value
        (a, a, a, a)
        (a, a, a, a)
        (a, a, a, a)
    
    | Dim4x4Value
        (a, a, a, a)
        (a, a, a, a)
        (a, a, a, a)
        (a, a, a, a)
    deriving (Show, Data, Typeable)












-- *
-- | # Base
-- *
newtype Low = Low Text
    deriving (Show, Data, Typeable)

newtype Big = Big Text
    deriving (Show, Data, Typeable)




-- | Qualified Name Identifiers
--


-- | E.g. `myNamespace::a`
newtype ScopePath = ScopePath [QSeg]
    deriving (Show, Data, Typeable)

-- | E.g. `instance.nested.field`
newtype ValuePath = ValuePath [QSeg]
    deriving (Show, Data, Typeable)


-- | Qualified Segment Identifier
--
data QSeg
    = BigSeg Big
    | LowSeg Low
    deriving (Show, Data, Typeable)








-- *
-- | ## Type System
-- *

data Type
    = Void
    | ScalarType ScalarType
    | VectorType VectorType
    | MatrixType MatrixType
    | PixelType PixelType
    | AddressSpacePtr AddressSpacePtr
    | ConstType Type
    | ConstexprType Type
    | GenericType Big
    deriving (Show, Data, Typeable)

data ScalarType
    = BoolType
    | F32Type
    | F16Type
    | I32Type
    | I16Type
    | I8Type
    | U32Type
    | U16Type
    | U8Type
    deriving (Show, Data, Typeable)



data AddressSpacePtr
    = DeviceType Type
    | ThreadgroupType Type
    | ThreadgroupImgBlkType Type
    | ConstantType Type
    | ThreadType Type
    deriving (Show, Data, Typeable)



data MatrixType
    = F32MatType DimType
    | F16MatType DimType
    deriving (Show, Data, Typeable)

data VectorType
    = BoolVecType IndexType
    | F32VecType  IndexType
    | F16VecType  IndexType
    | I32VecType  IndexType
    | I16VecType  IndexType
    | I8VecType   IndexType
    | U32VecType  IndexType
    | U16VecType  IndexType
    | U8VecType   IndexType
    deriving (Show, Data, Typeable)



-- TODO: ...
data PixelType = PixelType'
    deriving (Show, Data, Typeable)

data BufferType
data TextureType
data SamplerType

-- | Compound Types
data ImageblockType
data AggregateType

data UniformType



-- | Aux.
data IndexType
    = Index2Type
    | Index3Type
    | Index4Type
    deriving (Show, Data, Typeable)


data DimType
    = Dim2x2Type
    | Dim2x3Type
    | Dim2x4Type
    | Dim3x2Type
    | Dim3x3Type
    | Dim3x4Type
    | Dim4x2Type
    | Dim4x3Type
    | Dim4x4Type
    deriving (Show, Data, Typeable)


























