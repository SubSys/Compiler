{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module CGIR.GLSL.Internal.AST where


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


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable as F

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP
-- ~


{-# ANN module ("HLint: ignore" :: String) #-}







{-
    # Top-Level Items
-}


{-
    ## Value Declarations
-}

data Function = Function Type Ident [Input] Block
    deriving (Show, Eq, Ord, Data, Typeable)

data Object = Object (Maybe StorageQualifier) Type Ident
    deriving (Show, Eq, Ord, Data, Typeable)





{-
    ## Type Declarations
-}

-- TODO: ....
-- data Struct = Struct



{-
    # Block-Level Items
-}

newtype Block = Block [Stmt]
    deriving (Show, Eq, Ord, Data, Typeable)


data Stmt
    = ExprStmt ExprStmt
    | SelectionStmt SelectionStmt
    | IterationStmt IterationStmt
    | JumpStmt JumpStmt
    | InitLocalObject LocalObjectStmt
    deriving (Show, Eq, Ord, Data, Typeable)


data ExprStmt
    = AssignmentStmt Stmt Stmt
    
    | ReferenceStmt Ident
    | ConstructorCallStmt Ident [Stmt]
    | FunctionCallStmt Ident [Stmt]
    
    | MethodAccessStmt Ident Ident
    | ArrayAccessStmt Ident Index
    | LiteralStmt LiteralValue
    deriving (Show, Eq, Ord, Data, Typeable)



data SelectionStmt
    = IfStmt [(Stmt, Block)] (Maybe Block)
    | SwitchStmt Stmt [(Stmt, Block)] (Maybe Block)
    deriving (Show, Eq, Ord, Data, Typeable)

data IterationStmt
    = ForStmt (Stmt, Stmt, Stmt) Block
    | WhileStmt Stmt Block
    deriving (Show, Eq, Ord, Data, Typeable)

data JumpStmt
    = ContinueStmt
    | BreakStmt
    | ReturnStmt (Maybe Stmt)
    | DiscardStmt
    deriving (Show, Eq, Ord, Data, Typeable)


data LocalObjectStmt
    = LocalObjectStmt Type Ident (Maybe Stmt)
    deriving (Show, Eq, Ord, Data, Typeable)





{-
    # Type Items
-}

data Type
    = TransparentType TransparentType
    | FloatingPointSamplerType FloatingPointSamplerType
    | SignedIntegerSamplerType SignedIntegerSamplerType
    | UnsignedIntegerSamplerType UnsignedIntegerSamplerType
    deriving (Show, Eq, Ord, Data, Typeable)


data TransparentType
    = VoidType
    | BoolType
    | IntType
    | UIntType
    | FloatType
    | Vec2Type
    | Vec3Type
    | Vec4Type
    | BVec2Type
    | BVec3Type
    | BVec4Type
    | IVec2Type
    | IVec3Type
    | IVec4Type
    | UVec2Type
    | UVec3Type
    | UVec4Type
    | Mat2x2Type
    | Mat2x3Type
    | Mat2x4Type
    | Mat3x2Type
    | Mat3x3Type
    | Mat3x4Type
    | Mat4x2Type
    | Mat4x3Type
    | Mat4x4Type
    deriving (Show, Eq, Ord, Data, Typeable)


data FloatingPointSamplerType
    = Sampler1DType
    | Sampler2DType
    | Sampler3DType
    | SamplerCubeType
    | Sampler2DRectType
    | Sampler1DShadowType
    | Sampler2DShadowType
    | Sampler2DRectShadowType
    | Sampler1DArrayType
    | Sampler2DArrayType
    | Sampler1DArrayShadowType
    | Sampler2DArrayShadowType
    | SamplerBufferType
    | Sampler2DMSType
    | Sampler2DMSArrayType
    deriving (Show, Eq, Ord, Data, Typeable)


data SignedIntegerSamplerType
    = Isampler1DType
    | Isampler2DType
    | Isampler3DType
    | IsamplerCubeType
    | Isampler2DRectType
    | Isampler1DArrayType
    | Isampler2DArrayType
    | IsamplerBufferType
    | Isampler2DMSType
    | Isampler2DMSArrayType
    deriving (Show, Eq, Ord, Data, Typeable)



data UnsignedIntegerSamplerType
    = Usampler1DType
    | Usampler2DType
    | Usampler3DType
    | UsamplerCubeType
    | Usampler2DRectType
    | Usampler1DArrayType
    | Usampler2DArrayType
    | UsamplerBufferType
    | Usampler2DMSType
    | Usampler2DMSArrayType
    deriving (Show, Eq, Ord, Data, Typeable)




{-
    # Qualification Items
-}


{-
    ## Storage Qualifier
-}

data StorageQualifier
    = ConstStorageQualifier
    | InStorageQualifier (Maybe InterpolationQualifier)
    | CentroidInStorageQualifier (Maybe InterpolationQualifier)
    | OutStorageQualifier (Maybe InterpolationQualifier)
    | CentroidOutStorageQualifier (Maybe InterpolationQualifier)
    | UniformStorageQualifier
    deriving (Show, Eq, Ord, Data, Typeable)

{-
    ## Parameter Qualifier
-}

data ParameterQualifier
    = InParameterQualifier
    | OutParameterQualifier
    | InoutParameterQualifier
    deriving (Show, Eq, Ord, Data, Typeable)


{-
    ## Etc.
-}
data InterpolationQualifier
    = SmoothInterpolation
    | FlatInterpolation
    | NoperspectiveInterpolation
    deriving (Show, Eq, Ord, Data, Typeable)








{-
    # Base
-}


{-
    # Base Values
-}
data LiteralValue
    = Float Double
    | Int Int
    | Bool Bool
    deriving (Show, Eq, Ord, Data, Typeable)


{-
    ## Identifiers
-}
data Ident = Ident Text (Maybe Namespace)
    deriving (Show, Eq, Ord, Data, Typeable)

data Namespace = Namespace [Text]
    deriving (Show, Eq, Ord, Data, Typeable)


{-
    ## Etc.
-}

data Input = Input (Maybe ParameterQualifier) Type Ident
    deriving (Show, Eq, Ord, Data, Typeable)




{-
    ## Misc. Aliases
    > (For readability - Not Exposed)
-}
type Index = Int





















