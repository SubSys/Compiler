{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.Cmm.Internal.AST where


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

data FunctionDecl = Function Ident [Input] Output [Stmt]

data GlobalDecl = Global (Maybe StorageQualifier) Type Ident





{-
    ## Type Declarations
-}


-- TODO: ....
data Struct = Struct



{-
    # Block-Level Items
-}

data Stmt = Stmt




{-
    # Type Items
-}

data Type
    = TransparentType TransparentType
    | FloatingPointSamplerType FloatingPointSamplerType
    | SignedIntegerSamplerType SignedIntegerSamplerType
    | UnsignedIntegerSamplerType UnsignedIntegerSamplerType


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




{-
    # Qualifier Items
-}


{-
    ## Storage Qualifier
-}

data StorageQualifier
    = ConstQualifier
    | InQualifier (Maybe InterpolationQualifier)
    | CentroidInQualifier (Maybe InterpolationQualifier)
    | OutQualifier (Maybe InterpolationQualifier)
    | CentroidOutQualifier (Maybe InterpolationQualifier)
    | UniformQualifier


{-
    ## Parameter Qualifier
-}


{-
    ## Etc.
-}
data InterpolationQualifier
    = SmoothInterpolation
    | FlatInterpolation
    | NoperspectiveInterpolation








{-
    # Base
-}


{-
    ## Identifiers
-}
data Ident



{-
    ## Etc.
-}
data Input
data Output






















