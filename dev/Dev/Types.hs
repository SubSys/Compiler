{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LLIR.DarkRoast.Internal.AST.Base.Types (
      Type(..)
    , Aggregate(..)
    , FnType(..)
    , Scalar(..)
    , Input(..)
    , Output(..)
) where


-- *
import Core
import Data.Data (Data, Typeable)

import qualified Data.HashMap.Strict as HM

--- Local
import qualified LLIR.DarkRoast.Internal.AST.Base.Ident as ID
-- *




-- *
-- | Type System
-- *
data Type
    = Structure ID.Big     [Type]
    | RecStructure ID.Big  [Type]

    | Generic ID.Big


    -- |
    -- Builtin
    | Aggregate Aggregate
    | Scalar Scalar
    
    | FnTyDecl FnType
    
    deriving (Show, Data, Typeable)


data FnType
    = FnType [Type] Type
    deriving (Show, Data, Typeable)


data Aggregate
    = Record [(ID.Low, Type)]
    | Tuple  [Type]
    | List   Type
    deriving (Show, Data, Typeable)


data Scalar
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
-- | # Etc 
-- *

-- TODO: Move this out of `Types` (Maybe back to `Functions`)...
data Input = Input ID.Low Type
    deriving (Show, Data, Typeable)

newtype Output = Output Type
    deriving (Show, Data, Typeable)




