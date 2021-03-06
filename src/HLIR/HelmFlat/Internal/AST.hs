{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module HLIR.HelmFlat.Internal.AST where


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
    # Unions
-}

data Union = Union Ident [Ident] [Constructor]
    deriving (Show, Eq, Ord, Data, Typeable)

data Constructor = Constructor Ident [Type]
    deriving (Show, Eq, Ord, Data, Typeable)





{-
    # Function Declarations
-}

data Function = Function Binder [Binder] Expr (Maybe Scheme)
    deriving (Show, Eq, Ord, Data, Typeable)



{-
    # Expression Nodes
-}


data Expr
    = LitExpr LiteralValue
    | CaseExpr Expr [CaseAlt]
    | ConstrExpr Ident
    | ListExpr [Expr]
    | TupleExpr [Expr]
    | FunCallExpr Ref [Expr]
    | ConCallExpr Ident [Expr]
    | RefExpr Ref
    deriving (Show, Eq, Ord, Data, Typeable)





{-
    # Pattern Nodes
-}


data Pattern
    = VarPattern Binder
    | LitPattern LiteralValue
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
    | ListConsPattern [Pattern] (Maybe Pattern)
    | TuplePattern [Pattern]
    | ConstrPattern Ident [Pattern]
    | WildcardPattern
    deriving (Show, Eq, Ord, Data, Typeable)



{-
    ## Case-Alt Node
-}

data CaseAlt = CaseAlt Pattern Expr
    deriving (Show, Eq, Ord, Data, Typeable)




{-
    # Type Nodes
-}

data Type
    = TupleType  [Type]
    | ListType   Type
    | UnionType Ident [Type]
    | VarType Ident
    | ArrType Type Type
    | StringType
    | CharType
    | IntType
    | FloatType
    | BoolType
    deriving (Show, Eq, Ord, Data, Typeable)




data Scheme = Forall [Ident] Type
    deriving (Show, Eq, Ord, Data, Typeable)



{-
    # Base Values
-}

data LiteralValue
    = CharLit Text
    | StringLit Text
    | IntLit Int
    | FloatLit Double
    | BoolLit Bool
    deriving (Show, Eq, Ord, Data, Typeable)




{-
    # Identifiers
-}
data Ident = Ident Text (Maybe Namespace)
    deriving (Show, Eq, Ord, Data, Typeable)

newtype Namespace = Namespace [Text]
    deriving (Show, Eq, Ord, Data, Typeable)


{-
    # Uncategorized - Etc.
-}

data Binder = Binder Ident (Maybe Type)
    deriving (Show, Eq, Ord, Data, Typeable)

newtype Ref = Ref Ident
    deriving (Show, Eq, Ord, Data, Typeable)



