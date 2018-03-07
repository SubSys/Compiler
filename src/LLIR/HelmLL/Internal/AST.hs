{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module LLIR.HelmLL.Internal.AST where


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

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable as F

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP
-- ~


{-# ANN module ("HLint: ignore" :: String) #-}












{-
    # Top-Level Items
-}


data Function = Function Binder [Binder] Block (Maybe Scheme)
    deriving (Show, Eq, Ord, Data, Typeable)



data Union = Union Ident [Ident] [Constr]
    deriving (Show, Eq, Ord, Data, Typeable)


data Constr = Constr Ident [Type]
    deriving (Show, Eq, Ord, Data, Typeable)



{-
    # Term-Level Items
-}


data Stmt
    = RefStmt Bounded
    | LitStmt LiteralValue
    | FunCallStmt Bounded [Stmt]
    | ConCallStmt Ident [Stmt]
    | IfStmt [(Stmt, Block)] Block
    | CaseStmt Stmt [CaseAlt]
    
    | ListStmt [Stmt]
    | TupleStmt [Stmt]
    
    | LoopStmt Stmt Block
    deriving (Show, Eq, Ord, Data, Typeable)






data CaseAlt = CaseAlt Pattern Block
    deriving (Show, Eq, Ord, Data, Typeable)


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
    # Base
-}


newtype Block = Block [Stmt]
    deriving (Show, Eq, Ord, Data, Typeable)

{-
    ## Type Declarations
-}

data Type
    = TupleType [Type]
    | ListType Type
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
    ## Base Values
-}
data LiteralValue
    = Float Double
    | Int Int
    | String Text
    | Char Text
    | Bool Bool
    deriving (Show, Eq, Ord, Data, Typeable)




{-
    ## Identifiers
-}
data Ident = Ident Text (Maybe Namespace)
    deriving (Show, Eq, Ord, Data, Typeable)

data Binder = Binder Ident (Maybe Type)
    deriving (Show, Eq, Ord, Data, Typeable)

data Namespace = Namespace [Text]
    deriving (Show, Eq, Ord, Data, Typeable)


-- | Special - Wrapped internally at the external interface.
--
data Bounded = Bounded Ident
    deriving (Show, Eq, Ord, Data, Typeable)


{-
    ## Etc.
-}




{-
    ## Misc. Aliases
    > (For readability - Not Exposed)
-}
type Index = Int










