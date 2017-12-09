{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmCore.Internal.AST where


-- *
import Core
import Data.Data (Data, Typeable)
import Data.Functor.Foldable.TH (makeBaseFunctor)

-- import qualified Text.Show.Prettyprint as PP
-- import qualified Text.PrettyPrint.Leijen.Text as P

import qualified Data.Text as Text

--- Local Deps
import Framework.Render hiding (Render(..))
import qualified Framework.Render.Utils as Util
import qualified Framework.Render.Display as Display
-- *






-- *
-- | # TopLevel
-- *




-- *
-- | ## Functions
-- *
data Function = Function Binder Expr Scheme
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





-- *
-- | ## Expressions
-- *
data Expr
    = VarExpr Ref
    | LitExpr  LiteralValue
    | RecordExpr [(Low, Expr)]
    | TupleExpr [Expr]
    | ListExpr [Expr]
    
    | ConExpr Big
    
    | LetExpr [Function] Expr
    
    | CaseExpr Expr [CaseAlt]
    
    | AppExpr Expr Expr
    
    | AbsExpr Binder Expr
    
    deriving (Show, Data, Typeable)


-- *
-- | ## Patterns
-- *


data CaseAlt = CaseAlt Pattern Expr
    deriving (Show, Data, Typeable)


data Pattern
    = LitPattern  LiteralValue
    
    | RecordPattern [Low]
    | ListPattern [Pattern]
    | ConsPattern [Pattern] (Maybe Pattern)
    
    
    | TuplePattern [Pattern]
    
    | ConPattern Big [Pattern]
    | VarPattern Binder
    | WildcardPattern
    deriving (Show, Data, Typeable)










-- *
-- | # Base
-- *
data Binder = Binder Text (Maybe Namespace)
    deriving (Eq, Ord, Data, Typeable)

data Ref = Ref Text (Maybe Namespace)
    deriving (Eq, Ord, Data, Typeable)



-- *
-- | ## Identifiers (Ident)
-- *
data Low = Low Text (Maybe Namespace)
    deriving (Show, Data, Typeable, Ord, Eq)

data Big = Big Text (Maybe Namespace)
    deriving (Show, Data, Typeable, Ord, Eq)



newtype Namespace = Namespace [Text]
    deriving (Data, Typeable, Ord, Eq)




-- *
-- | ## Types
-- *
data Type
    = LiteralType LiteralType
    | RecordType [(Low, Type)]
    | TupleType  [Type]
    | ListType   Type

    | UnionType Big [Type]
    | VarType Low
    | ArrType Type Type
    deriving (Show, Data, Typeable, Eq, Ord)



data LiteralType
    = StringType
    | CharType
    | IntType
    | FloatType
    | BoolType
    deriving (Show, Data, Typeable, Eq, Ord)




-- *
-- | ### Type Schemes
-- *
data Scheme = Forall [Low] Type
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
    deriving (Show, Data, Typeable)








-- *
-- | Etc.
-- *
















-- *
-- | # Internal
-- *


-- *
-- | ## Custom Render Instances
-- *


instance RenderDebug Namespace where
    debug (Namespace segs) =
        let segs' = Util.brackets (Util.vcat $ map debug segs)
        in
            Util.parens ("Namespace" <+> segs')

instance RenderDebug (Maybe Namespace) where
    debug Nothing = "Nothing"
    debug (Just ns) =
        Util.parens $
            "Just" <+> debug ns

instance RenderDebug Binder where
    debug (Binder txt ns) =
        Util.parens
            $ "Binder" <+> debug txt <+> debug ns


instance RenderDebug Ref where
    debug (Ref txt ns) =
        Util.parens
            $ "Ref" <+> debug txt <+> debug ns 


-- *
-- | ## Custom Show Instances
-- *
instance Show Namespace where
    show =
        Text.unpack . Display.packDebugDoc 


instance Show Binder where
    show =
        Text.unpack . Display.packDebugDoc 



instance Show Ref where
    show =
        Text.unpack . Display.packDebugDoc 




