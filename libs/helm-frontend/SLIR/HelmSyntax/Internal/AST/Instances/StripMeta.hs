{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Internal.AST.Instances.StripMeta where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)
import Prelude (mapM_, IO, String, return, (<$>))

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Control.Monad.State.Lazy as State


--- Local
import SLIR.HelmSyntax.Internal.AST.Classes.StripMeta (StripMeta(..))

import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *




{-# ANN module "HLint: ignore" #-}





-- *
-- | Internal Specials
-- *


instance StripMeta IR.Binder where
    stripMeta (IR.Binder text) =
        IR.Binder text

    stripMeta (IR.BinderIndex int) =
        IR.BinderIndex int


instance StripMeta IR.Ref where
    stripMeta (IR.Ref text) =
        IR.Ref text

    stripMeta (IR.RefIndex int) =
        IR.RefIndex int



-- *
-- | # TopLevel
-- *



-- *
-- | ## Fixities
-- *
instance StripMeta IR.Infix where
    stripMeta (IR.InfixL sym opPrecedence _) =
        IR.InfixL (stripMeta sym) (stripMeta opPrecedence) Nothing

    stripMeta (IR.InfixR sym opPrecedence _) =
        IR.InfixR (stripMeta sym) (stripMeta opPrecedence) Nothing

    stripMeta (IR.InfixN sym opPrecedence _) =
        IR.InfixN (stripMeta sym) (stripMeta opPrecedence) Nothing




instance StripMeta IR.OpPrecedence where
    stripMeta (IR.OpPrecedence i _) =
        IR.OpPrecedence i Nothing





-- *
-- | ## Functions
-- *

instance StripMeta IR.Function where
    stripMeta (IR.FnDecl name args expr optSig _) =
        IR.FnDecl
            (stripMeta name)
            (map stripMeta args)
            (stripMeta expr)
            (stripMeta optSig)
            Nothing

    stripMeta (IR.OpDecl name args expr optSig _) =
        IR.OpDecl
            (stripMeta name)
            (map stripMeta args)
            (stripMeta expr)
            (stripMeta optSig)
            Nothing



instance StripMeta (Maybe IR.Signature) where
    stripMeta Nothing = Nothing
    stripMeta (Just sig) = Just $ stripMeta sig


-- *
-- | ## Unions
-- *

instance StripMeta IR.Union where
    stripMeta (IR.Union name args cons _) =
        IR.Union
            (stripMeta name)
            (map stripMeta args)
            (map stripMeta cons)
            Nothing



instance StripMeta IR.Constructor where
    stripMeta (IR.Constructor name args _) =
        IR.Constructor
            (stripMeta name)
            (map stripMeta args)
            Nothing


-- *
-- | # TermLevel
-- *





-- *
-- | ## Expressions
-- *

instance StripMeta IR.Expr where
    stripMeta (IR.VarExpr name _) =
        IR.VarExpr
            (stripMeta name)
            Nothing

    stripMeta (IR.LitExpr value _) =
        IR.LitExpr
            (stripMeta value)
            Nothing

    stripMeta (IR.RecordExpr fields _) =
        IR.RecordExpr
            fields'
            Nothing
        where
            fields' = map field fields
            field (name, expr) =
                (stripMeta name, stripMeta expr)

    stripMeta (IR.TupleExpr items _) =
        IR.TupleExpr
            (map stripMeta items)
            Nothing

    stripMeta (IR.ListExpr xs _) =
        IR.ListExpr
            (map stripMeta xs)
            Nothing

    stripMeta (IR.ConExpr name _) =
        IR.ConExpr
            (stripMeta name)
            Nothing

    stripMeta (IR.BinOpExpr sum e1 e2 _) =
        IR.BinOpExpr
            (stripMeta sum)
            (stripMeta e1)
            (stripMeta e2)
            Nothing

    stripMeta (IR.IfExpr branches elseExpr _) =
        IR.IfExpr
            branches'
            (stripMeta elseExpr)
            Nothing
        where
            branches' = map branch branches
            branch (con, body) =
                (stripMeta con, stripMeta body)

    stripMeta (IR.LetExpr fns expr _) =
        IR.LetExpr
            (map stripMeta fns)
            (stripMeta expr)
            Nothing 

    stripMeta (IR.CaseExpr expr alts _) =
        IR.CaseExpr
            (stripMeta expr)
            (map stripMeta alts)
            Nothing

    -- stripMeta (IR.RecordUpdateExpr (Low Text) [(Low Text, Expr)] _) =
    -- stripMeta (IR.RecordAccessExpr (Low Text) (Maybe Expr) _) =

    stripMeta (IR.ParensExpr expr _) =
        IR.ParensExpr
            (stripMeta expr)
            Nothing

    stripMeta (IR.AppExpr e1 e2 _) =
        IR.AppExpr
            (stripMeta e1)
            (stripMeta e2)
            Nothing

    stripMeta (IR.AbsExpr arg expr _) =
        IR.AbsExpr
            (stripMeta arg)
            (stripMeta expr)
            Nothing




-- *
-- | ## Patterns
-- *

instance StripMeta IR.CaseAlt where
    stripMeta (IR.CaseAlt patrn expr _) =
        IR.CaseAlt
            (stripMeta patrn)
            (stripMeta expr)
            Nothing



instance StripMeta IR.Pattern where
    stripMeta (IR.LitPattern  value _) =
        IR.LitPattern
            (stripMeta value)
            Nothing

    stripMeta (IR.RecordPattern xs _) =
        IR.RecordPattern
            (map stripMeta xs)
            Nothing

    stripMeta (IR.ListPattern xs _) =
        IR.ListPattern
            (map stripMeta xs)
            Nothing
    
    stripMeta (IR.ConsPattern xs Nothing _) =
        IR.ConsPattern
            (map stripMeta xs)
            Nothing
            Nothing

    stripMeta (IR.ConsPattern xs (Just rest) _) =
        IR.ConsPattern
            (map stripMeta xs)
            (Just $ stripMeta rest)
            Nothing

    stripMeta (IR.TuplePattern items _) =
        IR.TuplePattern
            (map stripMeta items)
            Nothing

    stripMeta (IR.ConPattern name args _) =
        IR.ConPattern
            (stripMeta name)
            (map stripMeta args)
            Nothing

    stripMeta (IR.VarPattern name _) =
        IR.VarPattern
            (stripMeta name)
            Nothing

    stripMeta (IR.WildcardPattern _) =
        IR.WildcardPattern Nothing




-- *
-- | # Base
-- *



-- *
-- | ## Identifiers (Ident)
-- *


instance StripMeta a => StripMeta (IR.Sym a) where
    stripMeta (IR.Sym x ns  _) =
        IR.Sym
            (stripMeta x)
            (stripMeta ns)
            Nothing

instance StripMeta a => StripMeta (IR.Low a) where
    stripMeta (IR.Low x ns  _) =
        IR.Low
            (stripMeta x)
            (stripMeta ns)
            Nothing

instance StripMeta a => StripMeta (IR.Big a) where
    stripMeta (IR.Big x ns  _) =
        IR.Big
            (stripMeta x)
            (stripMeta ns)
            Nothing



instance StripMeta Text where
    stripMeta x = x

instance StripMeta IR.Namespace where
    stripMeta (IR.Namespace segs) =
        IR.Namespace segs


instance StripMeta (Maybe IR.Namespace) where
    stripMeta Nothing = Nothing
    stripMeta (Just ns) = Just $ stripMeta ns


-- *
-- | ## Types
-- *


instance StripMeta IR.Type where
    stripMeta (IR.LiteralType litTy) =
        IR.LiteralType
            (stripMeta litTy)

    stripMeta (IR.RecordType fields _) =
        IR.RecordType fields' Nothing
        
        where
            fields' = map field fields
            field (x, y) =
                (stripMeta x, stripMeta y)

    stripMeta (IR.TupleType  items _) =
        IR.TupleType
            (map stripMeta items)
            Nothing

    stripMeta (IR.ListType   ty _) =
        IR.ListType
            (stripMeta ty)
            Nothing

    stripMeta (IR.UnionType name args _) =
        IR.UnionType
            (stripMeta name)
            (map stripMeta args)
            Nothing

    stripMeta (IR.VarType name _) =
        IR.VarType
            (stripMeta name)
            Nothing

    stripMeta (IR.ArrType t1 t2 _) =
        IR.ArrType
            (stripMeta t1)
            (stripMeta t2)
            Nothing

    stripMeta (IR.ParensType ty _) =
        IR.ParensType
            (stripMeta ty)
            Nothing


instance StripMeta IR.LiteralType where
    stripMeta (IR.StringType _) =
        IR.StringType Nothing

    stripMeta (IR.CharType _) =
        IR.CharType Nothing

    stripMeta (IR.IntType _) =
        IR.IntType Nothing

    stripMeta (IR.FloatType _) =
        IR.FloatType Nothing

    stripMeta (IR.BoolType _) =
        IR.BoolType Nothing


-- *
-- | ### Type Schemes
-- *
instance StripMeta IR.Scheme where
    stripMeta (IR.Forall args ty) =
        IR.Forall
            (map stripMeta args)
            (stripMeta ty)







-- *
-- | ## Values
-- *


instance StripMeta IR.LiteralValue where
    stripMeta (IR.CharLit val _) =
        IR.CharLit
            val
            Nothing
        
    stripMeta (IR.StringLit val _) =
        IR.StringLit
            val
            Nothing
        
    stripMeta (IR.IntLit val _) =
        IR.IntLit
            val
            Nothing
        
    stripMeta (IR.FloatLit val _) =
        IR.FloatLit
            val
            Nothing
        
    stripMeta (IR.BoolLit val _) =
        IR.BoolLit
            val
            Nothing


-- *
-- | Etc.
-- *
instance StripMeta IR.Signature where
    stripMeta (IR.Validated scheme _) =
        IR.Validated
            (stripMeta scheme)
            Nothing

    stripMeta (IR.Unresolved ty _) =
        IR.Unresolved
            (stripMeta ty)
            Nothing





-- *
-- | # Header
-- *


instance StripMeta IR.ModuleExporting where
    stripMeta (IR.ExportExplicit entries) = IR.ExportExplicit (map stripMeta entries)
    stripMeta IR.ExportEverything = IR.ExportEverything



instance StripMeta IR.ModuleImporting where
    stripMeta (IR.ModuleImporting importDecls) =
        IR.ModuleImporting
            (map stripMeta importDecls)



instance StripMeta IR.ImportDecl where
    stripMeta (IR.ImportQualified namespace Nothing) = 
        IR.ImportQualified
            (stripMeta namespace)
            Nothing

    stripMeta (IR.ImportQualified namespace (Just asName)) =
        IR.ImportQualified
            (stripMeta namespace)
            (Just $ stripMeta asName)
        
    stripMeta (IR.ImportExplicit namespace entries) =
        IR.ImportExplicit
            (stripMeta namespace)
            (map stripMeta entries)
        
    stripMeta (IR.ImportEverything namespace) =
        IR.ImportEverything
            (stripMeta namespace)




-- | Base (Module Header) Types
--
instance StripMeta IR.Entry where
    stripMeta (IR.ValueEntry txt) =
        IR.ValueEntry
            txt

    stripMeta (IR.UnionEntry txt unionExposing) =
        IR.UnionEntry
            txt
            (stripMeta unionExposing)



instance StripMeta IR.UnionExposing where
    stripMeta IR.UnionEverything =
        IR.UnionEverything
        
    stripMeta (IR.UnionExplicit txts) =
        IR.UnionExplicit txts



















-- *
-- | Metadata, 'Source Code Location' & Related
-- *















