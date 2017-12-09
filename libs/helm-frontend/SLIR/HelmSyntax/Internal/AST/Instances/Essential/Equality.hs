{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Internal.AST.Instances.Essential.Equality where


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
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ Instance Deps
import SLIR.HelmSyntax.Internal.AST.Instances.StripMeta ()
import SLIR.HelmSyntax.Internal.AST.Classes.StripMeta (StripMeta(..), stripMeta)
-- *




{-# ANN module "HLint: ignore" #-}









-- *
-- | Internal Specials
-- *

-- instance Eq IR.Binder where
--     (==) x y
--         | (stripMeta x) == (stripMeta y) = True
--         | otherwise = False
-- 
-- 
-- instance Eq IR.Ref where
--     (==) x y
--         | (stripMeta x) == (stripMeta y) = True
--         | otherwise = False



-- *
-- | # TopLevel
-- *



-- *
-- | ## Fixities
-- *
instance Eq IR.Infix where
    (==) (IR.InfixL sym1 opPrecedence1 _)
         (IR.InfixL sym2 opPrecedence2 _) =
             (sym1 == sym2) && (opPrecedence1 == opPrecedence2)

    (==) (IR.InfixR sym1 opPrecedence1 _)
         (IR.InfixR sym2 opPrecedence2 _) =
             (sym1 == sym2) && (opPrecedence1 == opPrecedence2)

    (==) (IR.InfixN sym1 opPrecedence1 _)
         (IR.InfixN sym2 opPrecedence2 _) =
             (sym1 == sym2) && (opPrecedence1 == opPrecedence2)




instance Eq IR.OpPrecedence where
    (==) (IR.OpPrecedence i1 _)
         (IR.OpPrecedence i2 _) =
             i1 == i2





-- *
-- | ## Functions
-- *

instance Eq IR.Function where
    (==) (IR.FnDecl name1 args1 expr1 optSig1 _)
         (IR.FnDecl name2 args2 expr2 optSig2 _) =
           (name1 == name2)
        && (args1 == args2)
        && (expr1 == expr2)
        && (optSig1 == optSig2)

    (==) (IR.OpDecl name1 args1 expr1 optSig1 _)
         (IR.OpDecl name2 args2 expr2 optSig2 _) =
           (name1 == name2)
        && (args1 == args2)
        && (expr1 == expr2)
        && (optSig1 == optSig2)
    
    _ == _ = False
    


-- *
-- | ## Unions
-- *

instance Eq IR.Union where
    (==) (IR.Union name1 args1 cons1 _)
         (IR.Union name2 args2 cons2 _) =
                (name1 == name2)
             && (args1 == args2)
             && (cons1 == cons2)


instance Eq IR.Constructor where
    (==) (IR.Constructor name1 args1 _)
         (IR.Constructor name2 args2 _) =
             (name1 == name2) && (args1 == args2)


-- *
-- | # TermLevel
-- *





-- *
-- | ## Expressions
-- *

instance Eq IR.Expr where
    (==) (IR.VarExpr name1 _)
         (IR.VarExpr name2 _) =
             name1 == name2

    (==) (IR.LitExpr value1 _)
         (IR.LitExpr value2 _) =
             value1 == value2

    (==) (IR.RecordExpr fields1 _)
         (IR.RecordExpr fields2 _) =
             fields1 == fields2

    (==) (IR.TupleExpr items1 _)
         (IR.TupleExpr items2 _) =
             items1 == items2

    (==) (IR.ListExpr xs1 _)
         (IR.ListExpr xs2 _) =
             xs1 == xs2

    (==) (IR.ConExpr name1 _)
         (IR.ConExpr name2 _) =
             name1 == name2

    (==) (IR.BinOpExpr sym e1 e2 _)
         (IR.BinOpExpr sym' e1' e2' _) =
                (sym == sym')
             && (e1 == e1')
             && (e2 == e2')

    (==) (IR.IfExpr intros1 else1 _)
         (IR.IfExpr intros2 else2 _) =
                (intros1 == intros2)
             && (else1 == else2)

    (==) (IR.LetExpr fns1 expr1 _)
         (IR.LetExpr fns2 expr2 _) =
             (fns1 == fns2) && (expr1 == expr2)

    (==) (IR.CaseExpr expr1 caseAlts1 _)
         (IR.CaseExpr expr2 caseAlts2 _) =
             (expr1 == expr2) && (caseAlts1 == caseAlts2)

    (==) (IR.RecordUpdateExpr name1 fields1 _)
         (IR.RecordUpdateExpr name2 fields2 _) =
             (name1 == name2) && (fields1 == fields2)

    (==) (IR.RecordAccessExpr name1 optExpr1 _)
         (IR.RecordAccessExpr name2 optExpr2 _) =
             (name1 == name2) && (optExpr1 == optExpr2)

    (==) (IR.ParensExpr expr1 _)
         (IR.ParensExpr expr2 _) =
             expr1 == expr2

    (==) (IR.AppExpr e1  e2  _)
         (IR.AppExpr e1' e2' _) =
             (e1 == e1') && (e2 == e2')

    (==) (IR.AbsExpr arg1 expr1 _)
         (IR.AbsExpr arg2 expr2 _) =
             (arg1 == arg2) && (expr1 == expr2)
    
    _ == _ = False



-- *
-- | ## Patterns
-- *

instance Eq IR.CaseAlt where
    (==) (IR.CaseAlt patrn1 expr1 _)
         (IR.CaseAlt patrn2 expr2 _) =
             (patrn1 == patrn2) && (expr1 == expr2)


instance Eq IR.Pattern where
    (==) (IR.LitPattern  value1 _)
         (IR.LitPattern  value2 _) =
             value1 == value2

    (==) (IR.RecordPattern names1 _)
         (IR.RecordPattern names2 _) =
             names1 == names2

    (==) (IR.ListPattern xs1 _)
         (IR.ListPattern xs2 _) =
             xs1 == xs2
    
    (==) (IR.ConsPattern xs1 rest1 _)
         (IR.ConsPattern xs2 rest2 _) =
             (xs1 == xs2) && (rest1 == rest2)
    
    (==) (IR.TuplePattern items1 _)
         (IR.TuplePattern items2 _) =
             items1 == items2

    (==) (IR.ConPattern name1 args1 _)
         (IR.ConPattern name2 args2 _) =
                (name1 == name2)
             && (args1 == args2)

    (==) (IR.VarPattern name1 _)
         (IR.VarPattern name2 _) =
             name1 == name2

    (==) (IR.WildcardPattern _)
         (IR.WildcardPattern _) = True
    
    _ == _ = False




-- *
-- | # Base
-- *



-- *
-- | ## Identifiers (Ident)
-- *


instance Eq a => Eq (IR.Low a) where
    (==) (IR.Low x1 ns1 _)
         (IR.Low x2 ns2 _) =
             (x1 == x2) && (ns1 == ns2)


instance Eq a => Eq (IR.Sym a) where
    (==) (IR.Sym x1 ns1 _)
         (IR.Sym x2 ns2 _) =
             (x1 == x2) && (ns1 == ns2)

instance Eq a => Eq (IR.Big a) where
    (==) (IR.Big x1 ns1 _)
         (IR.Big x2 ns2 _) =
             (x1 == x2) && (ns1 == ns2)


instance Eq IR.Namespace where
    (==) (IR.Namespace segs1)
         (IR.Namespace segs2) =
             segs1 == segs2

-- *
-- | ## Types
-- *


instance Eq IR.Type where
    (==) (IR.LiteralType litTy1)
         (IR.LiteralType litTy2) =
             litTy1 == litTy2

    (==) (IR.RecordType fields1 _)
         (IR.RecordType fields2 _) =
             fields1 == fields2

    (==) (IR.TupleType  items1 _)
         (IR.TupleType  items2 _) =
             items1 == items2

    (==) (IR.ListType   ty1 _)
         (IR.ListType   ty2 _) =
             ty1 == ty2

    (==) (IR.UnionType name1 args1 _)
         (IR.UnionType name2 args2 _) =
                (name1 == name2)
             && (args1 == args2)

    (==) (IR.VarType name1 _)
         (IR.VarType name2 _) =
             name1 == name2

    (==) (IR.ArrType t1  t2  _)
         (IR.ArrType t1' t2' _) =
                (t1 == t1')
             && (t2 == t2')

    (==) (IR.ParensType ty1 _)
         (IR.ParensType ty2 _) =
             ty1 == ty2
    
    _ == _ = False


instance Eq IR.LiteralType where
    (==) (IR.StringType _)
         (IR.StringType _) = True

    (==) (IR.CharType _)
         (IR.CharType _) = True

    (==) (IR.IntType _)
         (IR.IntType _) = True

    (==) (IR.FloatType _)
         (IR.FloatType _) = True

    (==) (IR.BoolType _)
         (IR.BoolType _) = True
    
    _ == _ = False


-- *
-- | ### Type Schemes
-- *
instance Eq IR.Scheme where
    (==) (IR.Forall args1 ty1)
         (IR.Forall args2 ty2) =
               (args1 == args2)
            && (ty1 == ty2)








-- *
-- | ## Values
-- *


instance Eq IR.LiteralValue where
    (==) (IR.CharLit value1 _)
         (IR.CharLit value2 _) =
             value1 == value2

    (==) (IR.StringLit value1 _)
         (IR.StringLit value2 _) =
             value1 == value2

    (==) (IR.IntLit value1 _)
         (IR.IntLit value2 _) =
             value1 == value2

    (==) (IR.FloatLit value1 _)
         (IR.FloatLit value2 _) =
             value1 == value2

    (==) (IR.BoolLit value1 _)
         (IR.BoolLit value2 _) =
             value1 == value2
    
    _ == _ = False



-- *
-- | Etc.
-- *
instance Eq IR.Signature where
    (==) (IR.Validated scheme1 _)
         (IR.Validated scheme2 _) =
             scheme1 == scheme2
    
    (==) (IR.Unresolved ty1 _)
         (IR.Unresolved ty2 _) =
             ty1 == ty2
    
    _ == _ = False





-- *
-- | # Header
-- *


instance Eq IR.ModuleExporting where
    (==) (IR.ExportExplicit entries1)
         (IR.ExportExplicit entries2) =
             entries1 == entries2
         
         
    (==) IR.ExportEverything
         IR.ExportEverything = True
    
    _ == _ = False



instance Eq IR.ModuleImporting where
    (==) (IR.ModuleImporting importDecls1)
         (IR.ModuleImporting importDecls2) =
             importDecls1 == importDecls2


instance Eq IR.ImportDecl where
    (==) (IR.ImportQualified namespace1 asName1)
         (IR.ImportQualified namespace2 asName2) =
                (namespace1 == namespace2)
             && (asName1 == asName2)
    
    (==) (IR.ImportExplicit namespace1 entries1)
         (IR.ImportExplicit namespace2 entries2) =
             (namespace1 == namespace2) && (entries1 == entries2)


    (==) (IR.ImportEverything namespace1)
         (IR.ImportEverything namespace2) =
             namespace1 == namespace2
    
    _ == _ = False




-- | Base (Module Header) Types
--
instance Eq IR.Entry where
    (==) (IR.ValueEntry txt1)
         (IR.ValueEntry txt2) =
             txt1 == txt2

    (==) (IR.UnionEntry txt1 unionExposing1)
         (IR.UnionEntry txt2 unionExposing2) =
             (txt1 == txt2) && (unionExposing1 == unionExposing2)
    
    _ == _ = False



instance Eq IR.UnionExposing where
    (==) IR.UnionEverything
         IR.UnionEverything = True

    (==) (IR.UnionExplicit txts1)
         (IR.UnionExplicit txts2) =
             txts1 == txts2
    
    _ == _ = False


















-- *
-- | Metadata, 'Source Code Location' & Related
-- *















