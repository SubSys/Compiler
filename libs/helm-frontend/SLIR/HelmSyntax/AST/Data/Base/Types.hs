{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Base.Types (
      IR.Type
    , IR.Scheme(..)

    , pattern Record
    , pattern Tuple
    , pattern List
    , pattern Union
    , pattern Var
    , pattern Arr
    , pattern Parens
    , pattern String
    , pattern Char
    , pattern Int
    , pattern Float
    , pattern Bool
    
    , pattern Record'
    , pattern Tuple'
    , pattern List'
    , pattern Union'
    , pattern Var'
    , pattern Arr'
    , pattern Parens'
    , pattern String'
    , pattern Char'
    , pattern Int'
    , pattern Float'
    , pattern Bool'
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ AST - Essential Instances
import SLIR.HelmSyntax.Internal.AST.Instances.Essential ()
-- *




pattern Record :: [(IR.Low Text, IR.Type)] -> Maybe IR.Meta -> IR.Type
pattern Record fields metaOpt = IR.RecordType fields metaOpt

pattern Tuple :: [IR.Type] -> Maybe IR.Meta -> IR.Type
pattern Tuple ts metaOpt = IR.TupleType ts metaOpt

pattern List :: IR.Type -> Maybe IR.Meta -> IR.Type
pattern List ty metaOpt = IR.ListType ty metaOpt

pattern Union :: IR.Big Text -> [IR.Type] -> Maybe IR.Meta -> IR.Type
pattern Union name args metaOpt = IR.UnionType name args metaOpt

pattern Var :: IR.Low Text -> Maybe IR.Meta -> IR.Type
pattern Var id' metaOpt = IR.VarType id' metaOpt

pattern Arr :: IR.Type -> IR.Type -> Maybe IR.Meta -> IR.Type
pattern Arr ty1 ty2 metaOpt = IR.ArrType ty1 ty2 metaOpt

pattern Parens :: IR.Type -> Maybe IR.Meta -> IR.Type
pattern Parens ty metaOpt = IR.ParensType ty metaOpt




-- *
-- | Literal Types
-- *
pattern String :: Maybe IR.Meta -> IR.Type
pattern String metaOpt = IR.LiteralType (IR.StringType metaOpt)

pattern Char :: Maybe IR.Meta -> IR.Type
pattern Char metaOpt = IR.LiteralType (IR.CharType metaOpt)

pattern Int :: Maybe IR.Meta -> IR.Type
pattern Int metaOpt = IR.LiteralType (IR.IntType metaOpt)

pattern Float :: Maybe IR.Meta -> IR.Type
pattern Float metaOpt = IR.LiteralType (IR.FloatType metaOpt)

pattern Bool :: Maybe IR.Meta -> IR.Type
pattern Bool metaOpt = IR.LiteralType (IR.BoolType metaOpt)



-- *
-- | Misc.
-- *

pattern Record' :: [(IR.Low Text, IR.Type)] -> IR.Type
pattern Record' fields = IR.RecordType fields Nothing

pattern Tuple' :: [IR.Type] -> IR.Type
pattern Tuple' ts = IR.TupleType ts Nothing

pattern List' :: IR.Type  -> IR.Type
pattern List' ty = IR.ListType ty Nothing

pattern Union' :: IR.Big Text -> [IR.Type] -> IR.Type
pattern Union' name args = IR.UnionType name args Nothing

pattern Var' :: IR.Low Text -> IR.Type
pattern Var' id' = IR.VarType id' Nothing

pattern Arr' :: IR.Type -> IR.Type -> IR.Type
pattern Arr' ty1 ty2 = IR.ArrType ty1 ty2 Nothing


pattern Parens' :: IR.Type -> IR.Type
pattern Parens' ty = IR.ParensType ty Nothing

pattern String' :: IR.Type
pattern String' = IR.LiteralType (IR.StringType Nothing)

pattern Char' :: IR.Type
pattern Char' = IR.LiteralType (IR.CharType Nothing)

pattern Int' :: IR.Type
pattern Int' = IR.LiteralType (IR.IntType Nothing)

pattern Float' :: IR.Type
pattern Float' = IR.LiteralType (IR.FloatType Nothing)

pattern Bool' :: IR.Type
pattern Bool' = IR.LiteralType (IR.BoolType Nothing)