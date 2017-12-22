{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmIntro.AST.Data.Base.Types (
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
    
    , pattern Superposed
) where


-- *
import Core

--- Local
import qualified HLIR.HelmIntro.Internal.AST as IR
-- *




pattern Record :: [(IR.Low, IR.Type)] ->  IR.Type
pattern Record fields = IR.RecordType fields

pattern Tuple :: [IR.Type] ->  IR.Type
pattern Tuple ts = IR.TupleType ts

pattern List :: IR.Type ->  IR.Type
pattern List ty = IR.ListType ty

pattern Union :: IR.Big -> [IR.Type] ->  IR.Type
pattern Union name args = IR.UnionType name args

pattern Var :: IR.Low ->  IR.Type
pattern Var id' = IR.VarType id'

pattern Arr :: IR.Type -> IR.Type ->  IR.Type
pattern Arr ty1 ty2 = IR.ArrType ty1 ty2

pattern Parens :: IR.Type ->  IR.Type
pattern Parens ty = IR.ParensType ty




-- *
-- | Literal Types
-- *
pattern String ::  IR.Type
pattern String = IR.LiteralType IR.StringType

pattern Char ::  IR.Type
pattern Char = IR.LiteralType IR.CharType

pattern Int ::  IR.Type
pattern Int = IR.LiteralType IR.IntType

pattern Float ::  IR.Type
pattern Float = IR.LiteralType IR.FloatType

pattern Bool ::  IR.Type
pattern Bool = IR.LiteralType IR.BoolType




-- *
-- | Etc
-- *


pattern Superposed :: IR.Type -> [IR.Type] -> IR.Type
pattern Superposed con ts = IR.Superposed con ts





