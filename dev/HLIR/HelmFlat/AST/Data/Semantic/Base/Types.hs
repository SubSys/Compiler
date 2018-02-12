{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module HLIR.HelmFlat.AST.Data.Semantic.Base.Types (
    IR.Type
  , IR.Scheme

  , pattern Tuple
  , pattern List
  , pattern Union
  , pattern Var
  , pattern Arr
  , pattern String
  , pattern Char
  , pattern Int
  , pattern Float
  , pattern Bool
  
  -- | Type Schemes
  --
  , pattern Forall
) where


-- *
import Core

--- Local
import qualified HLIR.HelmFlat.Internal.AST as IR
-- *


pattern Tuple :: [IR.Type] -> IR.Type
pattern Tuple ts = IR.TupleType ts

pattern List :: IR.Type -> IR.Type
pattern List ty = IR.ListType ty

pattern Union :: IR.Ident -> [IR.Type] -> IR.Type
pattern Union name args = IR.UnionType name args

pattern Var :: IR.Ident -> IR.Type
pattern Var id' = IR.VarType id'

pattern Arr :: IR.Type -> IR.Type -> IR.Type
pattern Arr ty1 ty2 = IR.ArrType ty1 ty2



-- *
-- | Literal Types
-- *
pattern String :: IR.Type
pattern String = IR.StringType

pattern Char :: IR.Type
pattern Char = IR.CharType

pattern Int :: IR.Type
pattern Int = IR.IntType

pattern Float :: IR.Type
pattern Float = IR.FloatType

pattern Bool :: IR.Type
pattern Bool = IR.BoolType





-- | Type Schemes
--

pattern Forall :: [IR.Ident] -> IR.Type -> IR.Scheme
pattern Forall as t = IR.Forall as t




