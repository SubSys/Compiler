{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.AST.Data.Semantic.Base.Types (
    IR.Type
  , IR.Scheme

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
    
  , pattern Superposed
  
  -- | Type Schemes
  --
  , pattern Forall
) where


-- *
import Core

--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR
-- *


pattern Tuple :: [IR.Type] -> IR.Meta -> IR.Type
pattern Tuple ts meta = IR.TupleType ts meta

pattern List :: IR.Type -> IR.Meta -> IR.Type
pattern List ty meta = IR.ListType ty meta

pattern Union :: IR.Ident -> [IR.Type] -> IR.Meta -> IR.Type
pattern Union name args meta = IR.UnionType name args meta

pattern Var :: IR.Ident -> IR.Meta -> IR.Type
pattern Var id' meta = IR.VarType id' meta

pattern Arr :: IR.Type -> IR.Type -> IR.Meta -> IR.Type
pattern Arr ty1 ty2 meta = IR.ArrType ty1 ty2 meta

pattern Parens :: IR.Type -> IR.Meta -> IR.Type
pattern Parens ty meta = IR.ParensType ty meta




-- *
-- | Literal Types
-- *
pattern String :: IR.Meta -> IR.Type
pattern String meta = IR.StringType meta

pattern Char :: IR.Meta -> IR.Type
pattern Char meta = IR.CharType meta

pattern Int :: IR.Meta -> IR.Type
pattern Int meta = IR.IntType meta

pattern Float :: IR.Meta -> IR.Type
pattern Float meta = IR.FloatType meta

pattern Bool :: IR.Meta -> IR.Type
pattern Bool meta = IR.BoolType meta



-- | Alternative variations & misc. helpers.
--


pattern Tuple' :: [IR.Type] -> IR.Type
pattern Tuple' ts <- IR.TupleType ts _
    where
        Tuple' ts = IR.TupleType ts IR.Empty

pattern List' :: IR.Type  -> IR.Type
pattern List' ty <- IR.ListType ty _
    where
        List' ty = IR.ListType ty IR.Empty

pattern Union' :: IR.Ident -> [IR.Type] -> IR.Type
pattern Union' name args <- IR.UnionType name args _
    where
        Union' name args = IR.UnionType name args IR.Empty

pattern Var' :: IR.Ident -> IR.Type
pattern Var' id' <- IR.VarType id' _
    where
        Var' id' = IR.VarType id' IR.Empty

pattern Arr' :: IR.Type -> IR.Type -> IR.Type
pattern Arr' ty1 ty2 <- IR.ArrType ty1 ty2 _
    where
        Arr' ty1 ty2 = IR.ArrType ty1 ty2 IR.Empty


pattern Parens' :: IR.Type -> IR.Type
pattern Parens' ty <- IR.ParensType ty _
    where
        Parens' ty = IR.ParensType ty IR.Empty

pattern String' :: IR.Type
pattern String' <- IR.StringType _
    where
        String' = IR.StringType IR.Empty

pattern Char' :: IR.Type
pattern Char' <- IR.CharType _
    where
        Char' = IR.CharType IR.Empty

pattern Int' :: IR.Type
pattern Int' <- IR.IntType _
    where
        Int' = IR.IntType IR.Empty

pattern Float' :: IR.Type
pattern Float' <- IR.FloatType _
    where
        Float' = IR.FloatType IR.Empty

pattern Bool' :: IR.Type
pattern Bool' <- IR.BoolType _
    where
        Bool' = IR.BoolType IR.Empty


-- *
-- | Etc
-- *


pattern Superposed :: [IR.Ident] -> [IR.Type] -> IR.Type
pattern Superposed con ts = IR.Superposed (IR.Forall con) ts



-- *
-- | # Type Schemes
-- *

pattern Forall :: [IR.Ident] -> IR.Type -> IR.Scheme
pattern Forall as t = IR.Scheme (IR.Forall as) t




