{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.RustCG.AST.Data.Semantic.Base.Types (
    IR.Type

  , pattern Fn
  , pattern Generic
  , pattern Union
  , pattern Box
  , pattern List
  , pattern Tuple
  , pattern String
  , pattern Char
  , pattern Int
  , pattern Float
  , pattern Bool
  
  , pattern Fn_
) where


-- *
import Core

--- Local
import qualified CGIR.RustCG.Internal.AST as IR
-- *



pattern Fn :: [IR.Type] -> IR.Output -> IR.Type
pattern Fn inTypes outType = IR.FnType inTypes outType

pattern Generic :: IR.Ident -> IR.Type
pattern Generic ident = IR.GenericType ident

pattern Union :: IR.Path -> [IR.Type] -> IR.Type
pattern Union path args = IR.UnionType path args

pattern Box :: IR.Type -> IR.Type
pattern Box ty = IR.BoxType ty

pattern List :: IR.Type -> IR.Type
pattern List ty = IR.ListType ty

pattern Tuple :: [IR.Type] -> IR.Type
pattern Tuple types = IR.TupleType types




-- | Literal Types
--

pattern String :: IR.Type
pattern String = IR.LiteralType IR.StringType

pattern Char :: IR.Type
pattern Char = IR.LiteralType IR.CharType

pattern Int :: IR.Type
pattern Int = IR.LiteralType IR.IntType

pattern Float :: IR.Type
pattern Float = IR.LiteralType IR.FloatType

pattern Bool :: IR.Type
pattern Bool = IR.LiteralType IR.BoolType




-- | Alternate variations - convenience helpers
--

pattern Fn_ :: [IR.Type] -> IR.Type -> IR.Type
pattern Fn_ inTypes outType = IR.FnType inTypes (IR.Output outType)




