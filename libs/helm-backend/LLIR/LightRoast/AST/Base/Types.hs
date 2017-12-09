{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.LightRoast.AST.Base.Types (
      IR.Type
    , pattern Record
    , pattern Tuple
    , pattern List
    , pattern Union
    , pattern Generic
    , pattern Fn
    , pattern String
    , pattern Char
    , pattern Int
    , pattern Float
    , pattern Bool
) where


-- *
import Core

--- Local
import qualified LLIR.LightRoast.Internal.AST as IR
-- *





pattern Record :: [(IR.Low, IR.Type)] -> IR.Type
pattern Record fields = IR.RecordType fields


pattern Tuple :: [IR.Type] -> IR.Type
pattern Tuple items = IR.TupleType items


pattern List :: IR.Type -> IR.Type
pattern List ty = IR.ListType ty


pattern Union :: IR.Big -> [IR.Type] -> IR.Type
pattern Union name args = IR.UnionType name args


pattern Generic :: IR.Generic -> IR.Type
pattern Generic name = IR.GenericType name


pattern Fn :: [IR.Type] -> IR.Output -> IR.Type
pattern Fn inTypes outType = IR.FnType inTypes outType





-- *
-- | Literal Types
-- *

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






