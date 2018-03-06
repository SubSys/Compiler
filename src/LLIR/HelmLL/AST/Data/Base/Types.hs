{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.HelmLL.AST.Data.Base.Types (
    IR.Type
  , IR.Scheme
  
  , pattern Forall

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
) where


-- ~
import Core
import Core.Utils as Core
import Core.List.Util (singleton)

import Prelude (error, ($))

import qualified Data.List    as List
import qualified Data.Maybe   as Maybe
import qualified Data.Text    as Text
import qualified Data.String  as String

import qualified LLIR.HelmLL.Internal.AST as IR
-- ~


pattern Forall :: [IR.Ident] -> IR.Type -> IR.Scheme
pattern Forall as ty = IR.Forall as ty

-- | Types
--
pattern Tuple :: [IR.Type] -> IR.Type
pattern Tuple items = IR.TupleType items

pattern List :: IR.Type -> IR.Type
pattern List ty = IR.ListType ty

pattern Union :: IR.Ident -> [IR.Type] -> IR.Type
pattern Union name args = IR.UnionType name args

pattern Var :: IR.Ident -> IR.Type
pattern Var name = IR.VarType name

pattern Arr :: IR.Type -> IR.Type -> IR.Type
pattern Arr t1 t2 = IR.ArrType t1 t2

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




