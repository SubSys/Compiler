{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.HelmLL.AST.Data.TopLevel.Unions (
    IR.Union
  , IR.Constr

  , pattern Union
  , pattern Constr
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

pattern Union :: IR.Ident -> [IR.Ident] -> [IR.Constr] -> IR.Union
pattern Union name args cons = IR.Union name args cons

pattern Constr :: IR.Ident -> [IR.Type] -> IR.Constr
pattern Constr name args = IR.Constr name args


