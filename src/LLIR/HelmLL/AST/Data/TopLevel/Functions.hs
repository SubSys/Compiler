{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.HelmLL.AST.Data.TopLevel.Functions (
    IR.Function

  , pattern Function
  , pattern Function_
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


pattern Function :: IR.Binder -> [IR.Binder] -> IR.Block -> Maybe IR.Scheme -> IR.Function
pattern Function name args block scheme = IR.Function name args block scheme


-- | Alternate variations - convenience helpers
--

pattern Function_ :: IR.Binder -> [IR.Binder] -> [IR.Stmt] -> Maybe IR.Scheme -> IR.Function
pattern Function_ name args stmts scheme = IR.Function name args (IR.Block stmts) scheme


