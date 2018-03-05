{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module LLIR.SPMD.AST.Data.TopLevel.Functions (
    IR.Function
  , pattern Function
  , pattern Function_
) where


-- ~
import Core
import qualified LLIR.SPMD.Internal.AST as IR
-- ~




pattern Function :: IR.Type -> IR.Ident -> [IR.Input] -> IR.Block -> IR.Function
pattern Function retType name args body = IR.Function retType name args body




-- | Alternate variations - convenience helpers
--

pattern Function_ :: IR.Type -> IR.Ident -> [IR.Input] -> [IR.Stmt] -> IR.Function
pattern Function_ retType name args body = IR.Function retType name args (IR.Block body)


















