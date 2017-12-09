{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Auxiliary Types
-- That don’t return `IR.Stmt` -Organized here to avoid confusion...
-- And some misc.
module CGIR.Rust.AST.BlockLevel.Stmt.Aux (
    pattern MethodChain
    , IR.MethodChain
) where


-- *
import Core
import Data.Data (Data, Typeable)

--- Local
import qualified CGIR.Rust.Internal.AST        as IR
-- *


-- | 
-- A single callee e.g. `.callee(x, y, z)`.
pattern MethodChain :: IR.Low -> [IR.Stmt] -> IR.MethodChain
pattern MethodChain name args = IR.MethodChain name args


