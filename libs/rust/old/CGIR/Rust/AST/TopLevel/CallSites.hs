{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Top-Level ‘Call Site’ Declarations
--
module CGIR.Rust.AST.TopLevel.CallSites (
      pattern Function
    , pattern Impl
    , IR.FunctionDecl
    , IR.ImplDecl
) where


-- *
import Core
import Data.Data (Data, Typeable)

--- Local
import qualified CGIR.Rust.Internal.AST        as IR
-- *



pattern Function :: IR.Low
                 -> Maybe IR.Self
                 -> [IR.Generic]
                 -> [IR.Input]
                 -> IR.Output
                 -> IR.Block
                 -> IR.FunctionDecl
pattern Function name amSelf generics inputs output blocl =
    IR.FunctionDecl name amSelf generics inputs output blocl


pattern Impl :: ImplPath -> [IR.FunctionDecl] -> IR.ImplDecl
pattern Impl path methods =
    IR.ImplDecl path methods


-- *
-- | Misc.
-- *

type ImplPath = IR.Path IR.Low

