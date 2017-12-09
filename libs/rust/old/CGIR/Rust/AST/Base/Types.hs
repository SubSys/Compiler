{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Rust.AST.Base.Types (
      IR.Type(..)
    , IR.SequenceType(..)
    , IR.ScalarType(..)
    , IR.FnType(..)
    , TypePath
) where


-- *
import Core
import Data.Data (Data, Typeable)

--- Local
import qualified CGIR.Rust.Internal.AST        as IR
-- *


-- *
-- | Misc.
-- *
type TypePath = IR.Path IR.Type

