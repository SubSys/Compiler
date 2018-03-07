{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.RustCG.AST.Data.TopLevel.Enums (
    IR.Enum
  , pattern Enum
) where


-- *
import Core

--- Local
import qualified CGIR.RustCG.Internal.AST as IR
-- *


pattern Enum :: IR.Ident -> [IR.Generic] -> [IR.Variant] -> IR.Enum
pattern Enum name generics variants = IR.Enum name generics variants



