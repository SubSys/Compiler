{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.TopLevel.Decls.EnumClass (
      IR.EnumClassDecl
    , pattern EnumClass
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *


pattern EnumClass :: IR.Big -> [IR.Big] -> IR.EnumClassDecl
pattern EnumClass name fields = IR.EnumClassDecl (Just name) fields


