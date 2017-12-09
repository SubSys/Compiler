{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.TopLevel.Decls.Struct (
      IR.StructDecl
    , pattern Struct
    , pattern StructNoName
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *


pattern Struct :: Maybe IR.TemplateDecl -> IR.Big -> [IR.ObjectMember] -> IR.StructDecl
pattern Struct optTemp name members = IR.StructDecl optTemp (Just name) members


pattern StructNoName :: Maybe IR.TemplateDecl -> [IR.ObjectMember] -> IR.StructDecl
pattern StructNoName optTemp members = IR.StructDecl optTemp Nothing members
