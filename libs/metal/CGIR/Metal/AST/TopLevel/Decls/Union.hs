{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.TopLevel.Decls.Union (
      IR.UnionDecl
    , pattern Union
    , pattern UnionNoName
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *

pattern Union ::  IR.Big -> [IR.ObjectMember] -> IR.UnionDecl
pattern Union name members = IR.UnionDecl (Just name) members

pattern UnionNoName ::  [IR.ObjectMember] -> IR.UnionDecl
pattern UnionNoName members = IR.UnionDecl Nothing members


