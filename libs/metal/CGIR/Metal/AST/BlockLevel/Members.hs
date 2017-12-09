{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module CGIR.Metal.AST.BlockLevel.Members (
      IR.ObjectMember
    , pattern DataField
    , pattern Method
    , pattern EnumClass
    , pattern Struct
    , pattern Union
) where


-- *
import Core

--- Local
import qualified CGIR.Metal.Internal.AST as IR
-- *

pattern DataField :: IR.Type -> IR.Low -> IR.ObjectMember
pattern DataField ty name = IR.DataFieldMember ty name

pattern Method :: IR.FunctionDecl -> IR.Low -> IR.ObjectMember
pattern Method decl name = IR.MethodMember decl name

pattern EnumClass :: IR.EnumClassDecl -> IR.Low -> IR.ObjectMember
pattern EnumClass decl name = IR.EnumClassMember decl name

pattern Struct :: IR.StructDecl -> IR.Low -> IR.ObjectMember
pattern Struct decl name = IR.StructMember decl name

pattern Union :: IR.UnionDecl -> IR.Low -> IR.ObjectMember
pattern Union decl name = IR.UnionMember decl name



