{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Metal.Prefabs.TypeSystem.TaggedUnion.Internal.Generate.TagField (
    tagField
) where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)
import Prelude (mapM_, IO, String, return, show)
import Data.List.Index (imap)

import qualified Data.Text as Text
import qualified Data.List as List

--- Local


-- ~ Metal AST
-- ~~ Base
import qualified CGIR.Metal.AST.Base.Ident       as ID
import qualified CGIR.Metal.AST.Base.Types       as T
import qualified CGIR.Metal.AST.Base.Values      as V
-- ~~ BlockLevel
import qualified CGIR.Metal.AST.BlockLevel.Stmts   as S
import qualified CGIR.Metal.AST.BlockLevel.Members as Member
-- ~~ TopLevel
import qualified CGIR.Metal.AST.TopLevel.Decls   as Decl
import qualified CGIR.Metal.AST.TopLevel.Decls.Header.Parameters as Param
-- *



tagField :: [ID.Big] -> Member.ObjectMember
tagField conNames =
    let
        typeName = ID.Big $ Text.pack "Tag"
        memberName = ID.Low $ Text.pack "tag"
        
        enumClassDecl = Decl.EnumClass typeName $ map genTagName conNames
    in
        Member.EnumClass enumClassDecl memberName



genTagName :: ID.Big -> ID.Big
genTagName (ID.Big txt) =
    let prefix = Text.pack "Tag"
        name = txt `Text.append` prefix
    in
        ID.Big name


