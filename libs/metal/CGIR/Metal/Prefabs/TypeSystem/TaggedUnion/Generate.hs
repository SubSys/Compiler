{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Metal.Prefabs.TypeSystem.TaggedUnion.Generate (
    generate
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
import qualified
    CGIR.Metal.Prefabs.TypeSystem.TaggedUnion.Internal.Generate.TagField as TagField

import qualified
    CGIR.Metal.Prefabs.TypeSystem.TaggedUnion.Internal.Generate.UnionField as UnionField



-- ~ Metal AST
-- ~~ Base
import qualified CGIR.Metal.AST.Base.Ident       as ID

import qualified CGIR.Metal.AST.Base.Types       as T
import qualified CGIR.Metal.AST.Base.Types.Aux   as T

import qualified CGIR.Metal.AST.Base.Values      as V
import qualified CGIR.Metal.AST.Base.Values.Aux  as V

-- ~~ BlockLevel
import qualified CGIR.Metal.AST.BlockLevel.Stmts     as S
import qualified CGIR.Metal.AST.BlockLevel.Stmts.Aux as S

import qualified CGIR.Metal.AST.BlockLevel.Members as Member

-- ~~ TopLevel
import qualified CGIR.Metal.AST.TopLevel.Decls   as Decl
import qualified CGIR.Metal.AST.TopLevel.Decls.Header.Parameters as Header
-- *



type GenericArgs = [ID.Big]


type StructureName = ID.Big
type ConstructorName = ID.Big
type ConstructorArgs = [T.Type]


generate :: StructureName
         -> GenericArgs
         -> [(ConstructorName, ConstructorArgs)]
         -> Decl.StructDecl

generate strName generics cons =
    let tagField = TagField.tagField $ map getConName cons
        unionField = UnionField.unionField cons
        strName' = serializeStrName strName
        
    in
        Decl.Struct (template generics) strName'
            [ tagField
            , unionField
            ]
    
    where
        template :: [ID.Big] -> Maybe Header.TemplateDecl
        template []   = Nothing
        template args =
            Just (Header.TemplateDecl $ map Header.Typename args)


        getConName :: (ConstructorName, ConstructorArgs) -> ConstructorName
        getConName (name, _) = name





serializeStrName :: ID.Big -> ID.Big
serializeStrName (ID.Big txt) =
    let prefix = Text.pack "Union"
        name = txt `Text.append` prefix
    in
        ID.Big name


