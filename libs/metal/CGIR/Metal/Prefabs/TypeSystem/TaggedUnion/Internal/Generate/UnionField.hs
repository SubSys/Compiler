{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Metal.Prefabs.TypeSystem.TaggedUnion.Internal.Generate.UnionField (
    unionField
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





-- | Maybe cause confusion?
type Structure = Member.ObjectMember
type Constructor = (ID.Big, [T.Type])


unionField :: [Constructor] -> Structure
unionField = structure


-- *
-- | Internal (For Now?)
-- *
structure :: [Constructor] -> Member.ObjectMember
structure cons =
    let name = ID.Low $ Text.pack "_u"
        body = Decl.UnionNoName $ map constructor cons
    in
        Member.Union body name


constructor :: Constructor -> Member.ObjectMember
constructor (conName, []) =
    let body = Decl.StructNoName Nothing []
        name = serializeConName conName
    
    in
        Member.Struct body name

constructor (conName, ts) =
    let name = serializeConName conName
        body = Decl.StructNoName Nothing $ imap dataField ts
    in
        Member.Struct body name

    where
        genArgName :: Int -> ID.Low
        genArgName idx =
            let prefix = Text.pack "arg"
                idx' = Text.pack $ show idx
                name = prefix `Text.append` idx'
            in
                ID.Low name
        
        dataField :: Int -> T.Type -> Member.ObjectMember
        dataField idx ty =
            let
                name = genArgName idx
            in
                Member.DataField ty name




serializeConName :: ID.Big -> ID.Low
serializeConName (ID.Big txt) =
    let txt' = Text.toLower txt
        suffix = Text.pack "Tag"
        
        name = txt' `Text.append` suffix
    in
        ID.Low name






