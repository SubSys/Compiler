{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module CGIR.Metal.Render.Instances where


-- *
import Core
import Core.Control.Flow

import qualified Data.Maybe as Maybe
import qualified Data.List as List

--- Framework(s)
import Framework.Render'
import qualified Framework.Render'.Utils as Util

--- Local
import qualified CGIR.Metal.Render.Instances.Helpers as Helper


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






-- | TODO: Move to 'Render' Utils
renderMaybe' :: forall a. Doc -> (a -> Doc) -> Maybe a -> Doc
renderMaybe' defaultVal _ Nothing = defaultVal
renderMaybe' _ f (Just a) = f a


renderMaybe :: Render a => Maybe a -> Doc
renderMaybe = renderMaybe' "" render













-- *
-- | Top-Level Declarations
-- *

instance Render Decl.FunctionDecl where
    render = renderFunction


instance Render Decl.StructDecl where
    render (Decl.Struct optTmp name members) =
        renderStruct optTmp (Just name) members

    render (Decl.StructNoName optTmp members) =
        renderStruct optTmp Nothing members



instance Render Decl.EnumClassDecl where
    render (Decl.EnumClass name fields) =
        let name' = render name
            fields' = map render fields
                |> Util.punctuate ","
                |> Util.hcat
        in
                "enum class"
            <+> name'
            <+> Util.braces fields'



instance Render Decl.UnionDecl where
    render (Decl.Union name members) =
        renderUnion (Just name) members

    render (Decl.UnionNoName members) =
        renderUnion Nothing members








-- | Header Parameters
--
instance Render Header.FunctionSpecifier where
    render Header.Kernel   = "kernel"
    render Header.Vertex   = "vertex"
    render Header.Fragment = "fragment"


instance Render Header.Output where
    render (Header.Output ty) = render ty

instance Render Header.Input where
    render (Header.Input ty id' optAttr) =
        render ty <+> render id' <+> renderMaybe optAttr



instance Render Header.ArgAttribute where
    render (Header.DeviceBufferAttr i) =
        renderArgAttribute "buffer" i

    render (Header.ConstantBufferAttr i) =
        renderArgAttribute "buffer" i

    render (Header.TextureAttr i) =
        renderArgAttribute "texture" i

    render (Header.TextureArrayAttr i) =
        renderArgAttribute "texture" i

    render (Header.SamplerAttr i) =
        renderArgAttribute "sampler" i

    render (Header.ThreadgroupBufferAttr i) =
        renderArgAttribute "threadgroup" i



instance Render Header.TemplateDecl where
    render (Header.TemplateDecl args) =
        renderTemplateDecl args


instance Render Header.TemplateArg where
    render (Header.Typename id') =
        "typename" <+> render id'






-- *
-- | Block & Namespace Declarations
-- *

instance Render Decl.Block where
    render (Decl.Block stmts) = renderBlock stmts










-- *
-- | Block Level
-- *


-- | Members
--
instance Render Member.ObjectMember where
    render (Member.DataField ty name) =
        render ty <+> render name

    render (Member.Method decl name) =
        render decl <+> render name

    render (Member.EnumClass decl name) =
        render decl <+> render name

    render (Member.Struct decl name) =
        render decl <+> render name

    render (Member.Union decl name) =
        render decl <+> render name





-- | Statements
--


instance Render S.Stmt where
    render S.Default  = "default"
    render S.Break    = "break"
    render S.Continue = "continue"
    
    render (S.Return Nothing)     = "return"
    render (S.Return (Just stmt)) = "return" <+> render stmt

    render (S.If intros fallback) = renderIfStmt intros fallback
    render (S.Switch con cases)   = renderCaseStmt con cases

    render (S.Init ty id') = render ty <+> render id'
    render (S.Init' ty id' fields) = renderInitsBlock ty id' fields

    render (S.Var id') = render id'
    
    render (S.Scalar val) = render val
    render (S.Vector val) = render val
    render (S.Matrix val) = render val

    render (S.NameAccess path) = render path
    render (S.RefAccess path) = render path
    render (S.PointerAccess path) = render path
    render (S.Assign s1 s2) =
        render s1 <+> "=" <+> render s2
        
    render (S.Call ref args) =
        let args' = map render args
                |> Util.punctuate ","
                |> Util.hcat
            ref' = render ref
        in
            ref' <> Util.parens args'

    render (S.LocalBlock stmts) =
        renderBlock stmts








-- *
-- | Base
-- *


-- | Identifiers
--
instance Render ID.Big where
    render (ID.Big txt) = render txt


instance Render ID.Low where
    render (ID.Low txt) = render txt

instance Render ID.ScopePath where
    render (ID.ScopePath segs) =
        map render segs
            |> Util.punctuate "::"
            |> Util.hcat


instance Render ID.ValuePath where
    render (ID.ValuePath segs) =
        map render segs
            |> Util.punctuate "."
            |> Util.hcat


instance Render ID.QSeg where
    render (ID.BigSeg id') = render id'
    render (ID.LowSeg id') = render id'






-- | Types
--


instance Render T.Type where
    render T.Void = "void"
    render (T.Const ty) = "const" <+> render ty
    render (T.Constexpr ty) = "constexpr" <+> render ty
    render (T.Generic id') = render id'
    
    
    -- | Scalar Types
    --
    render T.Bool = "bool"
    render T.F32 = "float"
    render T.F16 = "half"
    render T.I32 = "int"
    render T.I16 = "short"
    render T.I8 = "char"
    render T.U32 = "uint"
    render T.U16 = "ushort"
    render T.U8 = "uchar"
    
    
    -- | Vector Types
    --
    render (T.BoolVec T.Index2Type) = "bool2"
    render (T.BoolVec T.Index3Type) = "bool3"
    render (T.BoolVec T.Index4Type) = "bool4"
    
    render (T.F32Vec T.Index2Type) = "float2"
    render (T.F32Vec T.Index3Type) = "float3"
    render (T.F32Vec T.Index4Type) = "float4"
    
    render (T.F16Vec T.Index2Type) = "half2"
    render (T.F16Vec T.Index3Type) = "half3"
    render (T.F16Vec T.Index4Type) = "half4"
    
    render (T.I32Vec T.Index2Type) = "int2"
    render (T.I32Vec T.Index3Type) = "int3"
    render (T.I32Vec T.Index4Type) = "int4"
    
    render (T.I16Vec T.Index2Type) = "short2"
    render (T.I16Vec T.Index3Type) = "short3"
    render (T.I16Vec T.Index4Type) = "short4"

    render (T.I8Vec T.Index2Type) = "char2"
    render (T.I8Vec T.Index3Type) = "char3"
    render (T.I8Vec T.Index4Type) = "char4"

    render (T.U32Vec T.Index2Type) = "uint2"
    render (T.U32Vec T.Index3Type) = "uint3"
    render (T.U32Vec T.Index4Type) = "uint4"

    render (T.U16Vec T.Index2Type) = "ushort2"
    render (T.U16Vec T.Index3Type) = "ushort3"
    render (T.U16Vec T.Index4Type) = "ushort4"
    
    render (T.U8Vec T.Index2Type) = "uchar2"
    render (T.U8Vec T.Index3Type) = "uchar3"
    render (T.U8Vec T.Index4Type) = "uchar4"

    
    -- | Matrix Types
    --
    render (T.F32Mat T.Dim2x2Type) = "float2x2"
    render (T.F32Mat T.Dim2x3Type) = "float2x3"
    render (T.F32Mat T.Dim2x4Type) = "float2x4"
    render (T.F32Mat T.Dim3x2Type) = "float3x2"
    render (T.F32Mat T.Dim3x3Type) = "float3x3"
    render (T.F32Mat T.Dim3x4Type) = "float3x4"
    render (T.F32Mat T.Dim4x2Type) = "float4x2"
    render (T.F32Mat T.Dim4x3Type) = "float4x3"
    render (T.F32Mat T.Dim4x4Type) = "float4x4"
    
    render (T.F16Mat T.Dim2x2Type) = "half2x2"
    render (T.F16Mat T.Dim2x3Type) = "half2x3"
    render (T.F16Mat T.Dim2x4Type) = "half2x4"
    render (T.F16Mat T.Dim3x2Type) = "half3x2"
    render (T.F16Mat T.Dim3x3Type) = "half3x3"
    render (T.F16Mat T.Dim3x4Type) = "half3x4"
    render (T.F16Mat T.Dim4x2Type) = "half4x2"
    render (T.F16Mat T.Dim4x3Type) = "half4x3"
    render (T.F16Mat T.Dim4x4Type) = "half4x4"
    
    
    -- | Address Space Attributes
    --
    -- render (T.Device ty)
    -- render (T.Threadgroup ty)
    -- render (T.ThreadgroupImgBlk ty)
    -- render (T.Constant ty)
    -- render (T.Thread ty)






-- | Values
--
instance Render V.ScalarValue where
    render (V.Bool True)  = "true"
    render (V.Bool False) = "false"
    
    render (V.F32 val) = render val
    render (V.F16 val) = render val
    render (V.I32 val) = render val
    render (V.I16 val) = render val
    render (V.I8 val)  = render val
    render (V.U32 val) = render val
    render (V.U16 val) = render val
    render (V.U8 val)  = render val


instance Render V.VectorValue where
    render (V.BoolVec val) = Helper.renderIndexValue "bool"   val
    render (V.F32Vec val)  = Helper.renderIndexValue "float"  val
    render (V.F16Vec val)  = Helper.renderIndexValue "half"   val
    render (V.I32Vec val)  = Helper.renderIndexValue "int"    val
    render (V.I16Vec val)  = Helper.renderIndexValue "short"  val
    render (V.I8Vec val)   = Helper.renderIndexValue "char"   val
    render (V.U32Vec val)  = Helper.renderIndexValue "uint"   val
    render (V.U16Vec val)  = Helper.renderIndexValue "ushort" val
    render (V.U8Vec val)   = Helper.renderIndexValue "uchar"  val


instance Render V.MatrixValue where
    render (V.F32Mat val) = Helper.renderDimValue "float" val
    render (V.F16Mat val) = Helper.renderDimValue "half"  val


















-- *
-- | Helpers
-- *


-- | Helpers for 'Top-Level Declaration' Instances
--
renderFunction :: Decl.FunctionDecl -> Doc
renderFunction (Decl.Function optTmp optSpecifier returnTy name inputs body) =
    let optTmp'       = renderMaybe optTmp
        optSpecifier' = renderMaybe optSpecifier
        returnTy'     = render returnTy
        name'         = render name
        body'         = render body
        inputs'       = map render inputs
            |> Util.punctuate ","
            |> Util.hcat
    in
             optTmp' <$$> optSpecifier'
        <+>  returnTy'
        <+>  name'
        <+>  Util.parens inputs'
        <+>  body'




renderStruct :: Maybe Header.TemplateDecl -> Maybe ID.Big -> [Member.ObjectMember] -> Doc
renderStruct optTmp optName members =
    let optTmp' = renderMaybe optTmp
        optName' = renderMaybe optName
        members' = map render members
            |> Util.punctuate ";"
            |> Util.vcat
            |> Util.indent 4

    in
        optTmp' <$$> optName' <> "{" <$$> members' <$$> "}"



renderUnion :: Maybe ID.Big -> [Member.ObjectMember] -> Doc
renderUnion optName members =
    let optName' = renderMaybe optName
        members' = map render members
            |> Util.punctuate ";"
            |> Util.vcat
            |> Util.indent 4
    in
        optName' <> "{" <$$> members' <$$> "}"










-- | Helpers for Header Instances
--
renderArgAttribute :: Doc -> Int -> Doc
renderArgAttribute label idx =
    "[[" <> label <> parens <> "]]"
    where
        parens = Util.parens $ render idx


renderTemplateDecl :: [Header.TemplateArg] -> Doc
renderTemplateDecl args =
    let args' = map render args
            |> Util.punctuate ","
            |> Util.hcat
    in
        "template" <> Util.angles args'










-- | Helpers for 'Block & Namespace' Instances
renderBlock :: [S.Stmt] -> Doc
renderBlock stmts =
    let body = map render stmts
            |> Util.punctuate ";"
            |> Util.vcat
            |> Util.indent 4
    in
        "{" <$$> body <$$> "}"










-- | Helpers for Statement Instances
renderIfStmt :: [(S.Stmt, Decl.Block)] -> Decl.Block -> Doc
renderIfStmt xs@(y:ys) elseDefault
    | List.length xs == 1 =
        ifBranch y <$$> elseBranch elseDefault
    
    | otherwise =
        ifBranch y <$$> Util.vcat (map elseIfBranch ys) <$$> elseBranch elseDefault


    where
        ifBranch (con, body) =
            let con' = render con |> Util.parens
                body' = render body
            in
                "if" <+> con' <$$> body'
        
        elseIfBranch (con, body) =
            let con' = render con |> Util.parens
                body' = render body
            in
                "else if" <+> con' <$$> body'
        
        elseBranch body =
            "else" <+> render body




renderCaseStmt :: S.Stmt -> [(ID.ScopePath, Decl.Block)] -> Doc
renderCaseStmt con cases =
    con' <+> cases'
    where
        con' =
            "switch" <+> Util.parens (render con)
        
        cases' =
            let body = map caseBranch cases
                    |> Util.vcat
                    |> Util.indent 4
            in
                "{" <$$> body <$$> "}"
        
        
        
        caseBranch (id', block) =
            "case" <+> render id' <> ":" <+> render block



renderInitsBlock :: T.Type -> ID.Low -> [S.InitField] -> Doc
renderInitsBlock ty var fields =
    ty' <+> var' <+> block
    
    where
        ty' = render ty
        var' = render var
        fields' = map renderInitField fields
            |> Util.punctuate ","
            |> Util.vcat
            |> Util.indent 4
        
        block =
            "{" <$$> fields' <$$> "}"



renderInitField :: S.InitField -> Doc
renderInitField (S.InitField path value) =
    render path <+> "=" <+> render value
