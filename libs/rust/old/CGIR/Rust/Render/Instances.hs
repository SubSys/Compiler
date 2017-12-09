{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.Rust.Render.Instances where


-- *
import Core
import Core.Control.Flow

import qualified Data.Maybe as Maybe
import qualified Data.List as List

--- Framework(s)
import Framework.Render'
import qualified Framework.Render'.Utils as Util

--- Local
-- ~ RedRust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Etc    as Etc
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Types  as T
-- ~~ BlockLevel
import qualified CGIR.Rust.AST.BlockLevel.Patterns as P
import qualified CGIR.Rust.AST.BlockLevel.Stmts    as S
import qualified CGIR.Rust.AST.BlockLevel.Stmt.Aux as StmtAux
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.CallSites  as CS
import qualified CGIR.Rust.AST.TopLevel.TypeSystem as TS
-- *






-- *
-- | Top-Level Call Site Declarations
-- *



-- | Function Declarations
--
instance Render CS.FunctionDecl where
    render = renderFunction






renderFunction :: CS.FunctionDecl -> Doc
renderFunction (CS.Function name amSelf generics inputs output block) =
    let header = fnHeader name amSelf generics inputs output
    in
        header <+>  render block


-- | Render Function Helpers
--
fnHeader name amSelf generics inputs output =
    let name' = render name
        amSelf' = amSelfHelp amSelf
        generics' = genericsHelp generics
        inputs' = inputsHelp inputs 
        output' = render output
    in 
            "fn"
        <+> name'
        <+> generics'
        <+> inputs'
        <+> output'
    
    where
        amSelfHelp :: Maybe Etc.Self -> Doc
        amSelfHelp Nothing = ""
        amSelfHelp (Just Etc.Self) = "self"
        
        genericsHelp [] = ""
        genericsHelp xs =
            map render xs
                |> Util.punctuate ", "
                |> Util.hcat
                |> Util.angles
        
        inputsHelp [] = "()"
        inputsHelp xs =
            map render xs
                |> Util.punctuate ", "
                |> Util.hcat
                |> Util.parens



-- | Impl Declarations
---
instance Render CS.ImplDecl where
    render (CS.Impl path methods) =
        render path <+> renderMethods methods


renderMethods :: [CS.FunctionDecl] -> Doc
renderMethods methods =
    map render methods
        |> Util.vcat
        |> Util.indent 4




-- *
-- | Top-Level Struct/Enum Type Declarations
-- *



-- | Enum Declaration
--
instance Render TS.EnumDecl where
    render = renderEnumDecl


renderEnumDecl :: TS.EnumDecl -> Doc
renderEnumDecl (TS.Enum name genArgs variants) =
    let args = map render genArgs
                |> Util.punctuate ","
                |> Util.hcat
                |> Util.angles
        body = map render variants
                |> Util.punctuate ","
                |> Util.vcat
                |> Util.indent 4
        body' = "{" <$$> body <$$> "}"
    in if null genArgs then
        "enum" <+> render name <+> body'
    else
        "enum" <+> render name <+> args <+> body'



instance Render TS.VariantDecl where
    render (TS.StructVariant name fields) =
        renderStructVariant name fields
        
    render (TS.TupleVariant name args) =
        renderTupleVariant name args

    render (TS.UnitVariant name) =
        render name






-- | Struct Declaration
--
instance Render TS.StructDecl where
    render (TS.Struct id' fields) =
            "struct"
        <+> renderStructDecl id' fields





-- *
-- | Struct/Enum Helpers
-- *
renderStructDecl = renderStructVariant

renderStructVariant :: ID.Big -> [(ID.Low, T.Type)] -> Doc
renderStructVariant name fields =
    render name <+> renderFields fields

renderTupleVariant :: ID.Big -> [T.Type] -> Doc
renderTupleVariant name [] =
    render name

renderTupleVariant name args =
    render name <+> Util.parens args'
    where
        args' =
            map render args
                |> Util.punctuate ","
                |> Util.hcat



renderFields :: [(ID.Low, T.Type)] -> Doc
renderFields fields =
    let body = map renderField fields
                |> Util.punctuate ","
                |> Util.vcat
                |> Util.indent 4
    in
        "{" <$$> body <$$> "}"


renderField :: (ID.Low, T.Type) -> Doc
renderField (id', ty) =
    let id_ = render id'
        ty' = render ty
    in
        id_ <> ":" <+> ty'






-- *
-- | Block Declarations
-- *
instance Render Etc.Block where
    render (Etc.Block body) =
        renderBlock body


renderBlock :: [S.Stmt] -> Doc
renderBlock stmts =
    let body = map render stmts
            |> Util.punctuate ";"
            |> Util.vcat
            |> Util.indent 4
    in
        "{" <$$> body <$$> "}"







-- *
-- | Statement Declarations
-- *
instance Render S.Stmt where
    render (S.VarRef id')            = render id'
    render S.SelfRef                 = "self"
    render (S.SelfVarRef Nothing)    = "self"
    render (S.SelfVarRef (Just id')) = "self" <> "." <> render id'
    render (S.VarRef id')            = render id'
    render (S.PathRef path)          = render path
    
    render (S.Assign s1 s2) =
            render s1
        <+> "="
        <+> render s2
    
    render (S.FnCall path) =
        render path
        
        -- DELETE ME:
        -- let args' = map render args
        --         |> Util.punctuate ", "
        --         |> Util.hcat
        -- in
        --     render name <> Util.parens args'
    
    
    render (S.MethodCall name args) =
        renderMethdCall name args
    
    render (S.ImplCall path methods) =
        let body = map render methods
                    |> Util.vcat
                    |> Util.indent 4
        in
            render path <$$> body
    
    render (S.Variant path) =
        render path
    
    render (S.Struct path fields) =
        let body = map field fields
                |> Util.punctuate ","
                |> Util.vcat
                |> Util.indent 4
            path' = render path
        in
            path' <+> "{" <$$> body <$$> "}"
        
        where
            field (id', stmt) =
                render id' <> ":" <+> render stmt
    
    render (S.Tuple xs) =
        map render xs
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.parens

    
    render (S.Scalar val) = render val
    render (S.LetDecl id' stmt) =
        "let" <+> render id' <+> "=" <+> render stmt

    render (S.Block body) = renderBlock body
    render (S.FnDecl fn) = render fn
    
    render (S.Match stmt arms) =
            "match"
        <+> render stmt
        <+> body
        
        where
            body' = map render arms
                |> Util.punctuate ","
                |> Util.vcat
                |> Util.indent 4
            body =
                "{" <$$> body' <$$> "}"











instance Render StmtAux.MethodChain where
    render (StmtAux.MethodChain name args) =
        renderMethdCall name args




-- | 'Render Method Call' (Helper)
--
renderMethdCall :: ID.Low -> [S.Stmt] -> Doc
renderMethdCall name args =
    let args' = map render args
            |> Util.punctuate ","
            |> Util.hcat
    in
        "." <> render name <> Util.parens args'
    




-- *
-- | # Patterns
-- *

-- | Pattern Branches (Arms)
instance Render P.Arm where
    render (P.Arm patrn stmt) =
            render patrn
        <+> "=>"
        <+> render stmt



-- | Patterns
instance Render P.Pattern where
    render (P.Var id') = render id'
    render P.Wildcard = "_"
    render (P.Scalar val) = render val
    render (P.Variant path) = render path
    render (P.Tuple ps) =
        map render ps
            |> Util.punctuate ","
            |> Util.vcat
            |> Util.parens
















-- *
-- | # Base
-- *






-- *
-- | ### Simple Identifiers/References
-- *
instance Render ID.Low where
    render (ID.Low name) =
        render name


instance Render ID.Big where
    render (ID.Big name) =
        render name


-- *
-- | ### (External) Identification System
-- *
instance Render a => Render (ID.Path a) where
    render (ID.Path segs) =
        map render segs
            |> Util.punctuate "::"
            |> Util.hcat


instance Render a => Render (ID.Segment a) where
    render (ID.BigSeg name tyArgs amSelf inArgs) =
        let tys = renderTyArgs tyArgs
            ins =  renderInArgs amSelf inArgs
        in
            render name <+> tys <+> ins
        
    render (ID.LowSeg name tyArgs amSelf inArgs) =
        let tys = renderTyArgs tyArgs
            ins =  renderInArgs amSelf inArgs
        in
            render name <+> tys <+> ins





renderInArgs :: Render a => Maybe Etc.Self -> [a] -> Doc
renderInArgs (Just _) [] = "self"
renderInArgs Nothing [] = ""

renderInArgs amSelf xs =
    map render xs
        |> Util.punctuate ","
        |> ([renSelf amSelf] ++) 
        |> Util.hcat
        |> Util.parens
    
    where
        renSelf Nothing = ""
        renSelf (Just _) = "self "


renderTyArgs :: [T.Type] -> Doc
renderTyArgs [] = ""
renderTyArgs xs =
    map render xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.angles


-- instance Render ID.TyArg where
--     render (ID.TyArg ty) = render ty
-- 
-- instance Render a => Render (ID.InArg a) where
--     render (ID.InArg x) = render x
--     render (ID.InSelf Etc.Self) = "self"




-- *
-- | ## Type System
-- *
instance Render T.Type where
    render (T.ScalarType ty) =
        render ty
    
    render (T.TupleType ts) =
        map render ts
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.parens
    
    render (T.EnumPath path) =
        render path

    render (T.StructPath path) =
        render path

    render (T.GenericType id') =
        render id'
    
    render (T.FnTypeDecl fnType) =
        render fnType
    
    render (T.SequenceType ty) =
        render ty





instance Render T.FnType where
    render (T.FnType ins out) =
        renderFnType ins out


renderFnType :: [T.Type] -> T.Type -> Doc
renderFnType ins out =
        "&Fn"
    <+> ins'
    <+> "->"
    <+> render out
    where
        ins' = map render ins
                |> Util.punctuate ","
                |> Util.hcat
                |> Util.parens





instance Render T.SequenceType where
    render (T.Vec ty) =
        "Vec" <> Util.angles (render ty)




instance Render T.ScalarType where
    render T.Char   = "char"
    render T.String = "str"
    render T.Bool   = "bool"
    render T.I8     = "i8"
    render T.I16    = "i16"
    render T.I32    = "i32"
    render T.I64    = "i64"
    render T.I128   = "i128"
    render T.U8     = "u8"
    render T.U16    = "u16"
    render T.U32    = "u32"
    render T.U64    = "u64"
    render T.U128   = "u128"
    render T.F32    = "f32"
    render T.F64    = "f64"








-- *
-- | ## Values
-- *


instance Render V.ScalarValue where
    render (V.Char val)   = render val |> Util.squotes
    render (V.String val) = render val |> Util.dquotes
    render (V.Bool val)   = render val
    render (V.I8 val)     = render val
    render (V.I16 val)    = render val
    render (V.I32 val)    = render val
    render (V.I64 val)    = render val
    render (V.I128 val)   = render val
    render (V.U8 val)     = render val
    render (V.U16 val)    = render val
    render (V.U32 val)    = render val
    render (V.U64 val)    = render val
    render (V.U128 val)   = render val
    render (V.F32 val)    = render val
    render (V.F64 val)    = render val












-- *
-- | ## Etc
-- *
instance Render Etc.Input where
    render (Etc.Input low ty) =
        render low <> ":" <+> render ty


instance Render Etc.Output where
    render (Etc.Output ty) =
        "->" <+> render ty



instance Render Etc.Generic where
    render (Etc.Generic id') =
        render id'
