{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HLIR.HelmCore.Render.Instances where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import Prelude (show)

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Text as Text

--- Framework(s)
import Framework.Render
import qualified Framework.Render.Utils as Util

--- Local
-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as ID
import qualified HLIR.HelmCore.AST.Base.Types  as T
import qualified HLIR.HelmCore.AST.Base.Values as V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as Decl
-- *








-- *
-- | # TopLevel
-- *






-- *
-- | ## Functions
-- *

instance Render Decl.Function where
    render (Decl.Function name expr sig) =
        let name' = render name
            expr' = render expr
            sig'  = render sig
        in
                 name' <+> ":" <+> sig'
            <$$> name' <+> "=" <$$> Util.indent 4 expr' <> Util.linebreak



-- *
-- | ## Unions
-- *
instance Render Decl.Union where
    render (Decl.Union name vars cons) =
        let name' = render name
            cons' = renderConstructors cons
            vars' = map render vars
                |> Util.punctuate Util.space
                |> Util.hcat
        in
            "type" <+> name' <+> vars' <$$> cons' <$$> Util.softline


instance Render Decl.Constructor where
    render (Decl.Constructor name args) =
        let name' = render name
            args' = map render args
              |> Util.punctuate Util.space
              |> Util.hcat
        in
            name' <+> args'



renderConstructors :: [Decl.Constructor] -> Doc
renderConstructors [x] = Util.indent 4 $ "=" <+> render x
renderConstructors (x:xs) =
    let rest = map constr xs
          |> Util.vcat
    in
        Util.indent 4 $ "=" <+> render x <$$> rest
    
    where
        constr con =
            "|" <+> render con




















-- *
-- | # TermLevel
-- *




















-- *
-- | ## Expressions
-- *

instance Render E.Expr where
    render (E.Var id') = render id'
    render (E.Lit val) = render val


    render (E.Tuple items) =
        map render items
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.parens


    render (E.List xs) =
        map render xs
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.brackets


    render (E.Con name) =
        render name


    render (E.Let fns expr) =
        renderLet fns expr


    render (E.Case expr caseAlts) =
        renderCase expr caseAlts

    render (E.App e1 e2) =
        parens $ renderApp e1 e2

    render (E.Abs arg expr) =
        renderAbs arg expr
    
    render (E.Record fields) =
        map field fields
            |> Util.punctuate ","
            |> Util.punctuate Util.softline
            |> Util.vcat
            |> Util.braces
        where
            field :: (ID.Low, E.Expr) -> Doc
            field (name, val) = render name <+> ":" <+> render val




renderLet :: [Decl.Function] -> E.Expr -> Doc
renderLet fns expr =
    let fns' = map render fns
          |> Util.punctuate Util.linebreak
          |> Util.vcat
        expr' = render expr
    in
        "let" <> Util.indent 1 fns' <$$> "in" <$$> Util.indent 4 expr'


renderCase :: E.Expr -> [P.CaseAlt] -> Doc
renderCase expr caseAlts =
    let expr'     = render expr
        caseAlts' = map render caseAlts
          |> Util.punctuate Util.linebreak
          |> Util.hcat
          |> Util.indent 4
    in
        "case" <+> expr' <+> "of" <$$> caseAlts'


renderApp :: E.Expr -> E.Expr -> Doc
renderApp e1 e2 =
    render e1 <+> render e2

renderAbs :: ID.Binder -> E.Expr -> Doc
renderAbs var expr =
    let var' = render var
        expr' = render expr
    in parens $
            "λ" <> var' <> "." <+> expr'


parens x =
    "(" <> x <> ")"



-- *
-- | ## Patterns
-- *

instance Render P.CaseAlt where
    render (P.CaseAlt p e) =
        let p' = render p
            e' = render e
        in
            p' <+> "->" <$$> Util.indent 4 e'


instance Render P.Pattern where
    render (P.Var name) = render name
    render (P.Lit val)  = render val
    render P.Wildcard   = "_"
    
    render (P.Record fields) =
        let fields' = map render fields
              |> Util.punctuate ","
              |> Util.hcat
        in
            Util.parens fields'
        
    render (P.List xs) =
        map render xs
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.brackets

    render (P.Cons xs Nothing) =
        let xs' = map render xs
                |> Util.punctuate "::"
                |> Util.hcat
            rest = "::" <> "[]"
        in
            Util.parens $ xs' <> rest
    
    render (P.Cons xs (Just rest)) =
        let xs' = map render xs
                |> Util.punctuate "::"
                |> Util.hcat
            rest' = "::" <> render rest
        in
            Util.parens $ xs' <> rest'
    

    render (P.Tuple items) =
        map render items
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.parens
        
    render (P.Con name args) =
        let name' = render name
            args' = map render args
              |> Util.punctuate ","
              |> Util.punctuate Util.space
              |> Util.hcat
        in
            name' <+> args'
        



















-- *
-- | # Base
-- *



instance Render ID.Binder where
    render (ID.Binder txt ns) =
        render txt


instance Render ID.Ref where
    render (ID.Ref txt Nothing) =
        render txt
    
    render (ID.Ref txt (Just ns)) =
        let name = render txt
            ns'  = render ns
        in
            ns' <> "." <> name







-- *
-- | ## Identifiers (Ident)
-- *


instance Render ID.Low where
    render (ID.Low name Nothing) = render name
    render (ID.Low name (Just ns)) =
        render ns <> "." <> render name

instance Render ID.Big where
    render (ID.Big name Nothing) = render name
    render (ID.Big name (Just ns)) =
        render ns <> "." <> render name


instance Render ID.Namespace where
    render (ID.Namespace segs) =
        map render segs
            |> Util.punctuate "."
            |> Util.hcat






-- *
-- | ## Types
-- *

instance Render T.Type where
    render T.String = "String"
    render T.Char   = "Char"
    render T.Int    = "Int"
    render T.Float  = "Float"
    render T.Bool   = "Bool"
    
    render (T.Record fields) =
        let field (name, ty) = render name <+> ":" <+> render ty
        in map field fields
            |> Util.punctuate ","
            |> Util.vcat
            |> Util.braces


    render (T.Tuple ts) =
        map render ts
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.parens

    render (T.List ty) =
        "List" <+> render ty

    render (T.Union name args) =
        let args' = map render args
             |> Util.punctuate Util.space
             |> Util.hcat
        in
            render name <+> args'

    render (T.Var name) =
        render name

    render (T.Arr t1 t2) =
        let t1' = render t1
            t2' = render t2
        in
            Util.parens (t1' <+> "->" <+> t2')



-- *
-- | ### Type Schemes
-- *
instance Render T.Scheme where
    render (T.Forall [] ty) = renderTypeSig ty
    render (T.Forall vars ty) =
        let ty'   = renderTypeSig ty
            vars' = map render vars
              |> Util.punctuate Util.space
              |> Util.hcat
        in
            "forall" <+> vars' <> "." <+> ty'


renderTypeSig :: T.Type -> Doc
renderTypeSig (T.Arr t1 t2) =
    render t1 <+> "->" <+> renderTypeSig t2

renderTypeSig ty = render ty



-- *
-- | ## Values
-- *
instance Render V.LiteralValue where
    render (V.Int val)    = render val
    render (V.Float val)  = render val
    render (V.Bool True)  = "True"
    render (V.Bool False) = "False"
    
    render (V.Char val)   =
        "\'" <> render val <> "\'"
    render (V.String val) =
        "\"" <> render val <> "\""


























