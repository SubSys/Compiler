{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Render.Instances where


-- *
import Core
import Core.Control.Flow

import qualified Data.Maybe as Maybe
import qualified Data.List as List

--- Framework(s)
import Framework.Render
import qualified Framework.Render.Utils as Util

--- Local
-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Payload as Payload

-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Base      as Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Exporting as Export
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Importing as Import
-- *








-- *
-- | # TopLevel
-- *














-- *
-- | ## Fixities
-- *



instance Render Decl.Infix where
    render (Decl.InfixL sym opPrecedence meta) =
        "infixl" <+> render opPrecedence <+> render sym
    render (Decl.InfixR sym opPrecedence meta) =
        "infixr" <+> render opPrecedence <+> render sym
    render (Decl.InfixN sym opPrecedence meta) =
        "infix" <+> render opPrecedence <+> render sym

instance Render Decl.OpPrecedence where
    render (Decl.OpPrecedence i meta) = render i












-- *
-- | ## Functions
-- *

instance Render Decl.Function where
    render (Decl.FnDecl id' args expr sig meta) =
        renderFunction (render id') args expr sig

    render (Decl.OpDecl opID args expr sig meta) =
        renderFunction (Util.parens $ render opID) args expr sig



renderFunction :: Doc -> [ID.Low] -> E.Expr -> Maybe Etc.Signature -> Doc
renderFunction name args expr (Just sig) =
    let args' = map render args
          |> Util.punctuate Util.space
          |> Util.hcat
        expr' = render expr
        sig' = render sig
        
        typeDecl = name <+> ":" <+> sig'
        exprDecl = name <+> args' <+> "=" <$$> Util.indent 4 expr'
    in
        typeDecl <$$> exprDecl <$$> Util.softline

renderFunction name args expr Nothing =
    let args' = map render args
          |> Util.punctuate Util.space
          |> Util.hcat
        expr' = render expr
    in
        name <+> args' <+> "=" <$$> Util.indent 4 expr' <$$> Util.softline





-- *
-- | ## Unions
-- *
instance Render Decl.Union where
    render (Decl.Union name vars cons meta) =
        let name' = render name
            cons' = renderConstructors cons
            vars' = map render vars
                |> Util.punctuate Util.space
                |> Util.hcat
        in
            "type" <+> name' <+> vars' <$$> cons' <$$> Util.softline


instance Render Decl.Constructor where
    render (Decl.Constructor name args meta) =
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
    render (E.Var id' meta) = render id'
    render (E.Lit val meta) = render val


    render (E.Tuple items meta) =
        map render items
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.parens


    render (E.List xs meta) =
        map render xs
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.brackets


    render (E.Con name meta) =
        render name


    render (E.BinOp sym e1 e2 _) =
        renderBinOp sym e1 e2


    render (E.If intros outro meta) =
        renderIf intros outro


    render (E.Let fns expr meta) =
        renderLet fns expr


    render (E.Case expr caseAlts meta) =
        renderCase expr caseAlts


    -- render (E.RecordUpdate low fields meta) =
    -- render (E.RecordAccess low optExpr meta) =


    render (E.Parens expr meta) =
        Util.parens $ render expr

    render (E.App e1 e2 _) =
        Util.parens (renderApp e1 e2)


    render (E.Abs arg expr _) =
        renderAbs arg expr
    
    render (E.Record fields meta) =
        map field fields
            |> Util.punctuate ","
            |> Util.punctuate Util.softline
            |> Util.vcat
            |> Util.braces
        where
            field :: (ID.Low, E.Expr) -> Doc
            field (name, val) = render name <+> ":" <+> render val



renderBinOp :: ID.Sym -> E.Expr -> E.Expr -> Doc
renderBinOp sym e1 e2 =
    let sym' = render sym
        e1' = render e1
        e2' = render e2
    in 
        e1' <+> sym' <+> e2'


renderIf :: [(E.Expr, E.Expr)] -> E.Expr -> Doc
renderIf intros elseExpr =
    let ifBranches = map intro intros
          |> Util.vcat
        elseBranch = "else" <$$> Util.indent 4 (render elseExpr)
    in 
        ifBranches <$$> elseBranch
    where
        intro (con, expr) =
            let con' = render con
                expr' = render expr
            in
                "if" <+> con' <+> "then" <$$> Util.indent 4 expr'

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

renderAbs :: ID.Low -> E.Expr -> Doc
renderAbs var expr =
    let var' = render var
        expr' = render expr
    in Util.parens $
            "λ" <> var' <> "." <+> expr'





-- *
-- | ## Patterns
-- *

instance Render P.CaseAlt where
    render (P.CaseAlt p e meta) =
        let p' = render p
            e' = render e
        in
            p' <+> "->" <$$> Util.indent 4 e'



instance Render P.Pattern where
    render (P.Var name meta) = render name
    render (P.Lit val meta)  = render val
    render (P.Wildcard meta) = "_"
    
    render (P.Record fields meta) =
        let fields' = map render fields
              |> Util.punctuate ","
              |> Util.hcat
        in
            Util.parens fields'
        
    render (P.List xs meta) =
        map render xs
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.brackets

    render (P.Cons xs Nothing meta) =
        let xs' = map render xs
                |> Util.punctuate "::"
                |> Util.hcat
            rest = "::" <> "[]"
        in
            Util.parens $ xs' <> rest
    
    render (P.Cons xs (Just rest) meta) =
        let xs' = map render xs
                |> Util.punctuate "::"
                |> Util.hcat
            rest' = "::" <> render rest
        in
            Util.parens $ xs' <> rest'
    

    render (P.Tuple items meta) =
        map render items
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.parens
        
    render (P.Con name args meta) =
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















-- *
-- | ## Identifiers (Ident)
-- *


instance Render ID.Low where
    render (ID.Low name Nothing meta) = render name
    render (ID.Low name (Just ns) meta) =
        render ns <> "." <> render name

instance Render ID.Big where
    render (ID.Big name Nothing meta) = render name
    render (ID.Big name (Just ns) meta) =
        render ns <> "." <> render name


instance Render ID.Sym where
    render (ID.Sym name Nothing meta) = render name
    render (ID.Sym name (Just ns) meta) =
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
    render (T.String meta) = "String"
    render (T.Char meta)   = "Char"
    render (T.Int meta)    = "Int"
    render (T.Float meta)  = "Float"
    render (T.Bool meta)   = "Bool"
    
    render (T.Record fields meta) =
        let field (name, ty) = render name <+> ":" <+> render ty
        in map field fields
            |> Util.punctuate ","
            |> Util.vcat
            |> Util.braces


    render (T.Tuple ts meta) =
        map render ts
            |> Util.punctuate ","
            |> Util.hcat
            |> Util.parens

    render (T.List ty meta) =
        "List" <+> render ty

    render (T.Union name args meta) =
        let args' = map render args
             |> Util.punctuate Util.space
             |> Util.hcat
        in
            render name <+> args'

    render (T.Var name meta) =
        render name

    render (T.Arr t1 t2 meta) =
        let t1' = render t1
            t2' = render t2
        in
            Util.parens (t1' <+> "->" <+> t2')

    render (T.Parens ty meta) =
        let ty' = render ty
        in
            Util.parens ty'




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





-- *
-- | ## Values
-- *
instance Render V.LiteralValue where
    render (V.Int val meta)    = render val
    render (V.Float val meta)  = render val
    render (V.Bool True meta)  = "True"
    render (V.Bool False meta) = "False"
    
    render (V.Char val meta)   =
        "\'" <> render val <> "\'"
    render (V.String val meta) =
        "\"" <> render val <> "\""















-- *
-- | Etc.
-- *


instance Render Etc.Signature where
    render (Etc.Validated scheme meta) = render scheme
    render (Etc.Unresolved ty meta)   = renderTypeSig ty


renderTypeSig :: T.Type -> Doc
renderTypeSig (T.Arr t1 t2 meta) =
    render t1 <+> "->" <+> renderTypeSig t2

renderTypeSig ty = render ty





-- *
-- | # Header
-- *

instance Render Import.ModuleImporting where
    render (Import.ModuleImporting importDeclarations) =
        map render importDeclarations
            |> Util.punctuate Util.linebreak
            |> Util.vcat





-- *
-- | Export Declaration
-- * 
instance Render Export.ModuleExporting where
    render (Export.Explicit entries) =
        "exposing" <+> renderEntries entries
        
    render Export.Everything =
        "exposing" <+> "(..)"



-- *
-- | Import Declarations
-- *
instance Render Import.ImportDecl where
    render (Import.Qualified ns Nothing) =
        "import" <+> render ns
    
    render (Import.Qualified ns (Just asAlias)) =
        "import" <+> render ns <+> "as" <+> render asAlias


    render (Import.Explicit ns entries) =
        "import" <+> render ns <+> renderEntries entries

    render (Import.Everything ns) =
        "import" <+> render ns <+> "(..)"



renderEntries :: [Header.Entry] -> Doc
renderEntries entries =
    map render entries
          |> Util.punctuate ","
          |> Util.hcat
          |> Util.parens
    




-- | Base (Module Header) Types
--
instance Render Header.Entry where
    render (Header.ValueEntry txt) = render txt
    render (Header.UnionEntry txt cons) =
        render txt <+> render cons

instance Render Header.UnionExposing where
    render  Header.UnionEverything = "(..)"
    render (Header.UnionExplicit []) = "()"
    render (Header.UnionExplicit cons) =
        map render cons
             |> Util.punctuate ","
             |> Util.hcat
             |> Util.parens











-- *
-- | Metadata, 'Source Code Location' & Related
-- *



















