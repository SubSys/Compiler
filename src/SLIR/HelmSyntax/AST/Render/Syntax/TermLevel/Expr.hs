{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SLIR.HelmSyntax.AST.Render.Syntax.TermLevel.Expr (
    renderExpr
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
-- import Data.Monoid ((<>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude as Pre


import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable as F

-- + Frameworks
import Framework.Text.Renderer
import qualified Framework.Text.Renderer.Utils as Util

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Base.Etc           as Etc
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Base.Ident         as ID
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Base.Types         as T
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Base.Values        as V
import qualified SLIR.HelmSyntax.AST.Render.Syntax.TermLevel.Patterns as P
-- *



{-# ANN module ("HLint: ignore" :: String) #-}






renderExpr :: (Decl.Function -> Doc) -> E.Expr -> Doc
renderExpr f (E.Var id' meta) = ID.renderIdent id'
renderExpr f (E.Lit val meta) = V.renderValue val


renderExpr f (E.Tuple items meta) =
    map (renderExpr f) items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens


renderExpr f (E.List xs meta) =
    map (renderExpr f) xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets


renderExpr f (E.Constr name meta) =
    ID.renderIdent name


renderExpr f (E.InfixApp sym e1 e2 _) =
    let sym' = ID.renderIdent sym
        e1'  = renderExpr f e1
        e2'  = renderExpr f e2
    in 
        e1' <+> sym' <+> e2'



renderExpr f (E.If intros outro meta) =
    let ifBranches = map intro intros
            |> Util.vcat

        elseBranch = "else" <$$> Util.indent 4 (renderExpr f outro)
    in 
        ifBranches <$$> elseBranch
    where
        intro (con, expr) =
            let con' = renderExpr f con
                expr' = renderExpr f expr
            in
                "if" <+> con' <+> "then" <$$> Util.indent 4 expr'


renderExpr f (E.Let fns expr meta) =
    let fns' = map f fns
          |> Util.punctuate Util.linebreak
          |> Util.vcat
        expr' = renderExpr f expr
    in
        "let" <> Util.indent 1 fns' <$$> "in" <$$> Util.indent 4 expr'


renderExpr f (E.Case expr caseAlts meta) =
    let expr'     = renderExpr f expr
        caseAlts' = map renderAlt caseAlts
            |> Util.punctuate Util.linebreak
            |> Util.hcat
            |> Util.indent 4
    in
        "case" <+> expr' <+> "of" <$$> caseAlts'
    
    where
        renderAlt = P.renderCaseAlt (renderExpr f)


renderExpr f (E.Parens expr meta) =
    Util.parens $ renderExpr f expr

renderExpr f (E.App e1 e2 meta) =
    Util.parens
        $ renderExpr f e1 <+> renderExpr f e2

renderExpr f (E.Abs var expr _) =
    let var' = Etc.renderBinder var
        expr' = renderExpr f expr
    in
        Util.parens
            $ "λ" <> var' <> "." <+> expr'


renderExpr f (E.FunCall name args Nothing meta) =
    let
        name' = ID.renderIdent name
        args' = map (renderExpr f) args
            |> Util.punctuate ","
            |> Util.hcat
            |> callParens
    in
        name' <> args'


renderExpr f (E.ConCall name args Nothing meta) =
    let
        name' = ID.renderIdent name
        args' = map (renderExpr f) args
            |> Util.punctuate ","
            |> Util.hcat
            |> callParens
    in
        name' <> args'




renderExpr f (E.FunCall name args (Just ty) meta) =
    let
        name' = ID.renderIdent name
        args' = map (renderExpr f) args
            |> Util.punctuate ","
            |> Util.hcat
            |> callParens
        ty' = T.renderType ty
    in
        name' <> args' <+> ":" <+> ty'


renderExpr f (E.ConCall name args (Just ty) meta) =
    let
        name' = ID.renderIdent name
        args' = map (renderExpr f) args
            |> Util.punctuate ","
            |> Util.hcat
            |> callParens
        ty' = T.renderType ty
    in
        name' <> args' <+> ":" <+> ty'





-- | Internal Helpers

callParens x = "｟" <+> x <+> "｠"










