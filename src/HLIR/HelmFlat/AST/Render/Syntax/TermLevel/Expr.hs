{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HLIR.HelmFlat.AST.Render.Syntax.TermLevel.Expr (
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




-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Data.Interface as I

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Etc           as Etc
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Ident         as ID
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Types         as T
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Values        as V
import qualified HLIR.HelmFlat.AST.Render.Syntax.TermLevel.Patterns as P
-- *






renderExpr :: (Decl.Function -> Doc) -> E.Expr -> Doc
renderExpr f (E.Lit val) = V.renderValue val


renderExpr f (E.Tuple items) =
    map (renderExpr f) items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens


renderExpr f (E.List xs) =
    map (renderExpr f) xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets


renderExpr f (E.Case expr caseAlts) =
    let expr'     = renderExpr f expr
        caseAlts' = map renderAlt caseAlts
            |> Util.punctuate Util.linebreak
            |> Util.hcat
            |> Util.indent 4
    in
        "case" <+> expr' <+> "of" <$$> caseAlts'
    
    where
        renderAlt = P.renderCaseAlt (renderExpr f)


renderExpr f (E.Ref name) =
    "&" <> ID.renderIdent name


renderExpr f (E.FunCall name []) =
    let
        name' = ID.renderIdent name
    in
        name' <> emptyCallParens


renderExpr f (E.ConCall name []) =
    let
        name' = ID.renderIdent name
    in
        name' <> emptyCallParens




renderExpr f (E.FunCall name args) =
    let
        name' = ID.renderIdent name
        args' = map (renderExpr f) args
            |> Util.punctuate ","
            |> Util.hcat
            |> callParens
    in
        name' <> args'


renderExpr f (E.ConCall name args) =
    let
        name' = ID.renderIdent name
        args' = map (renderExpr f) args
            |> Util.punctuate ","
            |> Util.hcat
            |> callParens
    in
        name' <> args'







-- | Internal Helpers

callParens x = "｟" <+> x <+> "｠"
emptyCallParens = "｟｠"










