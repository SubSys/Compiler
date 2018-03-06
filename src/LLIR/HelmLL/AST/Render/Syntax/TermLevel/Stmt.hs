{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module LLIR.HelmLL.AST.Render.Syntax.TermLevel.Stmt (
    renderStmt
  , renderBlock
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




-- + HelmLL Module Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as T
import qualified LLIR.HelmLL.AST.Data.Base.Literals   as V

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as Decl

-- + Local
import qualified LLIR.HelmLL.AST.Render.Syntax.Base.Etc           as Etc
import qualified LLIR.HelmLL.AST.Render.Syntax.Base.Ident         as ID
import qualified LLIR.HelmLL.AST.Render.Syntax.Base.Types         as T
import qualified LLIR.HelmLL.AST.Render.Syntax.Base.Literals      as Lit
import qualified LLIR.HelmLL.AST.Render.Syntax.TermLevel.Patterns as P
-- *




renderBlock :: S.Block -> Doc
renderBlock (S.Block stmts) =
    let stmts' = map renderStmt stmts
            |> Util.punctuate ";"
            |> Util.vcat
    in
        "{" <$$> Util.indent 4 stmts' <$$> "}"




renderStmt :: S.Stmt -> Doc
renderStmt (S.Lit val) = Lit.renderLiteral val


renderStmt (S.Tuple items) =
    map renderStmt items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens


renderStmt (S.List xs) =
    map renderStmt xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets


renderStmt (S.Case expr caseAlts) =
    let expr'     = renderStmt expr
        caseAlts' = map (P.renderCaseAlt renderBlock) caseAlts
            |> Util.punctuate Util.linebreak
            |> Util.hcat
            |> Util.indent 4
    in
        "case" <+> expr' <+> "of" <$$> caseAlts'


renderStmt (S.Ref name) =
    "&" <> ID.renderIdent name


renderStmt (S.FunCall name []) =
    let
        name' = ID.renderIdent name
    in
        name' <> emptyCallParens


renderStmt (S.ConCall name []) =
    let
        name' = ID.renderIdent name
    in
        name' <> emptyCallParens




renderStmt (S.FunCall name args) =
    let
        name' = ID.renderIdent name
        args' = map renderStmt args
            |> Util.punctuate ","
            |> Util.hcat
            |> callParens
    in
        name' <> args'


renderStmt (S.ConCall name args) =
    let
        name' = ID.renderIdent name
        args' = map renderStmt args
            |> Util.punctuate ","
            |> Util.hcat
            |> callParens
    in
        name' <> args'







-- | Internal Helpers

callParens x = "｟" <+> x <+> "｠"
emptyCallParens = "｟｠"










