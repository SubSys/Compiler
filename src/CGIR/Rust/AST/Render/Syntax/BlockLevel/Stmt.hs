{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.Rust.AST.Render.Syntax.BlockLevel.Stmt (
    renderBlock
  , renderStmt
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Prelude
    (return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core


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
import qualified Data.Data                    as Data

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Frameworks
import Framework.Text.Renderer
import qualified Framework.Text.Renderer.Utils as Util

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + RustCG AST Interface
import qualified CGIR.Rust.Data.Interface as I

-- + RustCG AST
-- ++ Base
import qualified CGIR.Rust.AST.Data.Base.Ident                 as ID
import qualified CGIR.Rust.AST.Data.Base.Literals              as Lit
import qualified CGIR.Rust.AST.Data.Base.Types                 as T
import qualified CGIR.Rust.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.Rust.AST.Data.TermLevel.Stmt            as S
import qualified CGIR.Rust.AST.Data.TermLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified CGIR.Rust.AST.Data.TopLevel.Enums.Variants   as Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Enums            as Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Functions        as Decl

-- + Local
import qualified CGIR.Rust.AST.Render.Syntax.Base.Ident          as ID
import qualified CGIR.Rust.AST.Render.Syntax.Base.Literals       as Lit
import qualified CGIR.Rust.AST.Render.Syntax.BlockLevel.Patterns as P
-- *



{-# ANN module ("HLint: ignore" :: String) #-}



renderBlock :: S.Block -> Doc
renderBlock (S.Block stmts) =
    let stmts' = map renderStmt stmts
            |> Util.punctuate ";"
            |> Util.vcat
    in
        "{" <$$> Util.indent 4 stmts' <$$> "}"


renderStmt :: S.Stmt -> Doc
renderStmt (S.Lit val) =
    Lit.renderLiteral val

renderStmt (S.Ref path) =
    ID.renderPath path

renderStmt (S.FunCall path args) =
    let path' = ID.renderPath path
        args' = map renderStmt args
            |> Util.punctuate ","
            |> Util.punctuate Util.softline
            |> Util.hcat
            |> Util.parens
    in
        path' <> args'

renderStmt (S.ConCall path args) =
    let path' = ID.renderPath path
        args' = map renderStmt args
            |> Util.punctuate ","
            |> Util.punctuate Util.softline
            |> Util.hcat
            |> Util.parens
    in
        path' <> args'

renderStmt (S.Match con arms) =
    let con' = renderStmt con
        arms' = map (P.renderArm renderBlock) arms
            |> Util.punctuate ","
            |> Util.punctuate Util.linebreak
            |> Util.hcat
            |> Util.indent 4
    in
        "match" <+> con' <+> "{" <$$> arms' <$$> "}"


renderStmt (S.List xs) =
    map renderStmt xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets

renderStmt (S.Tuple items) =
    map renderStmt items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens



-- TODO: ...
renderStmt (S.If intros elseBranch) =
    let intros' = map renderBranch intros
            |> Util.vcat

        elseBranch' = "else" <$$> Util.indent 4 (renderBlock elseBranch)
    in 
        intros' <$$> elseBranch'




-- TODO:
-- renderStmt (S.Box value) = error "TODO"



-- | Internal Helpers
--

-- |
-- TODO: ...
renderBranch :: (S.Stmt, S.Block) -> Doc
renderBranch (con, block) =
    let con' = renderStmt con
        block' = renderBlock block
    in
        "if" <+> con' <+> "then" <$$> Util.indent 4 block'




