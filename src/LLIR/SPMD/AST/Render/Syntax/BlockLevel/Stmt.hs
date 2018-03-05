{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module LLIR.SPMD.AST.Render.Syntax.BlockLevel.Stmt (
    renderBlock
  , renderStmt
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
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
import qualified Data.String                  as String

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



-- + SPMD AST Interface
import qualified LLIR.SPMD.Data.Interface as I

-- + SPMD AST
-- ++ Base
import qualified LLIR.SPMD.AST.Data.Base.Ident                 as ID
import qualified LLIR.SPMD.AST.Data.Base.Literals              as Lit
import qualified LLIR.SPMD.AST.Data.Base.Types                 as T
import qualified LLIR.SPMD.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified LLIR.SPMD.AST.Data.BlockLevel.Stmt            as S
-- ++ Decl/Top Level
import qualified LLIR.SPMD.AST.Data.TopLevel.Functions         as Decl
import qualified LLIR.SPMD.AST.Data.TopLevel.Globals           as Decl

-- + Local
import qualified LLIR.SPMD.AST.Render.Syntax.Base.Ident    as ID
import qualified LLIR.SPMD.AST.Render.Syntax.Base.Etc      as Etc
import qualified LLIR.SPMD.AST.Render.Syntax.Base.Literals as Lit
import qualified LLIR.SPMD.AST.Render.Syntax.Base.Types    as T
-- *



{-# ANN module ("HLint: ignore" :: String) #-}




renderBlock :: S.Block -> Doc
renderBlock (S.Block stmts) =
    let stmts' = map renderStmt stmts
            |> map (<> ";")
            |> Util.vcat
    in
        "{" <$$> Util.indent 4 stmts' <$$> "}"


renderStmt :: S.Stmt -> Doc
renderStmt (S.Ref ident) = ID.renderIdent ident
renderStmt (S.Init ty name Nothing) =
    let ty' = T.renderType ty
        name' = ID.renderIdent name
    in
        ty' <+> name'

renderStmt (S.Init ty name (Just constant)) =
    let ty' = T.renderType ty
        name' = ID.renderIdent name
        constant' = renderStmt constant
    in
        ty' <+> name' <+> "=" <+> constant'

renderStmt (S.FunCall ident args) =
    let ident' = ID.renderIdent ident
        args' = map renderStmt args
            |> Util.punctuate ","
            |> Util.punctuate Util.softline
            |> Util.hcat
            |> Util.parens
    in
        ident' <> args'

renderStmt (S.ConCall ident args) =
    let ident' = ID.renderIdent ident
        args' = map renderStmt args
            |> Util.punctuate ","
            |> Util.punctuate Util.softline
            |> Util.hcat
            |> Util.parens
    in
        ident' <> args'


renderStmt (S.Return Nothing) = "return"
renderStmt (S.Return (Just s)) = "return" <+> renderStmt s


