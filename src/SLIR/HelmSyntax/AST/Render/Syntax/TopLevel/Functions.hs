{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SLIR.HelmSyntax.AST.Render.Syntax.TopLevel.Functions where


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

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident as ID

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
import qualified SLIR.HelmSyntax.AST.Render.Syntax.TermLevel.Expr     as E
-- *



{-# ANN module ("HLint: ignore" :: String) #-}





renderFunction (Decl.Function name args expr sig meta) =
    let name' = case ID.get name of
            (ID.OnSym txt Nothing _)   -> Util.parens $ render txt
            (ID.Ident txt Nothing _)   -> render txt
            (ID.OnSym txt (Just ns) _) -> renderNS ns <> Util.parens (render txt)
            (ID.Ident txt (Just ns) _) -> renderNS ns <> render txt
    in
        renderFunction' name' args expr sig
    
    where
        renderNS ns = ID.renderNamespace ns <> "."


renderFunction' :: Doc -> [Etc.Binder] -> E.Expr -> Decl.Signature -> Doc
renderFunction' name args expr Decl.Unknown =
    let
        args' = map Etc.renderBinder args
          |> Util.punctuate Util.space
          |> Util.hcat
        expr' = E.renderExpr renderFunction expr
    in
        name <+> args' <+> "=" <$$> Util.indent 4 expr' <$$> Util.softline


renderFunction' name args expr sig =
    let
        args' = map Etc.renderBinder args
          |> Util.punctuate Util.space
          |> Util.hcat
        expr' = E.renderExpr renderFunction expr
        sig' = renderSig sig

        typeDecl = name <+> ":" <+> sig'
        exprDecl = name <+> args' <+> "=" <$$> Util.indent 4 expr'
    in
        typeDecl <$$> exprDecl <$$> Util.softline
    
    where
        renderSig sig = renderSig_ T.renderScheme (Just sig)







renderSig_ :: (T.Scheme -> Doc) -> Maybe Decl.Signature -> Doc
renderSig_ f Nothing = Util.empty
renderSig_ f (Just sig) =
    renderSig' f sig




renderSig' :: (T.Scheme -> Doc) -> Decl.Signature -> Doc

renderSig' f (Decl.Validated scheme meta) =
    f scheme

renderSig' f (Decl.Unresolved ty meta) =
    f (T.Forall [] ty)

renderSig' f (Decl.Unknown) =
    ""




