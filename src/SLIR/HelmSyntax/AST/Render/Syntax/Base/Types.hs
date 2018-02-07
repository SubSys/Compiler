{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SLIR.HelmSyntax.AST.Render.Syntax.Base.Types where


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
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Base.Ident as ID
-- *



{-# ANN module ("HLint: ignore" :: String) #-}






-- *
-- | # Types
-- *

renderType :: T.Type -> Doc

renderType (T.String meta) = "String"
renderType (T.Char   meta) = "Char"
renderType (T.Int    meta) = "Int"
renderType (T.Float  meta) = "Float"
renderType (T.Bool   meta) = "Bool"


renderType (T.Tuple ts meta) =
    map renderType ts
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens

renderType (T.List ty meta) =
    "List" <+> renderType ty

renderType (T.Union name args meta) =
    let args' = map renderType args
         |> Util.punctuate Util.space
         |> Util.hcat
    in
        ID.renderIdent name <+> args'

renderType (T.Var name meta) =
    ID.renderIdent name

renderType (T.Arr t1 t2 meta) =
    let t1' = renderType t1
        t2' = renderType t2
    in
        Util.parens (t1' <+> "->" <+> t2')

renderType (T.Parens ty meta) =
    let ty' = renderType ty
    in
        Util.parens ty'


renderType (T.Superposed con ts) =
    let
        con' = map ID.renderIdent con
            |> Util.punctuate Util.space
            |> Util.hcat
        ts' = map renderType ts
            |> Util.punctuate "/"
            |> Util.hcat
    in
        Util.angles
            $ con' <+> ":" <+> ts'



-- *
-- | # Type Schemes
-- *
renderScheme :: T.Scheme -> Doc
renderScheme (T.Forall [] ty) = renderToplevelType ty
renderScheme (T.Forall vars ty) =
    let ty'   = renderToplevelType ty
        vars' = map ID.renderIdent vars
          |> Util.punctuate Util.space
          |> Util.hcat
    in
        "forall" <+> vars' <> "." <+> ty'


renderToplevelType :: T.Type -> Doc
renderToplevelType (T.Arr t1 t2 meta) =
    renderType t1 <+> "->" <+> renderToplevelType t2

renderToplevelType x = renderType x




