{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module LLIR.HelmLL.AST.Render.Syntax.Base.Types where


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
import qualified LLIR.HelmLL.AST.Render.Syntax.Base.Ident as ID
-- *



{-# ANN module ("HLint: ignore" :: String) #-}






-- *
-- | # Types
-- *

renderType :: T.Type -> Doc

renderType T.String = "String"
renderType T.Char = "Char"
renderType T.Int = "Int"
renderType T.Float = "Float"
renderType T.Bool = "Bool"


renderType (T.Tuple ts) =
    map renderType ts
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens

renderType (T.List ty) =
    "List" <+> renderType ty

renderType (T.Union name args) =
    let args' = map renderType args
         |> Util.punctuate Util.space
         |> Util.hcat
    in
        ID.renderIdent name <+> args'

renderType (T.Var name) =
    ID.renderIdent name

renderType (T.Arr t1 t2) =
    let t1' = renderType t1
        t2' = renderType t2
    in
        Util.parens (t1' <+> "->" <+> t2')





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
renderToplevelType (T.Arr t1 t2) =
    renderType t1 <+> "->" <+> renderToplevelType t2

renderToplevelType x = renderType x




