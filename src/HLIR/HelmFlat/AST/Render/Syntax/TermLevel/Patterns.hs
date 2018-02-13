{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HLIR.HelmFlat.AST.Render.Syntax.TermLevel.Patterns (
    renderCaseAlt
  , renderPattern
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
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Etc    as Etc
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Ident  as ID
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Types  as T
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Values as V
-- *





renderCaseAlt :: (E.Expr -> Doc) -> P.CaseAlt -> Doc
renderCaseAlt f (P.CaseAlt p e) =
    let p' = renderPattern p
        e' = f e
    in
        p' <+> "->" <$$> Util.indent 4 e'



renderPattern :: P.Pattern -> Doc
renderPattern P.Wildcard = "_"
renderPattern (P.Var name) = Etc.renderBinder name
renderPattern (P.Lit val)  = V.renderValue val

renderPattern (P.List xs) =
    map renderPattern xs
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.brackets

renderPattern (P.ListCons xs Nothing) =
    let xs' = map renderPattern xs
            |> Util.punctuate "::"
            |> Util.hcat
        rest = "::" <> "[]"
    in
        Util.parens $ xs' <> rest

renderPattern (P.ListCons xs (Just rest)) =
    let xs' = map renderPattern xs
            |> Util.punctuate "::"
            |> Util.hcat
        rest' = "::" <> renderPattern rest
    in
        Util.parens $ xs' <> rest'


renderPattern (P.Tuple items) =
    map renderPattern items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens

renderPattern (P.Constr name args) =
    let name' = ID.renderIdent name
        args' = map renderPattern args
          |> Util.punctuate ","
          |> Util.punctuate Util.space
          |> Util.hcat
    in
        name' <+> args'






