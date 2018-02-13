{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module HLIR.HelmFlat.AST.Render.Syntax.TopLevel.Unions (
    renderUnion
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
import qualified HLIR.HelmFlat.Module.Data.Interface as I

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Etc           as Etc
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Ident         as ID
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Types         as T
import qualified HLIR.HelmFlat.AST.Render.Syntax.Base.Values        as V
-- *



{-# ANN module ("HLint: ignore" :: String) #-}






renderUnion :: Decl.Union -> Doc
renderUnion (Decl.Union name vars cons meta) =
    let name' = ID.renderIdent name
        cons' = renderConstructors cons
        vars' = map ID.renderIdent vars
            |> Util.punctuate Util.space
            |> Util.hcat
    in
        "type" <+> name' <+> vars' <$$> cons' <$$> Util.softline


renderConstructor :: Decl.Constructor -> Doc
renderConstructor (Decl.Constructor name args meta) =
    let name' = ID.renderIdent name
        args' = map T.renderType args
          |> Util.punctuate Util.space
          |> Util.hcat
    in
        name' <+> args'




-- *
-- | Internal Helpers
-- *

renderConstructors :: [Decl.Constructor] -> Doc
renderConstructors [x] = Util.indent 4 $ "=" <+> renderConstructor x
renderConstructors (x:xs) =
    let rest = map constr xs
          |> Util.vcat
    in
        Util.indent 4 $ "=" <+> renderConstructor x <$$> rest

    where
        constr con =
            "|" <+> renderConstructor con







