{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.Rust.AST.Render.Syntax.BlockLevel.Patterns (
    renderArm
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
import qualified CGIR.Rust.AST.Render.Syntax.Base.Ident    as ID
import qualified CGIR.Rust.AST.Render.Syntax.Base.Literals as Lit
-- *



{-# ANN module ("HLint: ignore" :: String) #-}



-- | Render Arm branch (or Case Alt.)
--

renderArm :: (S.Block -> Doc) -> P.Arm -> Doc
renderArm f (P.Arm patrn stmt) =
    let patrn' = renderPattern patrn
        stmt'  = f stmt
    in
        patrn' <+> "=>" <$$> Util.indent 4 stmt'




-- | Render Patterns
--
renderPattern :: P.Pattern -> Doc
renderPattern P.Wildcard    = "_"
renderPattern (P.Var ident) = ID.renderIdent ident
renderPattern (P.Lit val)   = Lit.renderLiteral val

renderPattern (P.List xs) =
    map renderPattern xs
        |> Util.punctuate ","
        |> Util.punctuate Util.space
        |> Util.hcat
        |> Util.brackets

renderPattern (P.ListCons xs rest) =
    let xs' = map renderPattern xs
            |> Util.punctuate "::"
            |> Util.hcat
        rest' = Maybe.maybe ("::" <> "[]") (\end -> "::" <> renderPattern end) rest
    in
        Util.parens $ xs' <> rest'

renderPattern (P.Tuple items) =
    map renderPattern items
        |> Util.punctuate ","
        |> Util.hcat
        |> Util.parens

renderPattern (P.Variant path []) =
    ID.renderPath path

renderPattern (P.Variant path args) =
    let path' = ID.renderPath path
        args' = map renderPattern args
            |> Util.punctuate ","
            |> Util.punctuate Util.space
            |> Util.hcat
            |> Util.parens
    in
        path' <+> args'








