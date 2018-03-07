{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CGIR.RustCG.AST.Render.Syntax.DeclLevel.Functions (
    renderFunction
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
import qualified CGIR.RustCG.Data.Interface as I

-- + RustCG AST
-- ++ Base
import qualified CGIR.RustCG.AST.Data.Base.Ident                 as ID
import qualified CGIR.RustCG.AST.Data.Base.Literals              as Lit
import qualified CGIR.RustCG.AST.Data.Base.Types                 as T
import qualified CGIR.RustCG.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.RustCG.AST.Data.TermLevel.Stmt            as S
import qualified CGIR.RustCG.AST.Data.TermLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified CGIR.RustCG.AST.Data.TopLevel.Enums.Variants   as Decl
import qualified CGIR.RustCG.AST.Data.TopLevel.Enums            as Decl
import qualified CGIR.RustCG.AST.Data.TopLevel.Functions        as Decl

-- + Local
import qualified CGIR.RustCG.AST.Render.Syntax.Base.Ident      as ID
import qualified CGIR.RustCG.AST.Render.Syntax.Base.Literals   as Lit
import qualified CGIR.RustCG.AST.Render.Syntax.BlockLevel.Stmt as S
import qualified CGIR.RustCG.AST.Render.Syntax.Base.Types      as T
import qualified CGIR.RustCG.AST.Render.Syntax.Base.Etc        as Etc
-- *



{-# ANN module ("HLint: ignore" :: String) #-}



renderFunction :: Decl.Function -> Doc
renderFunction (Decl.Function name generics inputs output body) =
    let name'     = ID.renderIdent name
        generics' = Etc.renderGenerics generics
        inputs'   = map Etc.renderInput inputs
            |> Util.punctuate ","
            |> Util.punctuate Util.space
            |> Util.hcat
            |> Util.parens
        output'   = Etc.renderOutput output
        body'     = S.renderBlock body
    in
            "fn"
        <+> name'
        <+> generics'
        <+> inputs'
        <+> output'

        <+> body'
        <> Util.linebreak






