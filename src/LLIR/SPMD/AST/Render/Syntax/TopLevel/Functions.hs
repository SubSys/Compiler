{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module LLIR.SPMD.AST.Render.Syntax.TopLevel.Functions (
    renderFunction
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
import qualified LLIR.SPMD.AST.Render.Syntax.Base.Ident      as ID
import qualified LLIR.SPMD.AST.Render.Syntax.Base.Etc        as Etc
import qualified LLIR.SPMD.AST.Render.Syntax.Base.Literals   as Lit
import qualified LLIR.SPMD.AST.Render.Syntax.Base.Types      as T
import qualified LLIR.SPMD.AST.Render.Syntax.BlockLevel.Stmt as S
-- *




{-# ANN module ("HLint: ignore" :: String) #-}


renderFunction :: Decl.Function -> Doc
renderFunction (Decl.Function outputType name inputs (fixReturn -> body)) =
    let output'   = T.renderType outputType
        name'     = ID.renderIdent name
        inputs'   = map Etc.renderInput inputs
            |> Util.punctuate ","
            |> Util.punctuate Util.space
            |> Util.hcat
            |> Util.parens
        body'     = S.renderBlock body
    in
            output'
        <+> name'
        <+> inputs'
        <+> body'

        <> Util.linebreak



fixReturn :: S.Block -> S.Block
fixReturn (S.Block [s]) =
    S.Block [fixReturnStmt s]

fixReturn (S.Block xs) =
    let
        initStmts = List.init xs
        lastStmt = List.last xs
    in
        S.Block $ initStmts ++ [fixReturnStmt lastStmt]

fixReturnStmt :: S.Stmt -> S.Stmt
fixReturnStmt x@(isReturnStmt -> False) =
    S.Return $ Just x

fixReturnStmt x = x

isReturnStmt :: S.Stmt -> Bool
isReturnStmt S.Return{} = True
isReturnStmt _          = False


