{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.GLSL.Dev.DryRun where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
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

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP




-- + Upstream IRs
import qualified SLIR.HelmSyntax.Pipeline as HelmSyntax
import qualified HLIR.HelmFlat.Pipeline   as HelmFlat
import qualified LLIR.HelmLL.Pipeline     as HelmLL

-- + SPMD Syntax Renderer
import qualified CGIR.GLSL.AST.Render.Syntax as Syntax

-- + GLSL AST Interface
import qualified CGIR.GLSL.Data.Interface as I

-- + GLSL AST
-- ++ Base
import qualified CGIR.GLSL.AST.Data.Base.Ident                 as ID
import qualified CGIR.GLSL.AST.Data.Base.Literals              as Lit
import qualified CGIR.GLSL.AST.Data.Base.Types                 as T
import qualified CGIR.GLSL.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.GLSL.AST.Data.TermLevel.Stmt            as S
-- ++ Decl/Top Level
import qualified CGIR.GLSL.AST.Data.TopLevel.Functions         as Decl
import qualified CGIR.GLSL.AST.Data.TopLevel.Globals           as Decl

-- + SPMD Drivers
import qualified CGIR.GLSL.Core.Index.Driver as Driver
-- *




{-# ANN module ("HLint: ignore" :: String) #-}





inputFilePath
    = "/Users/colbyn/SubSystems/Compiler/etc/local-dev-resources/samples/Shader.helm"





upstream =
    let
        filePath   = inputFilePath
        sourceCode = SIO.readFile inputFilePath
    in
        sourceCode
            |> HelmSyntax.pipeline [] filePath
            |> HelmSyntax.toHelmFlat
            |> HelmFlat.pipeline
            |> HelmFlat.toHelmLL
            |> HelmLL.pipeline
            |> HelmLL.toGLSL



run = do
    result <- upstream
    case result of
        Left  err     -> putStrLn $ Text.unpack err
        Right payload -> run' payload



run' payload = do
    
    (TIO.putStrLn . Syntax.renderFunctions) fns

    where
        fns = I.getFunctions payload

