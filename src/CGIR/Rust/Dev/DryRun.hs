{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Dev.DryRun where


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

-- + Megaparsec & Related
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

-- + Frameworks
import Framework.Text.Parser

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP


-- + Upstream IRs
import qualified SLIR.HelmSyntax.Pipeline as HelmSyntax
import qualified HLIR.HelmFlat.Pipeline   as HelmFlat

-- + RustCG AST Interface
import qualified CGIR.Rust.Data.Interface as I

-- + RustCG AST Renderer
import qualified CGIR.Rust.AST.Render.Syntax as Syntax

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

-- + RustCG Drivers
import qualified CGIR.Rust.Core.Index.Driver as Driver
-- *




{-# ANN module ("HLint: ignore" :: String) #-}





inputFilePath
    = "/Users/colbyn/SubSystems/Compiler/etc/resources/samples/test-parser/One.helm"





upstream =
    let
        filePath   = inputFilePath
        sourceCode = SIO.readFile inputFilePath
    in
        sourceCode
            |> HelmSyntax.pipeline [] filePath
            |> HelmSyntax.toHelmFlat
            |> HelmFlat.pipeline
            |> HelmFlat.toRustCG
            |> Driver.index



run = do
    result <- upstream
    case result of
        Left  err     -> putStrLn $ Text.unpack err
        Right payload -> run' payload



run' payload = do
    
    (TIO.putStrLn . Syntax.renderEnums) ens
    putStrLn "\n"
    (TIO.putStrLn . Syntax.renderFunctions) fns

    where
        fns = I.getFunctions payload
        ens = I.getEnums payload

