{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Dev.Labs.DryRun where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)

import Prelude (mapM_, IO, String, return)

import qualified Data.Text as Text


--- Dev
import qualified Dev.HelmSamples.CPU as CPUSamples

import qualified Text.Show.Prettyprint as PP

--- Local
import qualified CGIR.Rust.Core as RustCore


-- ~ RedRust Payload
import qualified CGIR.Rust.Data.Payload.Input as IR

-- ~ RedRust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Etc    as Etc
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Types  as T
-- ~~ BlockLevel
import qualified CGIR.Rust.AST.BlockLevel.Patterns as P
import qualified CGIR.Rust.AST.BlockLevel.Stmts    as S
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.CallSites  as CS
import qualified CGIR.Rust.AST.TopLevel.TypeSystem as TS
-- ~ RedRust AST Utils
import qualified CGIR.Rust.AST.BlockLevel.Stmt.Utils as SU

-- ~ Render Utils
import qualified CGIR.Rust.Render.Utils as Util
-- *



kernel =
    do
        result <- RustCore.upstream CPUSamples.sampleOne
        
        case result of
            Left err ->
                return $ Left err
                
            Right payload ->
                return (Right $ RustCore.pipeline payload)


run =
    do
        result <- kernel
        
        case result of
            Left err ->
                putStrLn err
            Right payload ->
                do
                    -- mapM_ PP.prettyPrint program
                    -- putStrLn "\n"
                    -- mapM_ PP.prettyPrint enums
                    
                    putStrLn $ Text.unpack $ Util.renderFunctions program
                    putStrLn "\n"
                    putStrLn $ Text.unpack $ Util.renderEnums enums
                
                where
                    program = IR.getProgram payload
                    enums = IR.getEnums payload





