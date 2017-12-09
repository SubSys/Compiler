{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Pass.Formalize.CS.Functions (
    input
) where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)

import Prelude (mapM_, IO, String, return)

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Set  as Set


import qualified Data.Generics.Uniplate.Data as Uni

--- Framework(s)
import qualified Framework.Pipeline.Data    as Pipe
import qualified Framework.IR.Standard.Data as StdIR


--- Local


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
-- ~ Misc. Builder Utils
import qualified CGIR.Rust.Prefabs.Builder.Etc.Utils as Util

-- ~ Render Utils
import qualified CGIR.Rust.Render.Utils as Util
-- *




input :: IR.Payload -> IR.Payload
input payload =
    IR.updateProgram payload program
    where
        program =
            IR.getProgram payload
                |> processProgram




processProgram :: [CS.FunctionDecl] -> [CS.FunctionDecl]
processProgram = map processFunction


processFunction :: CS.FunctionDecl -> CS.FunctionDecl
processFunction (CS.Function name amSelf generics inputs output block) =
    let xs        = process inputs output |> map Etc.Generic
        generics' = generics ++ xs
        block' = processBlock block
    in
        CS.Function name amSelf generics' inputs output block'


processStmt :: S.Stmt -> S.Stmt
processStmt (S.Block stmts) =
    S.Block (map processStmt stmts)

processStmt (S.FnDecl fn) =
    S.FnDecl $ processFunction fn

processStmt x = x


processBlock :: Etc.Block -> Etc.Block
processBlock (Etc.Block stmts) =
    Etc.Block (map processStmt stmts)




-- *
-- | Core Logic
-- *
process :: [Etc.Input] -> Etc.Output -> [ID.Big]
process xs y =
    let xs' = map inp xs |> flatten
        y' = outp y
    in
        (xs' ++ y') |> Set.fromList |> Set.toList
    where
        inp :: Etc.Input -> [ID.Big]
        inp (Etc.Input _ ty) = getGenerics ty
        
        outp :: Etc.Output -> [ID.Big]
        outp (Etc.Output ty) = getGenerics ty



getGenerics :: T.Type -> [ID.Big]
getGenerics ty = [y | T.GenericType y <- Uni.universe ty]






