{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Core.Paths.Dev (
    updateConstrPaths
) where



-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid

import qualified Data.Generics.Uniplate.Data as Uni


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified Text.Show.Prettyprint as PP

import qualified CGIR.Rust.Render.Syntax as Display

-- ~ Upstream
import qualified SLIR.HelmSyntax.Core  as HelmSyntax
import qualified HLIR.HelmCore.Core    as HelmCore
import qualified HLIR.HelmOutro.Core   as HelmOutro

import qualified LLIR.LightRoast.Core as LightRoast



--- Local Deps
-- ~ LightRoast Payload
import qualified CGIR.Rust.Data.Payload as Payload

-- ~ (GCIR) - Rust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Types  as T
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified CGIR.Rust.AST.TermLevel.Stmt        as S
import qualified CGIR.Rust.AST.TermLevel.Patterns    as P
import qualified CGIR.Rust.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.Functions as Decl
import qualified CGIR.Rust.AST.TopLevel.Unions    as Decl

-- ~ (CGIR) - Rust Drivers
import qualified CGIR.Rust.Core.Indexing.Driver as Driver

--- Local
import qualified CGIR.Rust.Core.Paths.Data.TypesEnv as TypesEnv

import qualified CGIR.Rust.Core.Paths.Utils.Unions  as Util
import qualified CGIR.Rust.Core.Paths.Utils.Types   as Util
-- *


{-# ANN module "HLint: ignore" #-}

upstream =
    ParserSample.sampleOne
        |> HelmSyntax.frontend
        |> HelmSyntax.toHelmCore
        |> HelmCore.pipeline
        |> HelmCore.toHelmOutro
        |> HelmOutro.pipeline
        |> HelmOutro.toLightRoast
        |> LightRoast.toRust
        |> Driver.globalIndexer
        |> updateConstrPaths



run = do
    input <- upstream
    
    case input of
        Left err      -> putStrLn (Text.unpack err)
        Right payload -> run' payload



run' payload = do
    
    putStrLn $ Text.unpack $ Display.renderUnions unions
    putStrLn $ Text.unpack $ Display.renderFunctions functions
    
    
    where
        unions = Payload.getUnions payload
        functions  = Payload.getFunctions payload



-- *
-- | Paths
-- *


updateConstrPaths :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
updateConstrPaths upstream = do
    result <- upstream
    case result of
        Left err -> return $ Left err
        Right payload ->
            return $ Right (updateConstrPaths' payload)


updateConstrPaths' :: Payload.Module -> Payload.Module
updateConstrPaths' payload =
    Payload.updateFunctions payload (updateFunctions env unions functions)
    where
        env = Util.initTypesEnv functions
        
        unions = Payload.getUnions payload
        functions  = Payload.getFunctions payload


updateFunctions :: TypesEnv.TypesEnv -> [Decl.Union] -> [Decl.Function] -> [Decl.Function]
updateFunctions env uns fns =
    fns |> Uni.transformBi (updateStmt  env uns)
        |> Uni.transformBi (updatePatrn env uns)





updateStmt :: TypesEnv.TypesEnv -> [Decl.Union] -> S.Stmt -> S.Stmt
updateStmt env uns (S.ConCall name args) =
    case Util.lookupUnionName name uns of
        Nothing -> error $ "Cannot find union for constr: " ++ show name
        
        Just unionName ->
            let newName = serialize name unionName
            in
                S.ConCall newName args

updateStmt env uns (S.Ref name)
    | isFnType =
        S.Ref' S.AndRef name
    | otherwise =
        S.Ref name

    where
        isFnType = Util.lookupRef name env
    

updateStmt _ _ x = x


updatePatrn :: TypesEnv.TypesEnv -> [Decl.Union] -> P.Pattern -> P.Pattern
updatePatrn _ uns (P.Con name args) =
    case Util.lookupUnionName name uns of
        Nothing -> error $ "Cannot find union for constr: " ++ show name
        
        Just unionName ->
            let newName = serialize name unionName
            in
                P.Con newName args


updatePatrn _ _ x = x



-- *
-- | Internal Helpers
-- *

-- TODO: Append Parent NS?
serialize :: ID.Big -> ID.Big -> ID.Big
serialize (ID.Big conName ns1) (ID.Big unionName ns2) =
    ID.Big conName
        $ Just
        $ ID.Namespace [unionName]



