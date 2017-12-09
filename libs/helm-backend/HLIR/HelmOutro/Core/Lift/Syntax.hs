{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core.Lift.Syntax where

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
import qualified Text.Show.Prettyprint as PP


--- Dev
import qualified Dev.Samples.Basic      as BasicSamples
import qualified Dev.Samples.Complex    as ComplexSamples
import qualified Dev.Samples.TestParser as ParserSample

import qualified HLIR.HelmOutro.Render.Utils as Display


-- ~ Upstream
import qualified SLIR.HelmSyntax.Core  as HelmSyntax
import qualified HLIR.HelmCore.Core    as HelmCore


--- Local Deps
-- ~ HelmOutro Payload
import qualified HLIR.HelmOutro.Data.Payload as Payload

-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl

-- ~ HelmOutro Drivers
import qualified HLIR.HelmOutro.Core.Indexing.Driver as Driver
import qualified HLIR.HelmOutro.Core.PreLift.Driver  as Driver
import qualified HLIR.HelmOutro.Core.TypeCheck.Driver as Driver

-- ~ Auxiliary AST Nodes
import qualified HLIR.HelmOutro.AST.Auxiliary.Canonical.Ident as CID

--- Local
import qualified HLIR.HelmOutro.Core.Lift.Utils.Scope as Scope
import qualified HLIR.HelmOutro.Core.Lift.Data.System as Sys
-- *




{-# ANN module "HLint: ignore" #-}


-- TODO:
-- * eventually, lifted functions should include an updated signature,
--   perhaps using the types of the inserted arguments, or at the very
--   least, perhaps, generic type variables…




-- *
-- | Expressions
-- *

liftExpr :: E.Expr -> Sys.Lift E.Expr
liftExpr term@E.Lit{} = return term
liftExpr term@E.Con{} = return term


liftExpr node@(E.Var ref) = do
    vs <- Sys.getLifted
    
    case Map.lookup (CID.ident ref) vs of
        Nothing -> return node
        Just as ->
            return $ E.FunCall ref (map toVar as)


liftExpr node@(E.FunCall ref params) = do
    vs <- Sys.getLifted
    -- *
    
    -- *
    params' <- M.mapM liftExpr params
    -- *
    
    -- *
    case Map.lookup (CID.ident ref) vs of
        Nothing ->
            return $ E.FunCall ref params'

        Just as ->
            return $ E.FunCall ref (params' ++ map toVar as)


liftExpr (E.ConCall con params) = do
    params' <- M.mapM liftExpr params
    
    return $ E.ConCall con params'

    
liftExpr (E.Record fields) = do
    fields' <- M.mapM processField fields
    
    return $ E.Record fields'
    
liftExpr term@(E.Tuple xs) = do
    xs' <- M.mapM liftExpr xs
    
    return $ E.Tuple xs'

liftExpr term@(E.List xs) = do
    xs' <- M.mapM liftExpr xs
    
    return $ E.List xs'

liftExpr (E.Case con alts) = do
    con'  <- liftExpr con
    alts' <- M.mapM processCaseAlt alts
    
    return $ E.Case con' alts'

liftExpr (E.App e1 e2) = do
    e1' <- liftExpr e1
    e2' <- liftExpr e2
    
    return $ E.App e1' e2'


liftExpr (E.Abs args expr) =
    error $ "TODO: Lambda Lift `E.Abs` terms..."


-- liftExpr (E.Let fns expr) = do
--     env <- M.ask
--      <- topLevels fns env
--     -- *
-- 
--     -- *
--     names <- (Fold.foldl' Map.union Map.empty) <$> M.mapM liftLetDecl fns
--     expr' <- Scope.withLocalLTEnv names (liftExpr expr)
--     -- *
-- 
--     -- *
--     return expr'



liftExpr (E.Let fns expr) = do
    env <- M.ask
    case topLevels env fns of
        Left err -> M.throwError err
        Right result -> do
            let fns1     = map snd result
                fns1Deps = map fst result
            -- *

            -- *
            (fns2, names) <- List.unzip <$> M.mapM liftLetDecl fns1
            -- *
            
            -- *
            let env' = Fold.foldl' Map.union Map.empty names
            expr' <- Scope.withLocalLTEnv env' (liftExpr expr)
            -- *
            
            -- *
            M.tell $ unwrap $ List.zip fns1Deps fns2
            -- *

            -- *
            return expr'
    
    
    where
    --     repack :: [Decl.Function]
    --            -> [(NewFunctions, Decl.Function)]
    --            -> [(NewFunctions, Decl.Function)]
    --     repack fns res =
        unwrap :: [([Decl.Function], Decl.Function)] -> [Decl.Function]
        unwrap = Fold.foldr ((++) . unwrap') []
        
        unwrap' :: ([Decl.Function], Decl.Function) -> [Decl.Function]
        unwrap' (fns, fn) =
            fns ++ [fn]





liftLetDecl :: Decl.Function -> Sys.Lift (Decl.Function, Sys.LiftedEnv)
liftLetDecl fn@(Decl.Function name args expr sig) = do
    expr' <- liftExpr expr
    -- *

    -- *
    gs <- Sys.getGlobals
    let vars = (Scope.freeVars fn) `Scope.without` gs
    as <- M.mapM toArg vars
    -- *
    
    -- *
    -- M.tell [Decl.Function name (args ++ as) expr Nothing]
    -- *
    
    -- *
    return
        (Decl.Function name (args ++ as) expr Nothing, Map.singleton (CID.ident name) vars)




-- *
-- | Internal Helpers
-- *

toArg :: CID.Ident -> Sys.Lift Etc.Arg
toArg name = do
    return $ Etc.Arg (CID.toBinder name) Nothing

-- TODO: Add types env...
-- toArg :: CID.Ident -> Sys.Lift Etc.Arg
-- toArg name = do
--     env <- Sys.getTypes
-- 
--     case Map.lookup name env of
--         Nothing ->
--             M.throwError
--                 $ Sys.UnknownType
--                 $ Text.pack
--                 $ show name
--         Just ty ->
--             return $ Etc.Arg (CID.toBinder name) (Just ty)


toVar :: CID.Ident -> E.Expr
toVar name =
    E.Var $ CID.toRef name


processField :: (a, E.Expr) -> Sys.Lift (a, E.Expr)
processField (x, e) = do
    e' <- liftExpr e
    return (x, e')

processCaseAlt :: P.CaseAlt -> Sys.Lift P.CaseAlt
processCaseAlt (P.CaseAlt p e) = do
    e' <- liftExpr e
    
    return $ P.CaseAlt p e'








-- *
-- | Core Lifter API
-- *
type NewFunctions = [Decl.Function]


topLevels :: Sys.Env -> [Decl.Function] -> Either Sys.LiftError [(NewFunctions, Decl.Function)]
topLevels env []       = Right []
topLevels env (fn1:rest) =
    case topLevel env fn1 of
        Left err -> Left err
        Right fns1 ->
            case topLevels env rest of
                Left err -> Left err
                Right rest' ->
                    Right $ fns1 : rest'


topLevel :: Sys.Env -> Decl.Function -> Either Sys.LiftError (NewFunctions, Decl.Function)
topLevel env (Decl.Function name args expr scheme) =
    case Sys.runState env (liftExpr expr) of
        Left err    -> Left err
        Right (expr', fns) ->
            Right (fns, Decl.Function name args expr' scheme)




