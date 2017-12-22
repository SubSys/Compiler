{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Core.Program.SDD.Solver.Engine (
    runTypeSolver
  , runExprSolver
  , runUnify
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten)
import Data.Data (Data, Typeable)

import Prelude (return, String, IO, show, error, (<$>), (>>))

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
import qualified Data.String   as String
import qualified Data.Either   as Either

import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint as PP


-- ~ HelmSyntax Cores

-- ~ HelmSyntax IR
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Payload

--- Local Deps
-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Base.Metadata as Meta

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Auxiliary Nodes
import qualified SLIR.HelmSyntax.AST.Auxiliary.Canonical.Ident as CID


--- Local
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Data.Env               as Env
import qualified SLIR.HelmSyntax.Core.Program.SDD.Data.Report                      as Report
import qualified SLIR.HelmSyntax.Core.Program.SDD.Inference.Utils.TypeSystem       as TS
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.Constraint           as Con
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Data.System               as Sys
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Unifier                   as Unifier
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Types                      as TySub
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Expr                       as ExSub
-- *



runUnify :: T.Type -> T.Type -> Maybe TySub.TySub
runUnify t1 t2 =
    let
        res = fst $ M.runState (M.runExceptT (Unifier.unifyType t1 t2)) Sys.initCounter
    in
        case res of
            Left _ -> Nothing
            Right x -> Just x


-- | Run the constraint solver

runTypeSolver :: [Con.Constraint] -> Either Report.TypeError TySub.TySub
runTypeSolver cs =
    let
        -- Initial Data
        initTypeSubst = (TySub.emptySubst, filterTypeConstraints cs)
    in
        fst $ M.runState (M.runExceptT $ typeSolver initTypeSubst) Sys.initCounter





runExprSolver :: [Con.Constraint] -> Either Report.TypeError ExSub.Subst
runExprSolver cs =
    let
        -- Initial Data
        initExprSubst = (ExSub.emptySubst, filterExprConstraints cs)
        tsCs = filterTypeConstraints cs
    in
        fst $ M.runState (M.runExceptT $ exprSolver tsCs initExprSubst) Sys.initCounter




-- Unification solver
typeSolver :: Sys.TypeUnifier -> Sys.Solve TySub.TySub
typeSolver (su, cs) =
    case cs of
        [] -> return su
        (Con.TypeCon t1 t2:cs0) -> do
            su1  <- Unifier.unifyType t1 t2
            typeSolver (su1 `TySub.compose` su, TySub.apply su1 cs0)
        
        -- (Con.Inline ident expr:cs0) -> do
        --     solver (su1 `TySub.compose` su, TySub.apply su1 cs0)


exprSolver :: [Con.Constraint] -> Sys.ExprUnifier -> Sys.Solve ExSub.Subst
exprSolver tsCs (subs, cs) =
    case cs of
        [] -> return subs
        (Con.Inline name ty expr:cs0) -> do
            
            
            let inlineSub = ExSub.Subst $ Map.singleton name expr
            
            exprSolver tsCs (inlineSub `ExSub.compose` subs, ExSub.apply inlineSub cs0)
        
        
        -- (Con.Overloaded name t (Map.toList -> [(scheme, expr)]):cs0) -> do
        --     let inlineSub = ExSub.Subst $ Map.singleton name expr
        --     exprSolver tsCs (inlineSub `ExSub.compose` subs, ExSub.apply inlineSub cs0)
        
        
        (Con.Overloaded name t xs:cs0) ->
            case runTypeSolver tsCs of
                Left err -> M.throwError err
                Right tySubs -> do
                    let scheme = TS.closeOver $ TySub.apply tySubs t
                    let items = Map.toList xs
                    
                    -- error $ PP.prettyShow (TySub.apply tySubs t)
                    
                    
                    case findOverloadedExpr name scheme items of
                        Just expr -> do
                            let inlineSub = ExSub.Subst $ Map.singleton name expr
                            
                            exprSolver tsCs (inlineSub `ExSub.compose` subs, ExSub.apply inlineSub cs0)
                            
                            -- error $ PP.prettyShow name.





filterTypeConstraints :: [Con.Constraint] -> [Con.Constraint]
filterTypeConstraints =
    List.filter tyPred
    where
        tyPred Con.TypeCon{} = True
        tyPred _ = False


filterExprConstraints :: [Con.Constraint] -> [Con.Constraint]
filterExprConstraints =
    List.filter (not . tyPred)
    where
        tyPred Con.TypeCon{} = True
        tyPred _ = False



noMeta :: (Data a, Typeable a) => a -> a
noMeta =
    Uni.transformBi f
    where
        f :: Maybe Meta.Meta -> Maybe Meta.Meta
        f _ = Nothing




findOverloadedExpr :: CID.Ident -> T.Scheme -> [(T.Scheme, E.Expr)] -> Maybe E.Expr
findOverloadedExpr name scheme xs =
    case List.find pred xs of
        -- Nothing ->  Nothing
        Nothing -> 
            let
                x = String.unlines $ map (PP.prettyShow . serial . fst) xs
                y = PP.prettyShow  $ serial scheme
            in
                error $
                    String.unlines
                        [ x
                        , y
                        , PP.prettyShow name
                        ]
        
        
        Just (_, e) -> Just e

    where
        serial :: T.Scheme -> T.Scheme
        serial x =
            TS.normalize $ noMeta x
        
        pred :: (T.Scheme, E.Expr) -> Bool
        pred (x, _) =
            serial x == serial scheme










