{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module SLIR.HelmSyntax.Core.Program.SDD.Resolve.Update.AltAbs (
    updateAltAbs
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
import qualified SLIR.HelmSyntax.Core.Program.SDD.Solver.Engine                    as Solver
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Types                      as TySub
import qualified SLIR.HelmSyntax.Core.Program.SDD.Subst.Expr                       as ExSub
-- *





updateAltAbs :: Decl.Function
             -> T.Type
             -> Env.ExprEnv
             -> TySub.TySub
             -> ExSub.Subst
             -> Decl.Function
updateAltAbs (Decl.FnDecl name args expr sig meta) ty exprEnv tySubs exSubs =
    let
        expr' = updateAltAbs' expr ty exprEnv tySubs exSubs
    in
        Decl.FnDecl name args expr' sig meta

updateAltAbs (Decl.OpDecl name args expr sig meta) ty exprEnv tySubs exSubs =
    let
        expr' = updateAltAbs' expr ty exprEnv tySubs exSubs
    in
        Decl.OpDecl name args expr' sig meta


updateAltAbs' :: E.Expr
             -> T.Type
             -> Env.ExprEnv
             -> TySub.TySub
             -> ExSub.Subst
             -> E.Expr

updateAltAbs' expr ty exprEnv tySubs exSubs =
    Uni.transform f expr

    where
        f :: E.Expr -> E.Expr
        -- f (E.AltAbs args body (Just scheme@(T.Forall as t2))) =
        --     case Solver.runUnify (TySub.apply tySubs ty) t2 of
        --         Nothing ->
        --             let
        --                 resScheme = TS.closeOver $ TySub.apply tySubs ty
        --             in
        --                 E.AltAbs args body (Just resScheme)
        -- 
        --         Just sub2 ->
        --             let
        --                 resScheme = TS.closeOver $ TySub.apply (sub2 `TySub.compose` tySubs) ty
        --             in
        --                 E.AltAbs args body (Just resScheme)
        
        
        -- f (E.AltAbs args body (Just scheme)) =
        --     case Solver.runUnify (TySub.apply tySubs ty) (castScheme scheme) of
        --         -- Nothing ->
        --         --     let
        --         --         resScheme = TS.closeOver $ TySub.apply tySubs ty
        --         --     in
        --         --         E.AltAbs args body (Just resScheme)
        -- 
        --         Nothing ->
        --             case Solver.runUnify ty (castScheme scheme) of
        --                 Nothing -> E.AltAbs args body (Just scheme)
        --                 Just sub2 -> 
        --                     let
        --                         resScheme = TS.closeOver $ TySub.apply (sub2 `TySub.compose` tySubs) ty
        --                     in
        --                         E.AltAbs args body (Just resScheme)
        -- 
        -- 
        --         Just sub2 ->
        --             let
        --                 resScheme = TS.closeOver $ TySub.apply (sub2 `TySub.compose` tySubs) ty
        --             in
        --                 E.AltAbs args body (Just resScheme)
            
        f (E.AltAbs args body (Just scheme@(isSuperposed -> True))) =
            let
                t1 = getTy scheme
                t2 = TySub.apply tySubs t1
                resScheme = TS.closeOver t2
                
            in
                E.AltAbs args body (Just scheme)
        
        
        f (E.AltAbs args body (Just scheme@(isSuperposed -> False))) =
            E.AltAbs args body (Just scheme)
        
        f x = x





castScheme ::  T.Scheme -> T.Type
castScheme (T.Forall as t) =
    let
        as' = imap freshType as
        s = TySub.TySub $ Map.fromList $ List.zip (CID.idents as) as'

    in
        TySub.apply s t
    
    where
        prefix = Text.pack "!*"
        label = Text.pack . show
        
        freshType :: Int -> ID.Low -> T.Type
        freshType i x =
            T.Var' $ ID.Low' (prefix `Text.append` label i) Nothing




-- freshType :: Lift T.Type
-- freshType 

getTy :: T.Scheme -> T.Type
getTy (T.Forall _ x) = x


isSuperposed :: T.Scheme -> Bool
isSuperposed (T.Forall _ ty) =
    not $ null [ty | (isSuper -> Just ty) <- Uni.universe ty]
    where
        isSuper :: T.Type -> Maybe T.Type
        isSuper x@T.Superposed{} = Just x
        isSuper _ = Nothing


