{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Internal.AST.Indexing.Instances where


-- *
import Core
import Core.Control.Flow
import Core.List.Util (flatten)
import Prelude (mapM_, IO, String, return, (<$>))

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Control.Monad.State.Lazy as State


--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

-- ~ Indexing
import SLIR.HelmSyntax.Internal.AST.Indexing.Data.Env (Env)
import SLIR.HelmSyntax.Internal.AST.Indexing.Classes (Bindable(..), Indexable(..), Referable(..), refer, bind, index)

import qualified SLIR.HelmSyntax.Internal.AST.Indexing.Data.Env as Env

-- ~~ Indexing Core Instances (Deps)
import SLIR.HelmSyntax.Internal.AST.Indexing.Instances.Core ()
-- *




{-# ANN module "HLint: ignore" #-}







instance Indexable IR.Function where
    index (IR.FnDecl name args expr optSig optMeta) = do
        
        env <- State.get
        
        let (name', env1) = State.runState (bind name) env
        let (args', env2) = State.runState (bind args) env1
        let (expr', env3) = State.runState (index expr) env2
        
        let updatedEnv = Env.getCounter env3
                |> Env.mergeCounter env1
        
        State.put updatedEnv
        
        return $ IR.FnDecl name' args' expr' optSig optMeta



-- TODO: Use Fold (because of Env)?
instance Indexable [IR.Function] where
    index xs =
        State.mapM (index) xs



instance Indexable IR.Expr where
    index (IR.LetExpr fns expr optMeta) = do
        env <- State.get
        
        let (fns', env1) = State.runState (index fns) env
        let (expr', env2) = State.runState (index expr) env1
        
        -- fns' <- index fns
        -- expr' <- index expr
        
        let updatedEnv = Env.getCounter env2
                |> Env.mergeCounter env
        
        State.put updatedEnv
        
        return $ IR.LetExpr fns' expr' optMeta
    
    index (IR.VarExpr name optMeta) = do
        name' <- refer name
        
        return $ IR.VarExpr name' optMeta


--     index (IR.RecordExpr [(Low Text, Expr)] optMeta) =
--         return $ IR.RecordExpr x optMeta
-- 
--     index (IR.TupleExpr [Expr] optMeta) =
--         return $ IR.TupleExpr x optMeta
-- 
--     index (IR.ListExpr [Expr] optMeta) =
--         return $ IR.ListExpr x optMeta
-- 
--     index (IR.ConExpr (Big Text) optMeta) =
--         return $ IR.ConExpr x optMeta
-- 
    index (IR.BinOpExpr sym e1 e2 optMeta) = do
        sym' <- refer sym
        
        e1' <- index e1
        e2' <- index e2
        
        return $ IR.BinOpExpr sym' e1' e2' optMeta

    index (IR.IfExpr intros elseExpr optMeta) = do
        
        intros' <- State.mapM branch intros
        elseExpr' <- index elseExpr
        
        
        return $ IR.IfExpr intros' elseExpr' optMeta

        where
            branch (con, body) = do
                con' <- index con
                body' <- index body
                return (con', body)


    -- index (IR.CaseExpr Expr [CaseAlt] optMeta) =
    --     return $ IR.CaseExpr x optMeta

--     index (IR.RecordUpdateExpr (Low Text) [(Low Text, Expr)] optMeta) =
--         return $ IR.RecordUpdateExpr x optMeta
-- 
--     index (IR.RecordAccessExpr (Low Text) (Maybe Expr) optMeta) =
--         return $ IR.RecordAccessExpr x optMeta
-- 
    index (IR.ParensExpr expr optMeta) = do
        
        expr' <- index expr
        
        return $ IR.ParensExpr expr' optMeta


    index (IR.AppExpr e1 e2 optMeta) = do
        e1' <- index e1
        e2' <- index e2
        
        return $ IR.AppExpr e1' e2' optMeta


--     index (IR.AbsExpr (Low Binder) Expr optMeta) =
--         return $ IR.AbsExpr x optMeta
    index x = return x




-- *
-- | Identifiers
-- *
instance Bindable a => Bindable (IR.Low a) where
    bind (IR.Low x ns m) = do
        x' <- bind x
        return $ IR.Low x' ns m

instance Bindable a => Bindable (IR.Sym a) where
    bind (IR.Sym x ns m) = do
        x' <- bind x
        return $ IR.Sym x' ns m


instance Bindable a => Bindable [IR.Low a] where
    bind xs =
        State.mapM bind xs

instance Bindable Text where
    bind x = return x


instance Referable a => Referable (IR.Low a) where
    refer (IR.Low x ns m) = do
        x' <- refer x
        return $ IR.Low x' ns m

instance Referable a => Referable (IR.Sym a) where
    refer (IR.Sym x ns m) = do
        x' <- refer x
        return $ IR.Sym x' ns m


instance Referable Text where
    refer x = return x

















