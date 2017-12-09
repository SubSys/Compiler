{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Internal.AST.Indexing.Instances.Core where



-- *
import Core
import Core.Control.Flow
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
import SLIR.HelmSyntax.Internal.AST.Indexing.Classes
    (Bindable(..), Indexable(..), Referable(..), refer, bind, index)

import qualified SLIR.HelmSyntax.Internal.AST.Indexing.Data.Env as Env
-- *




{-# ANN module "HLint: ignore" #-}








-- *
-- | Bindable
-- *


instance Bindable IR.Binder where
    bind (IR.Binder txt) = do
        idx <- Env.getCounter <$> State.get
        
        
        
        State.modify $ Env.mergeSubst $ Env.newSubst txt idx
        
        State.modify Env.incCounter
        
        return $ IR.BinderIndex idx


instance Referable IR.Ref where
    refer (IR.Ref txt) = do
        subs <- Env.getSubst <$> State.get
        
        case Map.lookup txt subs of
            Nothing -> return $ IR.Ref txt
            Just i  -> return $ IR.RefIndex i


