{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.AST.Toolbox.Internal.Globalize.Indexer where


-- *
import Core
import Core.Control.Flow
import Prelude (return, error, show, (=<<), (<$>), (<*>))

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Control.Monad.State.Lazy as State
import qualified Data.Functor.Identity as Fun
import qualified Control.Monad.Morph as Morph

--- Local
import qualified SLIR.HelmSyntax.AST.Toolbox.Internal.Globalize.Data.Env as Env
import qualified SLIR.HelmSyntax.AST.Toolbox.Internal.Globalize.Indexer.Binders as Indexer
import qualified SLIR.HelmSyntax.AST.Toolbox.Internal.Globalize.Indexer.References as Indexer

-- ~ Misc. Bindable Helpers
import qualified SLIR.HelmSyntax.AST.Toolbox.Internal.Globalize.Misc.Helpers as Helper

-- ~ HelmSyntax AST
-- ~~ Base
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ HelmSyntax AST Toolbox
import qualified SLIR.HelmSyntax.AST.Toolbox.Internal.Data.Scope as Scope
-- *



{-# ANN module "HLint: ignore" #-}


root :: Decl.Function -> Decl.Function
root decl =
    let 
        -- Initial Data
        depth = Helper.binderDepth decl
        initialEnv = Env.initEnv 0
    
    in
        fst $ State.runState (Uni.transformBiM index decl) initialEnv



-- | Bottom-Up Transformation
--
index :: State.MonadState Env.Env m
      => Scope.Scope
      -> m Scope.Scope

index (Scope.FnDeclScope (intro, outro)) = do
        
        -- Index Binders
        intro' <- Uni.transformBiM Indexer.binder intro
        subs <- Env.getSubst <$> State.get
        
        -- Index References
        let updater = Indexer.reference subs
        outro' <- Uni.transformBiM updater outro
        
        State.modify Env.clearSubst
        
        -- return $ error $ show $ (show intro, show subs)
        
        -- Done
        return $ Scope.FnDeclScope (intro', outro')


index (Scope.OpDeclScope (intro, outro)) = do
    -- Index Binders
    intro' <- Uni.transformBiM Indexer.binder intro
    subs <- Env.getSubst <$> State.get
    
    -- Index References
    let updater = Indexer.reference subs
    outro' <- Uni.transformBiM updater outro
    
    return $ Scope.OpDeclScope (intro', outro')


index (Scope.LetScope    (intro, outro)) = do
    -- Index Binders
    -- intro' <- Uni.transformBiM Indexer.binder intro
    -- subs <- Env.getSubst <$> State.get
    
    -- Index References
    -- let updater = Indexer.reference subs
    -- outro' <- Uni.transformBiM updater outro
    
    
    -- Done
    return $ Scope.LetScope (intro, outro)


