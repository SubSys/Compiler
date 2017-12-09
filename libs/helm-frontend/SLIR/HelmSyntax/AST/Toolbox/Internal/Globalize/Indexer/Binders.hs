{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.AST.Toolbox.Internal.Globalize.Indexer.Binders (
    binder
) where


-- *
import Core
import Core.Control.Flow
import Prelude (return, error, show)

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Control.Monad.State.Lazy as State


--- Local
import qualified SLIR.HelmSyntax.AST.Toolbox.Internal.Globalize.Data.Env as Env

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




binder :: State.MonadState Env.Env m => Scope.Binder -> m Scope.Binder
binder (Scope.Binder txt) = do
    e <- State.get
    let idx = Env.getCounterIdx e
    
    let updater = Env.updateBinderState txt idx
    
    State.modify updater
    
    return $ Scope.BinderIndex idx


binder (Scope.BinderIndex i) = do
    -- e <- State.get
    -- 
    -- return $ error $ show $ (show i, show e)
    
    return $ Scope.BinderIndex i








