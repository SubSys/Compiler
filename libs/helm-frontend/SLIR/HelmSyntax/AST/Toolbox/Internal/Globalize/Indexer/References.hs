{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.AST.Toolbox.Internal.Globalize.Indexer.References where


-- *
import Core
import Core.Control.Flow
import Prelude (return)

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





reference :: State.MonadState Env.Env m => Env.Subst -> Scope.Ref -> m Scope.Ref
reference subs original@(Scope.Ref name) =
    case Map.lookup name subs of
        Just i ->
            return $ Scope.RefIndex i
        Nothing ->
            return original



reference subs ref@(Scope.RefIndex i) =
    
    return ref
    






