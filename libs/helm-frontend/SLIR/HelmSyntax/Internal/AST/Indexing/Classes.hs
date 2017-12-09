{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Internal.AST.Indexing.Classes where


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


import qualified SLIR.HelmSyntax.Internal.AST.Indexing.Data.Env as Env
-- *




{-# ANN module "HLint: ignore" #-}




class Bindable a where
    bind :: State.MonadState Env m => a -> m a


class Indexable a where
    index :: State.MonadState Env m => a -> m a


class Referable a where
    refer :: State.MonadState Env m => a -> m a



