{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}
module SLIR.HelmSyntax.Internal.AST.Instances.Essential (
    module SLIR.HelmSyntax.Internal.AST.Instances.Essential.Equality
) where


-- *
import Core

import Data.Data (Data, Typeable)
import qualified Text.Show.Prettyprint as PP
import qualified Data.Generics.Uniplate.Data as Uni
import Data.Monoid ((<>))


--- Local
import qualified SLIR.HelmSyntax.Internal.AST as IR

import SLIR.HelmSyntax.Internal.AST.Instances.Essential.Equality
-- *



deriving instance Ord IR.Low
deriving instance Ord IR.Sym
deriving instance Ord IR.Big

deriving instance Ord IR.Namespace


deriving instance Ord IR.Type
deriving instance Ord IR.LiteralType
deriving instance Ord IR.Scheme


