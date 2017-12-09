{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module SLIR.HelmSyntax.Internal.AST.Classes.StripMeta where


-- *
import Core
-- *




{-# ANN module "HLint: ignore" #-}


class StripMeta a where
    stripMeta :: a -> a










