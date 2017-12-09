{-# LANGUAGE
      NoImplicitPrelude
    , ExistentialQuantification
#-}
module Framework.Parser.Data.Util (
    Unsorted(..)
) where

-- *
import Prelude ()
import Core

import Data.List ((++))
import Data.Typeable
-- *

-- | Heterogenous Value Container
--  Convenient item container for temporary working with intermediate values
--  of conflicting data types.
data Unsorted = forall a. (Show a, Typeable a) => Unsorted a

instance Show Unsorted where
    show (Unsorted a) = show "Unsorted (" ++ show a ++ " )"
    