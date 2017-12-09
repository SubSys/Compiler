{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Various Standardized IR Constructors
--
module Framework.IR.Standard.Data (
      ModName(..)
    , Exporting(..)
    , Importing(..)
    , Ware(..)
) where


-- *
import Core
import Data.Data (Data, Typeable)

import qualified Data.HashMap.Strict as HM

--- Local
import qualified Framework.IR.Standard.Ware.Unresolved as Unresolved
-- *



-- TODO: Still working on…
--





-- *
-- | Module System
-- *
newtype ModName = ModName Text
    deriving (Show, Data, Typeable)



-- *
-- | Module Export/Import System
-- *
data Exporting
    = Exporting [Ware]
    | ExportsUnresolved
    deriving (Show, Data, Typeable)

data Importing
    = Importing [Ware]
    | ImportsUnresolved
    deriving (Show, Data, Typeable)



data Ware
    = Value Name
    | Type Name Constructors
    deriving (Show, Data, Typeable)


type Constructors = [Text]

-- *
-- | Misc. Internal
-- *
type Name = Text

