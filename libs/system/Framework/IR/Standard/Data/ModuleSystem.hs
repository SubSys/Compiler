{-# LANGUAGE NoImplicitPrelude #-}
module Framework.IR.Standard.Data.ModuleSystem (
      Exporting(..)
    , Importing(..)
    , Entry(..)
) where


-- *
import Core
-- *





newtype Exporting = Exporting [Entry]
    deriving (Show)

newtype Importing = Importing [Entry]
    deriving (Show)


data Entry
    = Value Text
    | Union Text [Text]
    deriving (Show)










