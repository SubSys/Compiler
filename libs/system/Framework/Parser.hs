{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE MultiWayIf           #-}

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
module Framework.Parser
    ( module Data.String
    , module Data.Char
    , module Data.List
    , module Data.Void
    , module Data.Proxy
    , module Data.Either
    
    -- Megaparsec
    , module Text.Megaparsec
    
    
    -- Local Exports
    , module Framework.Parser.Prelude
    )
where



-- *
import Prelude (putStrLn)

import Core hiding (parens)
import Core.Control.Flow

import Data.String
import Data.Char
import Data.List
import Data.Void
import Data.Proxy
import Data.Either


import Text.Megaparsec



-- | Local Prelude
import Framework.Parser.Prelude
-- *

