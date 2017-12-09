{-# LANGUAGE NoImplicitPrelude #-}
module Dev.Samples.Legacy where


-- *
import Core
import Core.Control.Flow

import Prelude (IO, String, (++))

import qualified Data.Text.IO as TIO
-- *


-- | Resource FilePaths
--
rootPath = "/Users/colbyn/SubSystems/Toolchain/compiler/resources/Samples/Legacy"

onePath = rootPath ++ "/One.txt"



one :: IO Text
one =
    TIO.readFile onePath