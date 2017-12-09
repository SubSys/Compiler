{-# LANGUAGE NoImplicitPrelude #-}
module Dev.Samples.Complex where


-- *
import Core
import Core.Control.Flow

import Prelude (IO, String, (++))

import qualified Data.Text.IO as TIO
-- *


-- | Resource FilePaths
--
rootPath = "/Users/colbyn/SubSystems/Toolchain/compiler/resources/Samples/Complex"

systemOnePath = rootPath ++ "/SystemOne.txt"



systemOne :: IO Text
systemOne =
    TIO.readFile systemOnePath

