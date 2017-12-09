{-# LANGUAGE NoImplicitPrelude #-}
module Dev.HelmSamples.CPU (
      sampleOnePath
    , sampleOne
) where


-- *
import Core
import Core.Control.Flow

import Prelude (IO, String, (++))

import qualified Data.Text.IO as TIO

import qualified System.IO as SIO
-- *


-- | Resource FilePaths
--
rootPath = "/Users/colbyn/SubSystems/Toolchain/compiler/resources/HelmSamples/CPU"

sampleOnePath = rootPath ++ "/Main.txt"


sampleOne :: IO String
sampleOne =
    SIO.readFile sampleOnePath



