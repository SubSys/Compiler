{-# LANGUAGE NoImplicitPrelude #-}
module Dev.Samples.Basic where


-- *
import Core
import Core.Control.Flow

import Prelude (IO, String, (++))

import qualified Data.Text.IO as TIO
-- *


-- | Resource FilePaths
--
rootPath = "/Users/colbyn/SubSystems/Toolchain/compiler/resources/Samples/Basic"

evaluationPath = rootPath ++ "/Evaluation.txt"
typesPath = rootPath ++ "/Types.txt"



evaluation :: IO Text
evaluation =
    TIO.readFile evaluationPath


types :: IO Text
types =
    TIO.readFile typesPath


