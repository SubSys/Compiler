{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Core (
      pipeline
    , toLightRoast
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import Prelude (mapM_, IO, String, return)

--- Local
import qualified HLIR.HelmOutro.Data.Payload as Payload
import qualified HLIR.HelmOutro.Core.Pipeline as Pipeline
import qualified HLIR.HelmOutro.Feed.LightRoast as LightRoast

import qualified LLIR.LightRoast.Data.Payload as LIR
-- *


pipeline :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
pipeline = Pipeline.pipeline

toLightRoast :: IO (Either Text Payload.Module) -> IO (Either Text LIR.Module)
toLightRoast input = do
    result <- input
    
    case result of
        Left err -> return $ Left err
        Right payload -> return $ Right $ LightRoast.toLightRoast payload

