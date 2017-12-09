{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Core (
      pipeline
    , toHelmOutro
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import Prelude (mapM_, IO, String, return)

--- Local
import qualified HLIR.HelmCore.Data.Payload as Payload
import qualified HLIR.HelmCore.Core.Pipeline as Pipeline
import qualified HLIR.HelmCore.Feed.HelmOutro as HelmOutro

import qualified HLIR.HelmOutro.Data.Payload as OIR
-- *


pipeline :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
pipeline = Pipeline.pipeline

toHelmOutro :: IO (Either Text Payload.Module) -> IO (Either Text OIR.Module)
toHelmOutro input = do
    result <- input
    
    case result of
        Left err -> return $ Left err
        Right payload -> return $ Right $ HelmOutro.toHelmOutro payload



