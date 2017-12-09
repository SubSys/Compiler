{-# LANGUAGE NoImplicitPrelude #-}
module SLIR.HelmSyntax.Core (
      frontend
    , toHelmCore
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))

import Prelude (mapM_, IO, String, return)

--- Local
import qualified SLIR.HelmSyntax.Data.Payload as Payload
import qualified SLIR.HelmSyntax.Core.Pipeline as Pipeline
import qualified SLIR.HelmSyntax.Feed.HelmCore as HelmCore

import qualified HLIR.HelmCore.Data.Payload as CIR
-- *


frontend :: IO String -> IO (Either Text Payload.Module)
frontend = Pipeline.pipeline

toHelmCore :: IO (Either Text Payload.Module) -> IO (Either Text CIR.Module)
toHelmCore input = do
    result <- input
    
    case result of
        Left err -> return $ Left err
        Right payload -> return $ Right $ HelmCore.toHelmCore payload


