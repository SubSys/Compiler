{-# LANGUAGE NoImplicitPrelude #-}
module Framework.Pipeline.Data (
      Context(..)
    , Payload(..)
    , updatePayload
    , getPayload
) where


-- *
import Core

--- Local
import qualified Framework.IR.Standard.Data as StdIR
-- *


data Context = Context
    { moduleName :: StdIR.ModName
    , imports :: StdIR.Importing
    , exports :: StdIR.Exporting
    }
    deriving (Show)



data Payload a = Payload
    { context :: Context
    , payload :: a
    }
    deriving (Show)

updatePayload :: a -> Payload a -> Payload a
updatePayload new datum =
    Payload
        { context = context_
        , payload = new
        }
    where
        context_ = context datum

getPayload :: Payload a -> a
getPayload = payload
    


