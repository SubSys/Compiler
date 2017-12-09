{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Data.Payload.Input (
      Payload(..)
    , Program(..)
    , getProgram
    , getEnums
    , updateProgram
    , updateEnums
) where


-- *
import Core

--- Framework
import qualified Framework.Pipeline.Data as Pipe

--- Local
-- ~ RedRust AST
import qualified CGIR.Rust.AST.TopLevel.CallSites as CS
import qualified CGIR.Rust.AST.TopLevel.TypeSystem as TS
-- *


type Payload = Pipe.Payload Program


data Program = Program
    { program :: [CS.FunctionDecl]
    , enums :: [TS.EnumDecl]
    }
    deriving (Show)



getProgram :: Payload -> [CS.FunctionDecl]
getProgram datum =
    program $ Pipe.getPayload datum


getEnums :: Payload -> [TS.EnumDecl]
getEnums datum =
    enums $ Pipe.getPayload datum



updateProgram :: Payload -> [CS.FunctionDecl] -> Payload
updateProgram datum fns =
    Pipe.updatePayload new datum
    where
        new = Program
            { program = fns
            , enums = enums $ Pipe.getPayload datum
            }

updateEnums :: Payload -> [TS.EnumDecl] -> Payload
updateEnums datum es =
    Pipe.updatePayload new datum
    where
        new = Program
            { program = program $ Pipe.getPayload datum
            , enums = es
            }