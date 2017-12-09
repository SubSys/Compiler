{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.LightRoast.Core (
      pipeline
    , toRust
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Prelude (return, String, IO, show, error, (<$>))

import Data.List.Index  (imap)

import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M

import qualified Data.List     as List
import qualified Data.Text     as Text
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as Monoid


--- Local Deps
-- ~ (CGIR) Rust Payload
import qualified CGIR.Rust.Data.Payload as RIR

-- ~ LightRoast Payload
import qualified LLIR.LightRoast.Data.Payload as Payload


-- ~ LightRoast AST
-- ~~ Base
import qualified LLIR.LightRoast.AST.Base.Ident  as ID
import qualified LLIR.LightRoast.AST.Base.Types  as T
import qualified LLIR.LightRoast.AST.Base.Values as V
import qualified LLIR.LightRoast.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified LLIR.LightRoast.AST.TermLevel.Stmt        as S
import qualified LLIR.LightRoast.AST.TermLevel.Patterns    as P
import qualified LLIR.LightRoast.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified LLIR.LightRoast.AST.TopLevel.Functions as Decl
import qualified LLIR.LightRoast.AST.TopLevel.Unions    as Decl


--- Local
import qualified LLIR.LightRoast.Feed.Rust as Rust
-- *


{-# ANN module "HLint: ignore" #-}




pipeline :: IO (Either Text Payload.Module) -> IO (Either Text Payload.Module)
pipeline x = x



toRust :: IO (Either Text Payload.Module) -> IO (Either Text RIR.Module)
toRust input = do
    result <- input
    
    case result of
        Left err -> return $ Left err
        Right payload -> return $ Right $ Rust.toRust payload



