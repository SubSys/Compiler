{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.Rust.Data.Payload (
      Module(..)
    , updateFunctions
    , updateUnions
    , getFunctions
    , getUnions
) where

-- *
import Core

--- Local
-- ~ GCIR-Rust AST
-- ~~ Base
import qualified CGIR.Rust.AST.Base.Ident  as ID
import qualified CGIR.Rust.AST.Base.Types  as T
import qualified CGIR.Rust.AST.Base.Values as V
import qualified CGIR.Rust.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified CGIR.Rust.AST.TermLevel.Stmt        as S
import qualified CGIR.Rust.AST.TermLevel.Patterns    as P
import qualified CGIR.Rust.AST.TermLevel.Block       as Decl
-- ~~ TopLevel
import qualified CGIR.Rust.AST.TopLevel.Functions as Decl
import qualified CGIR.Rust.AST.TopLevel.Unions    as Decl
-- *


data Module = Module
    { functions :: [Decl.Function]
    , unions :: [Decl.Union]
    }
    deriving (Show)


updateFunctions :: Module -> [Decl.Function] -> Module
updateFunctions payload fns =
    Module
        { functions = fns
        , unions = unions payload
        }


updateUnions :: Module -> [Decl.Union] -> Module
updateUnions payload us =
    Module
        { functions = functions payload
        , unions = us
        }



getFunctions :: Module -> [Decl.Function]
getFunctions = functions

getUnions :: Module -> [Decl.Union]
getUnions = unions


