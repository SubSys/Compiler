{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.LightRoast.Data.Payload (
      Module(..)
    , updateFunctions
    , updateUnions
    , getFunctions
    , getUnions
) where

-- *
import Core

--- Local
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


