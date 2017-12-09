{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmCore.Data.Payload (
      Module(..)
    , updateFunctions
    , updateUnions
    , getFunctions
    , getUnions
) where

-- *
import Core

--- Local
-- ~ HelmCore AST
-- ~~ Base
import qualified HLIR.HelmCore.AST.Base.Ident  as ID
import qualified HLIR.HelmCore.AST.Base.Types  as T
import qualified HLIR.HelmCore.AST.Base.Values as V
-- ~~ TermLevel
import qualified HLIR.HelmCore.AST.TermLevel.Expressions as E
import qualified HLIR.HelmCore.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmCore.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmCore.AST.TopLevel.Unions    as Decl
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


