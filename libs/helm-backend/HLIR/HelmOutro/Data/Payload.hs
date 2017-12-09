{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmOutro.Data.Payload (
      Module(..)
    , updateFunctions
    , updateUnions
    , getFunctions
    , getUnions
) where

-- *
import Core

--- Local
-- ~ HelmOutro AST
-- ~~ Base
import qualified HLIR.HelmOutro.AST.Base.Ident  as ID
import qualified HLIR.HelmOutro.AST.Base.Types  as T
import qualified HLIR.HelmOutro.AST.Base.Values as V
import qualified HLIR.HelmOutro.AST.Base.Etc    as Etc
-- ~~ TermLevel
import qualified HLIR.HelmOutro.AST.TermLevel.Expressions as E
import qualified HLIR.HelmOutro.AST.TermLevel.Patterns    as P
-- ~~ TopLevel
import qualified HLIR.HelmOutro.AST.TopLevel.Functions as Decl
import qualified HLIR.HelmOutro.AST.TopLevel.Unions    as Decl
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


