{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
module HLIR.HelmIntro.Data.Payload (
    Program(..)
  , getFunctions
  , getUnions
  , updateFunctions
  , updateUnions
) where


-- *
import Core

import Data.Data (Data, Typeable)

--- Framework
import qualified Framework.Pipeline.Data as Pipe

--- Local
-- ~ HelmSyntax AST
-- ~~ Base
import qualified HLIR.HelmIntro.AST.Data.Base.Ident  as ID
import qualified HLIR.HelmIntro.AST.Data.Base.Types  as T
import qualified HLIR.HelmIntro.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Expressions as E
import qualified HLIR.HelmIntro.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Functions as Decl
import qualified HLIR.HelmIntro.AST.Data.TopLevel.Unions    as Decl
-- *




data Program = Program
    { unions    :: [Decl.Union]
    , functions :: [Decl.Function]
    }
    deriving (Show, Data, Typeable)






getFunctions :: Program -> [Decl.Function]
getFunctions =
    functions

getUnions :: Program -> [Decl.Union]
getUnions =
    unions


updateFunctions :: [Decl.Function] -> Program -> Program
updateFunctions fns datum =
    Program
        { functions = fns
        , unions = unions datum
        }

updateUnions :: [Decl.Union] -> Program -> Program
updateUnions uns datum =
    Program
        { functions = functions datum
        , unions = uns
        }





