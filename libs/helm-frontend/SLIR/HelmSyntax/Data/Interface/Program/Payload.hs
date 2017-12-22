{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SLIR.HelmSyntax.Data.Interface.Program.Payload (
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
import qualified SLIR.HelmSyntax.AST.Data.Base.Etc    as Etc
import qualified SLIR.HelmSyntax.AST.Data.Base.Ident  as ID
import qualified SLIR.HelmSyntax.AST.Data.Base.Types  as T
import qualified SLIR.HelmSyntax.AST.Data.Base.Values as V

-- ~~ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Expressions as E
import qualified SLIR.HelmSyntax.AST.Data.TermLevel.Patterns    as P

-- ~~ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.TopLevel.Unions    as Decl

-- ~~ Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Base       as Base
import qualified SLIR.HelmSyntax.AST.Data.Header.ImportDecl as Decl
-- *


data Test = Test
    deriving (Show, Data, Typeable)



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





