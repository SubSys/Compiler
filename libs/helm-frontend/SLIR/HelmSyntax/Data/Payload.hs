{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module SLIR.HelmSyntax.Data.Payload (
      Module(..)
    , ModuleHeader(..)
    , Program(..)
    
    -- |
    -- Header Utils
    , getModuleName
    
    -- |
    -- Program Utils
    , getFunctions
    , getUnions
    , getFixities
    , updateFunctions
    , updateUnions
    , updateFixities
) where


-- *
import Core

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
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Base      as Header
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Exporting as Export
import qualified SLIR.HelmSyntax.AST.Data.Header.Module.Importing as Import
-- *



data Module = Module
    { header :: ModuleHeader
    , program :: Program
    }
    deriving (Show)


data ModuleHeader = ModuleHeader
    { moduleName :: ID.Namespace
    , exporting :: Export.ModuleExporting
    , importing :: Import.ModuleImporting
    }
    deriving (Show)

data Program = Program
    { functions :: [Decl.Function]
    , unions :: [Decl.Union]
    , fixities :: [Decl.Infix]
    }
    deriving (Show)



-- *
-- | Header Utils
-- *
getModuleName :: Module -> ID.Namespace
getModuleName datum =
    moduleName $ header datum




-- *
-- | Program Utils
-- *
getFunctions :: Module -> [Decl.Function]
getFunctions datum =
    functions $ program datum

getUnions :: Module -> [Decl.Union]
getUnions datum =
    unions $ program datum

getFixities :: Module -> [Decl.Infix]
getFixities datum =
    fixities $ program datum





updateFunctions :: Module -> [Decl.Function] -> Module
updateFunctions datum new =
    Module
        { header = header datum
        , program = Program
            { functions = new
            , unions = unions $ program datum 
            , fixities = fixities $ program datum 
            }
        }


updateUnions :: Module -> [Decl.Union] -> Module
updateUnions datum new =
    Module
        { header = header datum
        , program = Program
            { functions = functions $ program datum 
            , unions = new
            , fixities = fixities $ program datum 
            }
        }



updateFixities :: Module -> [Decl.Infix] -> Module
updateFixities datum new =
    Module
        { header = header datum
        , program = Program
            { functions = functions $ program datum 
            , unions = unions $ program datum 
            , fixities = new
            }
        }

