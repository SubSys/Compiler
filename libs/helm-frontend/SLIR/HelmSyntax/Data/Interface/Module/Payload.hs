{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SLIR.HelmSyntax.Data.Interface.Module.Payload (
      Module(..)
    , ModuleHeader(..)
    , Program(..)
    , Dependencies(..)
    
    -- |
    -- Dependency Utils
    , emptyDependencies
    , initDependencies
    
    , getDependencies
    , getFixitieDeps
    , getUnionDeps
    , getFunctionDeps
    
    
    -- |
    -- Header Utils
    , getModuleHeader
    , getModuleName
    , getModulePath
    
    , getImports
    , getExports
    
    -- |
    -- Program Utils
    , getFunctions
    , getUnions
    , getFixities
    
    -- Program Updates
    , updateFunctions
    , updateUnions
    , updateFixities
    
    -- Alt. Versions
    , updateFunctions'
    , updateUnions'
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



data ModuleHeader = ModuleHeader
    { moduleName :: ID.Namespace
    , modulePath :: Text
    , exports    :: Base.Entries
    , imports    :: [Decl.ImportDecl]
    }
    deriving (Show, Data, Typeable)


data Program = Program
    { functions :: [Decl.Function]
    , unions :: [Decl.Union]
    , fixities :: [Decl.Infix]
    }
    deriving (Show, Data, Typeable)


data Module = Module
    { header :: ModuleHeader
    , program :: Program
    , deps    :: Dependencies
    }
    deriving (Show, Data, Typeable)


data Dependencies = Dependencies
    { fixitieDeps :: [Decl.Infix]
    , unionDeps :: [Decl.Union]
    , functionDeps :: [Decl.Function]
    
    -- | 
    -- In case the user imports a hidden (not exposed) identifier,
    -- I’m thinking of using the 'debug*Idents' fields for suggesting
    -- such possible scenarios.
    
    , debugFunctionIdents :: [(DebugFunName, IsHidden, ID.Namespace)]
    , debugUnionIdents    :: [(DebugUnionName, [(DebugConName, IsHidden)], IsHidden, ID.Namespace)]
    }
    deriving (Show, Data, Typeable)


-- *
-- | Dependency Utils
-- *


-- | Misc. Dependency Labels
-- 
type IsHidden       = Bool
type DebugFunName   = Text
type DebugConName   = Text
type DebugUnionName = Text 



emptyDependencies :: Dependencies
emptyDependencies =
    Dependencies
        { fixitieDeps = []
        , unionDeps = []
        , functionDeps = []
        , debugFunctionIdents = []
        , debugUnionIdents = []
        }


initDependencies :: Module
                 -> [Decl.Function]
                 -> [Decl.Union]
                 -> [Decl.Infix]
                 -> Module
initDependencies datum fns uns fixs =
    Module
        { header = header datum
        , program = program datum
        , deps = Dependencies
                { fixitieDeps = fixs
                , unionDeps = uns
                , functionDeps = fns
                , debugFunctionIdents = []
                , debugUnionIdents = []
                }
        }


getDependencies :: Module -> Dependencies
getDependencies =
    deps


getFixitieDeps :: Module -> [Decl.Infix]
getFixitieDeps datum =
    fixitieDeps $ deps datum


getUnionDeps :: Module -> [Decl.Union]
getUnionDeps datum =
    unionDeps $ deps datum


getFunctionDeps :: Module -> [Decl.Function]
getFunctionDeps datum =
    functionDeps $ deps datum



-- *
-- | Header Utils
-- *
getModuleName :: Module -> ID.Namespace
getModuleName datum =
    moduleName $ header datum


-- | I.e. return filePath of module
--
getModulePath :: Module -> Text
getModulePath datum =
    modulePath $ header datum

getModuleHeader :: Module -> ModuleHeader
getModuleHeader =
    header


getImports :: Module -> [Decl.ImportDecl]
getImports datum =
    imports $ header datum

getExports :: Module -> Base.Entries
getExports datum =
    exports $ header datum



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



-- *
-- | Program Updates
-- *

updateFunctions :: Module -> [Decl.Function] -> Module
updateFunctions datum new =
    Module
        { header = header datum
        , program = Program
            { functions = new
            , unions = unions $ program datum 
            , fixities = fixities $ program datum 
            }
        , deps = deps datum
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
        , deps = deps datum
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
        , deps = deps datum
        }


-- *
-- | Alt. Version
-- *

updateFunctions' :: [Decl.Function] -> Module -> Module
updateFunctions' fns datum =
    updateFunctions datum fns

updateUnions' :: [Decl.Union] -> Module -> Module
updateUnions' uns datum =
    updateUnions datum uns


