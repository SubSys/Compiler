{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SLIR.HelmSyntax.Module.Data.Interface (
      Module(..)
    , Header(..)
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
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    (return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import Data.Data (Data, Typeable)

import qualified Prelude as Pre


import qualified Control.Monad              as M
import qualified Control.Monad.State        as M
import qualified Control.Monad.Except       as M
import qualified Control.Monad.RWS          as M
import qualified Control.Monad.Identity     as M
import qualified Control.Monad.Reader       as M
import qualified Control.Monad.Writer       as M
import qualified Control.Monad.Trans        as M

import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as TIO
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import qualified Data.Foldable                as Fold
import qualified Data.Monoid                  as Monoid
import qualified Data.Maybe                   as Maybe
import qualified Data.Either                  as Either
import qualified Data.Char                    as Char
import qualified Data.Word                    as Word
import qualified Data.STRef                   as ST
import qualified Data.Bits                    as Bit
import qualified Data.Fixed                   as Fixed
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as MV
import qualified Data.Vector.Generic          as VG
import qualified Data.IORef                   as IORef
import qualified Data.ByteString              as BS
import qualified Data.Functor                 as Fun


-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable as F


-- + Megaparsec & Related
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

-- + Frameworks
import Framework.Text.Parser

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmSyntax AST
-- ++ Base
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Etc      as Etc
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Ident    as ID
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Types    as T
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Values   as V
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Metadata as Meta
import qualified SLIR.HelmSyntax.AST.Data.Semantic.Base.Header   as Header

-- ++ TermLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Expr     as E
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified SLIR.HelmSyntax.AST.Data.Semantic.TopLevel.Unions    as Decl
-- *


data Header = Header
    { moduleName :: ID.Namespace
    , modulePath :: Text
    , exports    :: Header.Entries
    , imports    :: [Header.ImportDecl]
    }
    deriving (Show, Data, Typeable)


data Program = Program
    { functions :: [Decl.Function]
    , unions :: [Decl.Union]
    , fixities :: [Decl.Infix]
    }
    deriving (Show, Data, Typeable)


data Module = Module
    { header :: Header
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




{-
    # Dependency Utils
-}



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



{-
    ## Misc. Dependency Labels
-}

type IsHidden       = Bool
type DebugFunName   = Text
type DebugConName   = Text
type DebugUnionName = Text 



{-
    # Header Utils
-}

getModuleName :: Module -> ID.Namespace
getModuleName datum =
    moduleName $ header datum


-- | I.e. return filePath of module
--
getModulePath :: Module -> Text
getModulePath datum =
    modulePath $ header datum

getModuleHeader :: Module -> Header
getModuleHeader =
    header


getImports :: Module -> [Header.ImportDecl]
getImports datum =
    imports $ header datum

getExports :: Module -> Header.Entries
getExports datum =
    exports $ header datum



{-
    # Program Utils
-}

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




