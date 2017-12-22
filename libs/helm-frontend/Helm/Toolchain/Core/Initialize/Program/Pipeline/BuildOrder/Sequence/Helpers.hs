{-# LANGUAGE NoImplicitPrelude #-}
module Helm.Toolchain.Core.Initialize.Program.Pipeline.BuildOrder.Sequence.Helpers where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Core.Text.Util    (punctuate)

import Prelude (return, String, IO, show, error, (<$>))
import System.FilePath (FilePath)
import Data.List.Index  (imap)

import qualified Control.Monad.State         as M
import qualified Control.Monad.Except        as M
import qualified Control.Monad.RWS           as M
import qualified Control.Monad.Identity      as M
import qualified Control.Monad.Reader        as M
import qualified Data.List                   as List
import qualified Data.Either                 as Either
import qualified Data.Text                   as Text
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Foldable               as Fold
import qualified Data.Monoid                 as Monoid
import qualified Data.Text.IO                as TIO
import qualified Data.Hashable               as Hash
import qualified Data.Generics.Uniplate.Data as Uni
import qualified Text.Show.Prettyprint       as PP

import qualified System.IO                   as SIO
import qualified System.FilePath             as FP
import qualified System.Directory            as SD

import qualified Algebra.Graph               as AG
import qualified Algebra.Graph.Class         as AGC
import qualified Algebra.Graph.AdjacencyMap  as AM
import qualified Algebra.Graph.Export        as Export
import qualified Algebra.Graph.Export.Dot    as ExportDot



--- Local Deps
-- ~ HelmSyntax Interfaces
import qualified SLIR.HelmSyntax.Data.Interface.Module.Payload   as Module
import qualified SLIR.HelmSyntax.Data.Interface.Program.Payload as Program
import qualified SLIR.HelmSyntax.Data.Interface.Utils              as IUtil

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

--- Local
import qualified Helm.Toolchain.System.Data.Report     as Report
import qualified Helm.Toolchain.System.Data.BuildLog   as Log
import qualified Helm.Toolchain.System.Data.BuildOrder as BuildOrder
-- *


removeSudoFFINamespace :: [(Text, [Text])] -> [(Text, [Text])]
removeSudoFFINamespace =
    map removeSudoFFINamespace'


removeSudoFFINamespace' :: (Text, [Text]) -> (Text, [Text])
removeSudoFFINamespace' (node, deps) =
    let
        deps1 = List.filter sudoFFIPref deps
    in 
        (node, deps1)
    
    
    where
        
        sudoFFILabel = Text.pack "Sudo.Helm.Native"
        
        sudoFFIPref :: Text -> Bool
        sudoFFIPref x =
            x /= sudoFFILabel
    

depOrderPred :: (Text, [Text]) -> (Text, [Text]) -> Ordering
depOrderPred (node1, deps1) (node2, deps2)
    | node1 `List.elem` deps2 =
        LT
    
    
    | otherwise =
        EQ

    -- where
        -- xs = List.filter (\x -> x `List.elem` deps1)



noImports (_, []) = True
noImports _       = False







subgroupNodes :: AG.Graph Text -> [(Text, [Text])]
subgroupNodes graph =
    map (subgroupNode graph) nodes
    where
        nodes = AG.vertexList graph


subgroupNode :: AG.Graph Text -> Text -> (Text, [Text])
subgroupNode graph node =
    (node, imports)
    where
        -- Initial Data
        edges = AG.edgeList graph
        
        -- Resulting Data
        imports = mapMaybe ownImports edges 
        
        -- Helpers
        ownImports :: (Text, Text) -> Maybe Text
        ownImports (x, y)
            | x == node =
                Just y
        ownImports _ = Nothing
        




