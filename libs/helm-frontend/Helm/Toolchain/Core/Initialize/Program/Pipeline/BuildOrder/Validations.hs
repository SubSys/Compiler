{-# LANGUAGE NoImplicitPrelude #-}
module Helm.Toolchain.Core.Initialize.Program.Pipeline.BuildOrder.Validations (
    checkCycles
) where


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




checkCycles :: AG.Graph Text -> Either Report.BuildError (AG.Graph Text)
checkCycles graph
    | null result =
        -- Ok
        Right graph
    
    | otherwise =
        Left (toReport result)


    where
        -- Initial Data 
        edges = AG.edgeList graph
        nodes = AG.vertexList graph
        
        -- Finish
        result = mapMaybe (checkCycles' edges) nodes
        
        -- Helpers
        toReport :: [(Text, Text)] -> Report.BuildError
        toReport ((node1, node2):_) =
            Report.ImportsCycle node1 node2



checkCycles' :: [(Text, Text)] -> Text -> Maybe (Text, Text)
checkCycles' edgeList node
    | null cycles =
        -- Ok.
        Nothing
    
    | otherwise =
        if List.length cycles == 1 then
            
            Just (node, List.head cycles)

        else
            error
                $  "Internal compiler error, how did the compiler reach this state?\n"
                ++ "I’m pretty sure the cycle list should be a single element"

    
    where
        ownImports = map snd $ List.filter getOwnImports edgeList
        cycles = map fst $ mapMaybe getCycles edgeList
        
        -- Helpers
        getOwnImports :: (Text, Text) -> Bool
        getOwnImports (x, y) =
            x == node
        
        
        getCycles :: (Text, Text) -> Maybe (Text, Text)
        getCycles (x, y)
            | y == node && x `List.elem` ownImports =
                Just (x, y)
        
        getCycles _ = Nothing





