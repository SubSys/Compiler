{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module SLIR.HelmSyntax.Module.Core.Ordering.Syntax (
    sortEvalOrder
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
import Data.Monoid ((<>))
import Prelude
    ( return
    , String
    , IO
    , show
    , error
    , (<$>)
    , (>>=)
    , (>>)
    , fromIntegral
    )

import qualified Prelude    as Pre
import qualified Core.Utils as Core

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
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + Graphing & Related
import qualified Algebra.Graph              as G
import qualified Algebra.Graph.Export.Dot   as Dot
import qualified Algebra.Graph.AdjacencyMap as AM



-- + HelmSyntax Module Interface
import qualified SLIR.HelmSyntax.Module.Data.Interface as I

-- + HelmSyntax AST Renderer
import qualified SLIR.HelmSyntax.AST.Render.Syntax.Driver as Syntax

-- + HelmSyntax AST Utils
import qualified SLIR.HelmSyntax.AST.Utils.Scope           as Scope
import qualified SLIR.HelmSyntax.AST.Utils.Auxiliary.Ident as ID

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

-- + HelmSyntax - Module Drivers
import qualified SLIR.HelmSyntax.Module.Core.Parser.Driver    as Driver
import qualified SLIR.HelmSyntax.Module.Core.TypeCheck.Driver as Driver

-- + Local
import qualified SLIR.HelmSyntax.Module.Core.Ordering.Data.Report      as Report
import qualified SLIR.HelmSyntax.Module.Core.Ordering.Debug.Dot.Export as Export
-- *



{-# ANN module ("HLint: ignore" :: String) #-}






sortEvalOrder :: [Decl.Function] -> Either Report.OrderingError [Decl.Function]
sortEvalOrder input = M.runExcept (Uni.transformM f input)
    where
        f :: (M.MonadError Report.OrderingError m) => [Decl.Function] -> m [Decl.Function]
        f decls@(initDecls -> graph) =
            case sortDecls graph of
                Left err -> M.throwError err
                Right xs ->
                    return $ arrangeDeclsBy xs decls





sortEvalOrder' :: [Decl.Function] -> Either Report.OrderingError [Decl.Function]
sortEvalOrder' decls@(initDecls -> graph) =
    case sortDecls graph of
        Left err -> Left err
        Right xs ->
            Right $ arrangeDeclsBy xs decls



arrangeDeclsBy :: [Text] -> [Decl.Function] -> [Decl.Function]
arrangeDeclsBy xs decls = flatten $ map get xs
    where
        pred :: Text -> Decl.Function -> Bool
        pred (ID.Ident_ -> ident) fn =
            ident == ID.get fn
        
        get :: Text -> [Decl.Function]
        get label =
            List.filter (pred label) decls


sortDecls :: AM.AdjacencyMap Text -> Either Report.OrderingError [Text]
sortDecls (AM.topSort -> result) =
    case result of
        -- TODO: Better error message
        Nothing -> Left (Report.CyclicDependencies [])
        Just xs -> Right $ List.reverse xs




initDecls :: [Decl.Function] -> AM.AdjacencyMap Text
initDecls decls =
    AM.fromAdjacencyList $ map initDecl decls
    
    where
        initDecl :: Decl.Function -> (Text, [Text])
        initDecl decl@(Decl.Function name@(ID.get -> ident) args expr sig meta) =
            let
                -- Setup
                freeVars = Scope.freeVars decl
                
                -- Finalize
                node = (ID.getText ident, ID.getTexts freeVars)
            in
                node





