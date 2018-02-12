{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmFlat.Core.Ordering.Driver (
    sortEvalOrder
  , sortEvalOrder'
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



-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Module.Data.Interface as I

-- + HelmFlat AST Renderer
import qualified HLIR.HelmFlat.AST.Render.Syntax.Driver as Syntax

-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Scope           as Scope
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Ident as ID

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Metadata as Meta
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Header   as Header

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + HelmFlat - Module Drivers
import qualified HLIR.HelmFlat.Core.Parser.Driver    as Driver
import qualified HLIR.HelmFlat.Core.TypeCheck.Driver as Driver

-- + Local
import qualified HLIR.HelmFlat.Core.Ordering.Data.Report      as Report
import qualified HLIR.HelmFlat.Core.Ordering.Debug.Dot.Export as Export
import qualified HLIR.HelmFlat.Core.Ordering.Syntax           as Syntax
-- *



{-# ANN module ("HLint: ignore" :: String) #-}



-- TODO:
-- * Verify handling of overloaded functions with a more complicated program…
-- * How to handle sorting of overloaded functions, individually (Before desugaring)?

-- NOTE:
-- * For now, sorting of overloaded functions occurs as a single group… 


sortEvalOrder :: IO (Either Text I.Module) -> IO (Either Text I.Module)
sortEvalOrder upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return
                $ sortEvalOrder' payload


sortEvalOrder' :: I.Module -> Either Text I.Module
sortEvalOrder' payload@(I.getFunctions -> decls) =
    case Syntax.sortEvalOrder decls of
        Left err -> Left $ Text.pack $ PP.prettyShow err
        Right decls' ->
            Right
                $ I.updateFunctions payload decls'











