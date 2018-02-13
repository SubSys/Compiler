{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmFlat.Core.Index.Driver (
    index
  , index'
  , globalize
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

-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Program.Data.Interface as I

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

-- + Local
import qualified HLIR.HelmFlat.Core.Index.Data            as Index
import qualified HLIR.HelmFlat.Core.Index.Scope.Referable as Referable
import qualified HLIR.HelmFlat.Core.Index.Scope.Bindable  as Bindable
import qualified HLIR.HelmFlat.Core.Index.Scope.Utils     as Scope
import qualified HLIR.HelmFlat.Core.Index.Syntax          as Syntax
import qualified HLIR.HelmFlat.Core.Index.Setup.Decls     as Setup
-- *





index :: IO (Either Text I.Program) -> IO (Either Text I.Program)
index input = do
    result <- input
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return
                $ Right
                $ index' payload




index' :: I.Program -> I.Program
index' payload@(I.getFunctions -> decls) =
        I.updateFunctions (globalize decls) payload
    





-- *
-- | Indexers
-- *

globalize :: [Decl.Function] -> [Decl.Function]
globalize input =
    let
        -- Setup Data
        (fns, ignored) = Setup.setupDecls input
        
        -- Finish
        result = fst $ Index.runState (Syntax.traverseDecls fns) 0 Map.empty
    in
        flatten ignored ++ result






