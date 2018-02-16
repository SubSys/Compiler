{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE ViewPatterns #-}
module GCIR.RustCG.Core.Index.Scope.Referable (
    referable
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (flatten, singleton)
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
import qualified Data.Data                    as Data

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP



-- + RustCG AST Interface
import qualified GCIR.RustCG.Data.Interface as I

-- + RustCG AST Utils
import qualified GCIR.RustCG.AST.Utils.Ident as ID

-- + RustCG AST
-- ++ Base
import qualified GCIR.RustCG.AST.Data.Semantic.Base.Ident                 as ID
import qualified GCIR.RustCG.AST.Data.Semantic.Base.Literals              as Lit
import qualified GCIR.RustCG.AST.Data.Semantic.Base.Types                 as T
import qualified GCIR.RustCG.AST.Data.Semantic.Base.Etc                   as Etc
-- ++ Block Level
import qualified GCIR.RustCG.AST.Data.Semantic.BlockLevel.Stmt            as S
import qualified GCIR.RustCG.AST.Data.Semantic.BlockLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified GCIR.RustCG.AST.Data.Semantic.DeclLevel.Enums.Variants   as Decl
import qualified GCIR.RustCG.AST.Data.Semantic.DeclLevel.Enums            as Decl
import qualified GCIR.RustCG.AST.Data.Semantic.DeclLevel.Functions        as Decl

-- + Local
import qualified GCIR.RustCG.Core.Index.Data.System as Sys
-- *



referable :: ID.Path -> Sys.State (ID.Path, Sys.Subst)
referable path = do
    s <- M.ask

    case Map.lookup (ID.getRefAsIdent path) s of
        Nothing    -> return (path, Map.empty)
        Just ident -> return (ID.updatePathRef ident path, Map.empty)









