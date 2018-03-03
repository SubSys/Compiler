{-# LANGUAGE NoImplicitPrelude #-}
module CGIR.RustCG.Core.Index.Driver (
    index
  , index'
  , globalize
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
import qualified CGIR.RustCG.Data.Interface as I

-- + RustCG AST Utils
import qualified CGIR.RustCG.AST.Utils.Functions as Decl

-- + RustCG AST
-- ++ Base
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Ident                 as ID
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Literals              as Lit
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Types                 as T
import qualified CGIR.RustCG.AST.Data.Semantic.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.RustCG.AST.Data.Semantic.BlockLevel.Stmt            as S
import qualified CGIR.RustCG.AST.Data.Semantic.BlockLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified CGIR.RustCG.AST.Data.Semantic.DeclLevel.Enums.Variants   as Decl
import qualified CGIR.RustCG.AST.Data.Semantic.DeclLevel.Enums            as Decl
import qualified CGIR.RustCG.AST.Data.Semantic.DeclLevel.Functions        as Decl

-- + Local Prelude
import CGIR.RustCG.Core.Index.Data.System (enter, binder)

-- + Local
import qualified CGIR.RustCG.Core.Index.Data.System                as Sys
import qualified CGIR.RustCG.Core.Index.Syntax.DeclLevel.Functions as Decl
import qualified CGIR.RustCG.Core.Index.Syntax.DeclLevel.Enums     as Decl
-- *



index :: IO (Either Text I.Program) -> IO (Either Text I.Program)
index upstream = do
    result <- upstream
    
    case result of
        Left err      -> return $ Left err
        Right payload ->
            return
                $ Right
                $ index' payload



index' :: I.Program -> I.Program
index' payload =
    let fns = globalize (I.getFunctions payload)
        ens = map normEnum $ I.getEnums payload
    in
    
        I.Program
            { I.enums = ens
            , I.functions = fns
            }




-- *
-- | Indexers
-- *

globalize :: [Decl.Function] -> [Decl.Function]
globalize decls =
    fst $ Sys.runState (Decl.traverseDecls decls) 0 Map.empty

normEnum :: Decl.Enum -> Decl.Enum
normEnum enum =
    fst $ Sys.runState (Decl.indexEnum enum) 0 Map.empty





