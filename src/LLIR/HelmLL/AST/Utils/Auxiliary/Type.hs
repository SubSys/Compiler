{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module LLIR.HelmLL.AST.Utils.Auxiliary.Type (
    getReturnType
  , getInputTypes
  , flatten
) where


-- *
import Core
import Core.Control.Flow ((|>), (<|))
import Core.List.Util    (singleton)
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
import qualified Data.Data                    as Data
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP




-- + HelmLL Module Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as T
import qualified LLIR.HelmLL.AST.Data.Base.Literals   as V

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as Decl

-- + Local
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident   as ID
-- *





getReturnType :: T.Type -> T.Type
getReturnType (T.Arr t1 t2) =
    getReturnType t2

getReturnType x = x





getInputTypes :: T.Type -> [T.Type]
getInputTypes (flatten -> ts)
    | List.length ts >= 2 =
        List.init ts
    
    | otherwise =
        []



flatten :: T.Type -> [T.Type]
flatten (T.Arr t1 t2) =
    t1 : flatten t2

flatten x = [x]





