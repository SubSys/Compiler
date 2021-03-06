{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.HelmLL.Feed.GLSL.Syntax.Base.Etc (
    dropBinder
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
import qualified Data.Data                    as Data
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP




-- + HelmLL AST Interface
import qualified LLIR.HelmLL.Data.Interface as I

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc      as LL.Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident    as LL.ID
import qualified LLIR.HelmLL.AST.Data.Base.Types    as LL.T
import qualified LLIR.HelmLL.AST.Data.Base.Literals as LL.Lit

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as LL.E
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as LL.P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as LL.Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as LL.Decl

-- + GLSL AST
-- ++ Base
import qualified CGIR.GLSL.AST.Data.Base.Ident                 as CG.ID
import qualified CGIR.GLSL.AST.Data.Base.Literals              as CG.Lit
import qualified CGIR.GLSL.AST.Data.Base.Types                 as CG.T
import qualified CGIR.GLSL.AST.Data.Base.Etc                   as CG.Etc
-- ++ Block Level
import qualified CGIR.GLSL.AST.Data.TermLevel.Stmt             as CG.S
-- ++ Decl/Top Level
import qualified CGIR.GLSL.AST.Data.TopLevel.Functions         as CG.Decl
import qualified CGIR.GLSL.AST.Data.TopLevel.Globals           as CG.Decl


-- + Local
import qualified LLIR.HelmLL.Feed.GLSL.Utils.Error       as Error
import qualified LLIR.HelmLL.Feed.GLSL.Syntax.Base.Ident as ID
import qualified LLIR.HelmLL.Feed.GLSL.Syntax.Base.Types as T
-- *



dropBinder :: LL.Etc.Binder -> CG.Etc.Input
dropBinder (LL.Etc.Binder ident (Just ty)) =
    CG.Etc.Input
        Nothing
        (T.dropType ty)
        (ID.dropIdent ident)



