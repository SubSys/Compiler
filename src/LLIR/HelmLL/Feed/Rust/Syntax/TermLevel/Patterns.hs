{-# LANGUAGE NoImplicitPrelude #-}
module LLIR.HelmLL.Feed.Rust.Syntax.TermLevel.Patterns (
    dropCaseAlt
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





-- + HelmLL AST Utils
import qualified LLIR.HelmLL.AST.Utils.Generic.Scope       as Scope
import qualified LLIR.HelmLL.AST.Utils.Class.Ident         as ID
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Type      as Ty
import qualified LLIR.HelmLL.AST.Utils.Auxiliary.Functions as Fn
import qualified LLIR.HelmLL.AST.Utils.Generic.SudoFFI     as SudoFFI

-- + HelmLL AST
-- ++ Base
import qualified LLIR.HelmLL.AST.Data.Base.Etc           as H.Etc
import qualified LLIR.HelmLL.AST.Data.Base.Ident         as H.ID
import qualified LLIR.HelmLL.AST.Data.Base.Types         as H.T
import qualified LLIR.HelmLL.AST.Data.Base.Literals      as H.Lit

-- ++ TermLevel
import qualified LLIR.HelmLL.AST.Data.TermLevel.Stmt     as H.S
import qualified LLIR.HelmLL.AST.Data.TermLevel.Patterns as H.P

-- ++ TopLevel
import qualified LLIR.HelmLL.AST.Data.TopLevel.Functions as H.Decl
import qualified LLIR.HelmLL.AST.Data.TopLevel.Unions    as H.Decl

-- + RustCG AST
-- ++ Base
import qualified CGIR.Rust.AST.Data.Base.Ident                 as R.ID
import qualified CGIR.Rust.AST.Data.Base.Literals              as R.Lit
import qualified CGIR.Rust.AST.Data.Base.Types                 as R.T
import qualified CGIR.Rust.AST.Data.Base.Etc                   as R.Etc
-- ++ TermLevel
import qualified CGIR.Rust.AST.Data.TermLevel.Stmt             as R.S
import qualified CGIR.Rust.AST.Data.TermLevel.Patterns         as R.P
-- ++ TopLevel
import qualified CGIR.Rust.AST.Data.TopLevel.Enums.Variants   as R.Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Enums            as R.Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Functions        as R.Decl

-- + Local
import qualified LLIR.HelmLL.Feed.Rust.Syntax.Base.Ident    as ID
import qualified LLIR.HelmLL.Feed.Rust.Syntax.Base.Types    as T
import qualified LLIR.HelmLL.Feed.Rust.Syntax.Base.Literals as Lit
import qualified LLIR.HelmLL.Feed.Rust.Syntax.Base.Etc      as Etc
-- *



dropCaseAlt :: (H.S.Block -> R.S.Block) -> H.P.CaseAlt -> R.P.Arm
dropCaseAlt f (H.P.CaseAlt patrn block) =
    R.P.Arm
        (dropPattern patrn)
        (f block)




dropPattern :: H.P.Pattern -> R.P.Pattern
dropPattern (H.P.Lit lit) =
    R.P.Lit
        (Lit.dropLiteral lit)

dropPattern (H.P.List xs) =
    R.P.List
        (map dropPattern xs)

dropPattern (H.P.ListCons xs rest) =
    R.P.ListCons
        (map dropPattern xs)
        (Core.applyMaybe dropPattern rest)

dropPattern (H.P.Tuple items) =
    R.P.Tuple
        (map dropPattern items)

dropPattern (H.P.Constr ident args) =
    R.P.Variant
        (ID.toPath ident)
        (map dropPattern args)

dropPattern (H.P.Var ident) =
    R.P.Var
        (ID.binder2Ident ident)

dropPattern H.P.Wildcard =
    R.P.Wildcard



