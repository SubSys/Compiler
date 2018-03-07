{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmFlat.Feed.GLSL.Syntax.Base.Types (
    dropType
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
import qualified Data.String                  as String

-- + Recursion Schemes & Related
import qualified Data.Functor.Foldable       as F
import qualified Data.Generics.Uniplate.Data as Uni

-- + OS APIS & Related
import qualified System.IO as SIO

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP


-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Types                    as Type
import qualified HLIR.HelmFlat.AST.Utils.Generic.SudoFFI          as SudoFFI
import qualified HLIR.HelmFlat.AST.Utils.Generic.TypesEnv         as TyEnv
import qualified HLIR.HelmFlat.AST.Utils.Generic.TypesEnv.Helpers as TyEnv

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc           as H.Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident         as H.ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types         as H.T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values        as H.V
-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as H.E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as H.P
-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as H.Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as H.Decl

-- + GLSL AST
-- ++ Base
import qualified CGIR.GLSL.AST.Data.Base.Ident                 as S.ID
import qualified CGIR.GLSL.AST.Data.Base.Literals              as S.Lit
import qualified CGIR.GLSL.AST.Data.Base.Types                 as S.T
import qualified CGIR.GLSL.AST.Data.Base.Etc                   as S.Etc
-- ++ Block Level
import qualified CGIR.GLSL.AST.Data.TermLevel.Stmt            as S.S
-- ++ Decl/Top Level
import qualified CGIR.GLSL.AST.Data.TopLevel.Functions         as S.Decl
import qualified CGIR.GLSL.AST.Data.TopLevel.Globals           as S.Decl

-- + Local
import qualified HLIR.HelmFlat.Feed.GLSL.Utils.Error as Error

import qualified HLIR.HelmFlat.Feed.GLSL.Syntax.Base.Ident as ID
-- *



-- | Drop Types
--
dropType :: H.T.Type -> S.T.Type
dropType (builtin -> Just ty) = ty

dropType H.T.Int    = S.T.Int
dropType H.T.Float  = S.T.Float
dropType H.T.Bool   = S.T.Bool

dropType (H.T.List ty)         = error "Not yet implemented"
dropType (H.T.Tuple ts)        = error "Not yet implemented"
dropType (H.T.Union name args) = error "Not yet implemented"

dropType H.T.String    = Error.unsupported "String"
dropType H.T.Char      = Error.unsupported "Char"
dropType (H.T.Var id') = Error.unsupported "Type variables. Internal error, should have been removed by now!"
dropType ty@H.T.Arr{}  = Error.unsupported "Fn Types. Internal error, should have been removed by now!"



-- TODO: ...
builtin :: H.T.Type -> Maybe S.T.Type

-- Tuples to Vectors
builtin (H.T.Tuple [H.T.Float, H.T.Float])                       = Just S.T.Vec2
builtin (H.T.Tuple [H.T.Float, H.T.Float, H.T.Float])            = Just S.T.Vec3
builtin (H.T.Tuple [H.T.Float, H.T.Float, H.T.Float, H.T.Float]) = Just S.T.Vec4

-- ?
builtin (H.T.Union (H.ID.Ident' "Void") []) = Just S.T.Void

builtin _ = Nothing




