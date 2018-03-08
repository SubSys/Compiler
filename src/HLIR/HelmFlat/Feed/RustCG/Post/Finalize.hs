{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmFlat.Feed.RustCG.Post.Finalize (
    setFunRefs
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


-- + HelmFlat AST Utils
import qualified HLIR.HelmFlat.AST.Utils.Types                    as Type
import qualified HLIR.HelmFlat.AST.Utils.Generic.SudoFFI          as SudoFFI
import qualified HLIR.HelmFlat.AST.Utils.Generic.TypesEnv         as TyEnv
import qualified HLIR.HelmFlat.AST.Utils.Generic.TypesEnv.Helpers as TyEnv

-- + RustCG AST
-- ++ Base
import qualified CGIR.Rust.AST.Data.Base.Ident                 as ID
import qualified CGIR.Rust.AST.Data.Base.Literals              as Lit
import qualified CGIR.Rust.AST.Data.Base.Types                 as T
import qualified CGIR.Rust.AST.Data.Base.Etc                   as Etc
-- ++ Block Level
import qualified CGIR.Rust.AST.Data.TermLevel.Stmt            as S
import qualified CGIR.Rust.AST.Data.TermLevel.Patterns        as P
-- ++ Decl/Top Level
import qualified CGIR.Rust.AST.Data.TopLevel.Enums.Variants   as Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Enums            as Decl
import qualified CGIR.Rust.AST.Data.TopLevel.Functions        as Decl

-- + Local
import qualified HLIR.HelmFlat.Feed.RustCG.Syntax as Syntax
-- *




-- | 
-- Essentially, if a value is referencing a function, we need to update the ref value with an `&` prefix.
--
setFunRefs env = Uni.transformBi (setFunRefs' (convertTypesEnv env))

setFunRefs' :: Map.Map ID.Ident T.Type -> S.Stmt -> S.Stmt
setFunRefs' env (S.FunCall path args) =
    S.FunCall path (map (checkArg env) args)

setFunRefs' env x = x


checkArg :: Map.Map ID.Ident T.Type -> S.Stmt -> S.Stmt
checkArg env (S.Ref path) =
    S.Ref (checkPath env path)

checkArg _ s = s


checkPath :: Map.Map ID.Ident T.Type -> ID.Path -> ID.Path
checkPath env (ID.Path [ID.Seg Nothing txt])
    | Just T.Fn{} <- Map.lookup (ID.Ident txt) env =
        ID.Path [ID.Seg (Just ID.Ref) txt]

checkPath env (ID.Path segs)
    | (ID.Seg Nothing txt) <- ref
    , Just T.Fn{} <- Map.lookup (ID.Ident txt) env =
        let ref' = ID.Seg (Just ID.Ref) txt
        in
            ID.Path (ns ++ [ref'])
    where
        ref = List.last segs
        ns  = List.init segs


checkPath env p = p


-- | Internal Helpers
--

convertTypesEnv env = Map.fromList $ map convert $ Map.toList env
    where
        convert (ident, ty) =
            ( Syntax.dropIdent ident
            , Syntax.dropType ty
            )






