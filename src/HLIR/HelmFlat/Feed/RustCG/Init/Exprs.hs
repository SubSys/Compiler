{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmFlat.Feed.RustCG.Init.Exprs (
    deFunBaseValues
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
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc           as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident         as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types         as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values        as V
-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P
-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl
-- *



deFunBaseValues :: [Decl.Function] -> [Decl.Function]
deFunBaseValues decls =
    let
        tyEnv = TyEnv.genTypesEnv decls
        fnEnv = genFunEnv decls
    in
        decls
              |> Uni.transformBi (deFunBaseValues' fnEnv tyEnv)
              |> Uni.transformBi (deFunParamRefs fnEnv tyEnv)


deFunBaseValues' :: [ID.Ident] -> Map.Map ID.Ident T.Type -> E.Expr -> E.Expr
deFunBaseValues' fnEnv tyEnv expr@(E.FunCall ident [])
    | Nothing <- Map.lookup ident tyEnv =
        error $ Text.unpack $ Text.unlines
            [ Text.pack "Unknown base-type: "
            , Text.pack $ PP.prettyShow ident
            ]

deFunBaseValues' fnEnv tyEnv expr@(E.FunCall ident [])
    | True <- TyEnv.isBaseType ident tyEnv
    , False <- ident `List.elem` fnEnv =
        E.Ref ident
    | otherwise =
        expr

deFunBaseValues' _ _ x = x



deFunParamRefs fnEnv tyEnv (E.FunCall ident args)
    | Just ty <- Map.lookup ident tyEnv =
        let inputTypes = Type.getInputTypes ty
        in 
            E.FunCall ident (checkArgs inputTypes args)

deFunParamRefs _ _ x = x

checkArgs :: [T.Type] -> [E.Expr] -> [E.Expr]
checkArgs = List.zipWith f 
    where
        f :: T.Type -> E.Expr -> E.Expr
        f t@T.Arr{} (E.FunCall ident []) =
            E.Ref ident
        
        f _ e = e




-- | Setup Stuff
--

-- | Env for solving "Is this a function reference, or call?"
--
genFunEnv :: (Data.Data a, Data.Typeable a) => a -> [ID.Ident]
genFunEnv input =
    [ name | (Decl.Function (Etc.Binder name _) _ _ _) <- Uni.universeBi input ]





