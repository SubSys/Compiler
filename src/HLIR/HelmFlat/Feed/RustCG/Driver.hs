{-# LANGUAGE NoImplicitPrelude #-}
module HLIR.HelmFlat.Feed.RustCG.Driver (
    toRustCG
  , toRustCG'
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

-- + HelmFlat AST Interface
import qualified HLIR.HelmFlat.Data.Interface as HelmFlat
import qualified HLIR.HelmFlat.Data.Interface as I
-- + RustCG AST Interface
import qualified CGIR.Rust.Data.Interface   as RustCG

-- + Local
import qualified HLIR.HelmFlat.Feed.RustCG.Syntax        as Syntax
import qualified HLIR.HelmFlat.Feed.RustCG.Init.Variants as Init

import qualified HLIR.HelmFlat.Feed.RustCG.Post.Finalize as Finalize
-- *


toRustCG :: IO (Either Text HelmFlat.Program) -> IO (Either Text RustCG.Program)
toRustCG upstream = do
    result <- upstream
    
    case result of
        Left err -> return $ Left err
        Right payload ->
            return
                $ Right
                $ Finalize.setFunRefs (TyEnv.genTypesEnv (I.getFunctions payload)) (toRustCG' payload)




toRustCG' :: HelmFlat.Program -> RustCG.Program
toRustCG' payload =
    let
        fns = HelmFlat.getFunctions payload
            |> Init.setVariantPaths (I.getUnions payload)
            |> map Syntax.dropFunction
        uns = HelmFlat.getUnions payload
            |> Init.setVariantPaths (I.getUnions payload)
            |> map Syntax.dropUnion
    in
        RustCG.Program
            { RustCG.enums = uns
            , RustCG.functions = fns
            }
    







