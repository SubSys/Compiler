{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module HLIR.HelmFlat.AST.Utils.Auxiliary.Special.Superposed (
    splitSuperposedDecls
  , isSuperposed
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
import qualified Data.Functor.Foldable as F

-- + Dev & Debugging
import qualified Text.Show.Prettyprint as PP

-- + HelmFlat Module Interface
import qualified HLIR.HelmFlat.Module.Data.Interface as I

-- + HelmFlat AST
-- ++ Base
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Etc      as Etc
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Ident    as ID
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Types    as T
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Values   as V
import qualified HLIR.HelmFlat.AST.Data.Semantic.Base.Metadata as Meta

-- ++ TermLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Expr     as E
import qualified HLIR.HelmFlat.AST.Data.Semantic.TermLevel.Patterns as P

-- ++ TopLevel
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Fixities  as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Functions as Decl
import qualified HLIR.HelmFlat.AST.Data.Semantic.TopLevel.Unions    as Decl

-- + Local
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Type  as T
import qualified HLIR.HelmFlat.AST.Utils.Auxiliary.Ident as ID
-- *


type Normal = Decl.Function
type Superposed = Decl.Function



splitSuperposedDecls :: [Decl.Function] -> (Map.Map ID.Ident Superposed, [Normal])
splitSuperposedDecls xs =
    let
        (normals, supers) = Either.partitionEithers $ map splitSuperposedDecl xs
    in
        (Map.fromList supers, normals)



splitSuperposedDecl :: Decl.Function -> Either Normal (ID.Ident, Superposed)
splitSuperposedDecl decl@(Decl.Function name args expr (isSuperposed -> Just scheme) meta) =
    Right (ID.get name, decl)


splitSuperposedDecl decl = Left decl






isSuperposed :: Decl.Signature -> Maybe T.Scheme
isSuperposed (Decl.Validated scheme@(T.Forall _ ty) meta)
    | (t:ts) <- T.flatten ty
    , T.Superposed{} <- t =
        Just scheme

isSuperposed _ = Nothing


isSuperposed' :: T.Type -> Bool
isSuperposed' ty@(T.flatten -> ts) =
        List.any isSuper ts

-- isSuperposed' _ = False


isSuper :: T.Type -> Bool
isSuper T.Superposed{} = True
isSuper _ = False




